
# 05_shiny_dashboard.R

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load forecast and optimisation data
NGO_Data_Expanded <- read_csv("NGO_Data_Expanded.csv")

# Aggregate forecast and cost data
summary_df <- NGO_Data_Expanded %>%
  group_by(Aid_Type, Area, Month) %>%
  summarise(
    Forecasted_Demand = sum(Demand_Predicted),
    Actual_Distribution = sum(Quantity_Distributed),
    Avg_Transport_Cost = mean(Transportation_Cost),
    Avg_Storage_Cost = mean(Storage_Cost),
    Storage_Capacity = mean(Storage_Capacity),
    .groups = "drop"
  ) %>%
  mutate(Unit_Cost = Avg_Transport_Cost + Avg_Storage_Cost)

# UI
ui <- fluidPage(
  titlePanel("NGO Resource Planning Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("aid_type", "Select Aid Type:", choices = unique(summary_df$Aid_Type)),
      selectInput("area", "Select Region:", choices = unique(summary_df$Area)),
      sliderInput("budget", "Budget Limit (£):", min = 10000, max = 300000, value = 250000, step = 10000)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast Overview", plotOutput("forecastPlot")),
        tabPanel("Allocation View", plotOutput("allocationPlot")),
        tabPanel("Cost Overview", plotOutput("costPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  filtered_data <- reactive({
    summary_df %>%
      filter(Aid_Type == input$aid_type, Area == input$area)
  })

  output$forecastPlot <- renderPlot({
    plot_data <- filtered_data()
    ggplot(plot_data, aes(x = Month)) +
      geom_line(aes(y = Forecasted_Demand, color = "Forecasted")) +
      geom_line(aes(y = Actual_Distribution, color = "Actual")) +
      labs(title = paste("Forecasted vs Actual -", input$aid_type, "in", input$area),
           x = "Month", y = "Units") +
      scale_color_manual(values = c("Forecasted" = "blue", "Actual" = "red")) +
      theme_minimal()
  })

  output$allocationPlot <- renderPlot({
    alloc_data <- summary_df %>%
      filter(Aid_Type == input$aid_type) %>%
      group_by(Area) %>%
      summarise(
        Forecasted = sum(Forecasted_Demand),
        Actual = sum(Actual_Distribution),
        .groups = "drop"
      )

    ggplot(alloc_data, aes(x = Area)) +
      geom_bar(aes(y = Forecasted, fill = "Forecasted"), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Actual, fill = "Actual"), stat = "identity", position = "dodge") +
      labs(title = paste("Allocation by Region -", input$aid_type),
           y = "Units", x = "Region") +
      scale_fill_manual(values = c("Forecasted" = "skyblue", "Actual" = "darkgreen")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$costPlot <- renderPlot({
    cost_data <- summary_df %>%
      filter(Aid_Type == input$aid_type) %>%
      group_by(Area) %>%
      summarise(
        Total_Cost = sum((Forecasted_Demand + Actual_Distribution) * Unit_Cost),
        .groups = "drop"
      )

    ggplot(cost_data, aes(x = Area, y = Total_Cost, fill = Area)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Total Cost by Region -", input$aid_type),
           x = "Region", y = "Total Cost (£)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)


# 04d_optimisation_lp_milp.R

# Load required packages
library(lpSolve)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)
library(ggplot2)
library(readr)

# Load data
NGO_Data_Expanded <- read_csv("NGO_Data_Expanded.csv")

# Aggregate by Aid Type and Area
summary_df <- NGO_Data_Expanded %>%
  group_by(Aid_Type, Area) %>%
  summarise(
    Forecasted_Demand = sum(Demand_Predicted),
    Storage_Capacity = sum(Storage_Capacity),
    Avg_Transport_Cost = mean(Transportation_Cost),
    Avg_Storage_Cost = mean(Storage_Cost),
    Unit_Cost = mean(Transportation_Cost) + mean(Storage_Cost),
    .groups = "drop"
  )

# Filter for one aid type for simplicity (can be expanded)
aid_data <- summary_df %>% filter(Aid_Type == "Food")
n <- nrow(aid_data)

# Set budget
total_budget <- 250000

# LINEAR PROGRAMMING (LP)

costs <- aid_data$Unit_Cost
demand <- aid_data$Forecasted_Demand
storage <- aid_data$Storage_Capacity

constraints <- rbind(
  diag(n),                  # Allocation ≤ demand
  diag(n),                  # Allocation ≤ storage
  matrix(1, nrow = 1, ncol = n)  # Total cost constraint
)

rhs <- c(demand, storage, total_budget / costs)
dir <- rep("<=", length(rhs))

lp_result <- lp("min", costs, constraints, dir, rhs)

aid_data$LP_Allocated <- round(lp_result$solution, 1)


# MIXED INTEGER LINEAR PROGRAMMING (MILP)
milp_model <- MIPModel() %>%
  add_variable(x[i], i = 1:n, type = "continuous", lb = 0) %>%
  add_variable(y[i], i = 1:n, type = "binary") %>%
  set_objective(sum_expr(aid_data$Unit_Cost[i] * x[i], i = 1:n), "min") %>%
  add_constraint(x[i] <= demand[i] * y[i], i = 1:n) %>%
  add_constraint(x[i] <= storage[i] * y[i], i = 1:n) %>%
  add_constraint(sum_expr(aid_data$Unit_Cost[i] * x[i], i = 1:n) <= total_budget)

milp_result <- solve_model(milp_model, with_ROI(solver = "glpk"))

aid_data$MILP_Allocated <- get_solution(milp_result, x[i])$value
aid_data$Aid_Sent <- ifelse(aid_data$MILP_Allocated > 0, "Yes", "No")


# VISUALISATION: Forecasted vs Allocated (LP)
ggplot(aid_data, aes(x = Area)) +
  geom_bar(aes(y = Forecasted_Demand, fill = "Forecasted"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = LP_Allocated, fill = "Allocated (LP)"), stat = "identity", position = "dodge") +
  labs(title = "LP Model: Forecasted vs Allocated Demand",
       y = "Units", x = "Area") +
  scale_fill_manual(values = c("Forecasted" = "skyblue", "Allocated (LP)" = "darkblue")) +
  theme_minimal()


# VISUALISATION: Forecasted vs Allocated (MILP)
ggplot(aid_data, aes(x = Area)) +
  geom_bar(aes(y = Forecasted_Demand, fill = "Forecasted"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = MILP_Allocated, fill = "Allocated (MILP)"), stat = "identity", position = "dodge") +
  labs(title = "MILP Model: Forecasted vs Allocated Demand",
       y = "Units", x = "Area") +
  scale_fill_manual(values = c("Forecasted" = "skyblue", "Allocated (MILP)" = "darkgreen")) +
  theme_minimal()


# VISUALISATION: Total Cost by Region
aid_data$LP_Cost <- round(aid_data$LP_Allocated * aid_data$Unit_Cost, 2)

ggplot(aid_data, aes(x = Area, y = LP_Cost, fill = Area)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Cost by Region (LP Model)",
       x = "Region", y = "Total Cost (£)") +
  theme_minimal()

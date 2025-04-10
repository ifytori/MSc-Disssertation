# forecast_and_performance.R

library(prophet)
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

# Load the dataset
NGO_Data_Expanded <- read_csv("NGO_Data_Expanded.csv")

# Forecasting function using monthly data
forecast_and_evaluate <- function(area, aid_type, forecast_months = 3) {
  cat("\nProcessing:", aid_type, "in", area, "\n")
  
  # Prepare data (monthly)
  df_filtered <- NGO_Data_Expanded %>%
    filter(Area == area, Aid_Type == aid_type) %>%
    group_by(Month) %>%
    summarise(y = sum(Demand_Predicted), .groups = 'drop') %>%
    mutate(ds = as.Date(paste0(Month, "-01"), "%Y-%m-%d")) %>%
    select(ds, y) %>%
    arrange(ds)
  
  # Minimum data check
  if (nrow(df_filtered) < 6) {
    cat("Not enough data for:", aid_type, "in", area, "\n")
    return(NULL)
  }
  
  # Outlier handling (IQR)
  q1 <- quantile(df_filtered$y, 0.25)
  q3 <- quantile(df_filtered$y, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  df_filtered$y <- pmin(pmax(df_filtered$y, lower), upper)
  
  # Optional smoothing
  df_filtered$y <- rollmean(df_filtered$y, k = 2, fill = NA)
  df_filtered <- na.omit(df_filtered)
  
  # Check again
  if (nrow(df_filtered) < 6) {
    cat("Not enough data after cleaning for:", aid_type, "in", area, "\n")
    return(NULL)
  }
  
  # Train/test split (last 3 months for test)
  split_date <- max(df_filtered$ds) %m-% months(forecast_months)
  train_df <- df_filtered %>% filter(ds < split_date)
  test_df <- df_filtered %>% filter(ds >= split_date)
  
  if (nrow(train_df) < 6 || nrow(test_df) < 1) {
    cat("Not enough train/test data for:", aid_type, "in", area, "\n")
    return(NULL)
  }
  
  # Fit Prophet
  model <- prophet(
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    changepoint.range = 0.5,
    seasonality.mode = 'additive',
    n.changepoints = min(5, floor(nrow(train_df) / 2))
  )
  
  model <- fit.prophet(model, train_df)
  future <- make_future_dataframe(model, periods = forecast_months, freq = "month")
  forecast <- predict(model, future)
  
  # Join forecast with test
  forecast_subset <- forecast %>% select(ds, yhat)
  merged_df <- inner_join(test_df, forecast_subset, by = "ds") %>%
    mutate(
      error = y - yhat,
      abs_error = abs(error),
      squared_error = error^2,
    )
  
  # Metrics
  mae <- mean(merged_df$abs_error)
  rmse <- sqrt(mean(merged_df$squared_error))
  mean_actual <- mean(merged_df$y)
  mae_pct <- (mae / mean_actual) * 100
  rmse_pct <- (rmse / mean_actual) * 100
  
  cat("MAE:", round(mae, 2), "(", round(mae_pct, 2), "%)\n")
  cat("RMSE:", round(rmse, 2), "(", round(rmse_pct, 2), "%)\n")
  
  # Plot
  p <- ggplot(merged_df, aes(x = ds)) +
    geom_line(aes(y = y, color = "Actual"), size = 1.2) +
    geom_line(aes(y = yhat, color = "Forecast"), size = 1.2, linetype = "dashed") +
    labs(title = paste("Forecast vs Actual -", aid_type, "in", area, "(2024)"),
         x = "Month", y = "Demand") +
    scale_color_manual(values = c("Actual" = "red", "Forecast" = "blue")) +
    theme_minimal()
  
  print(p)
}

# Run forecasts
forecast_and_evaluate("East Park", "Food")
forecast_and_evaluate("Penn", "Clothing")
forecast_and_evaluate("Bilston", "Hygiene Products")
forecast_and_evaluate("Bushbury", "Baby Supplies")


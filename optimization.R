# spending_analysis.R

# Load Libraries ---------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(forecast)
library(purrr)
library(janitor)

today <- as.Date('2025-05-31')#lubridate::today()

# Load Data ---------------------------
spending <- read_excel("Data/Joint Spending Plan 2025.xlsx", sheet = "raw spending") |>
  mutate(month = floor_date(Date, "month")) |> 
  clean_names()

month_log <- read_excel("Data/Joint Spending Plan 2025.xlsx", sheet = "month log") |>
  mutate(month = floor_date(Month, "month")) |> 
  clean_names()

# Validate Data ---------------------------
data_issues <- spending |>
  filter(
    amount == 0 |
      amount > 10000 |
      is.na(date) |
      duplicated(across(c(date, description, amount)))
  )

# Category Completion Check ---------------------------
expected_categories <- c('Amazon',
                         'Books',
                         'Car Loans',
                         'Car Service',
                         'Cell Phones',
                         'Clothes',
                         'Coffee',
                         'Costco',
                         'Drinks',
                         'Eating Out',
                         'Fun',
                         'Gasoline',
                         'Gifts',
                         'Groceries',
                         'Gym',
                         'Hair',
                         'Home Improvement',
                         'Insurance',
                         'Medical',
                         'Mortgage',
                         'News',
                         'Pay',
                         'Pets',
                         'Spotify',
                         'Student Loans',
                         'Tickets',
                         'TV',
                         'Utilities',
                         'Vacation',
                         'Wifi',
                         'Xbox',
                         'Other')

# Get all actual (month, category) combinations
actual_month_category <- spending |>
  filter(type %in% expected_categories) |>
  distinct(month, type)

# Build all expected combinations
all_combos <- expand_grid(
  month = unique(spending$month),
  type = expected_categories
)

# Find missing category-month combos
missing_categories <- anti_join(all_combos, actual_month_category, by = c("month", "type")) |> 
  filter(month == floor_date(today,unit='month'))

print(missing_categories)

# Anomaly Detection ---------------------------
anomalies <- spending |>
  group_by(category) |>
  mutate(
    q1 = quantile(amount, 0.25, na.rm = TRUE),
    q3 = quantile(amount, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    is_anomaly = amount < (q1 - 1.5 * iqr) | amount > (q3 + 1.5 * iqr)
  ) |>
  filter(is_anomaly)

# Forecasting ---------------------------
balances_ts <- month_log |>
  pivot_longer(-month, names_to = "account", values_to = "balance") |>
  group_by(account) |>
  summarise(
    ts_data = list(ts(balance, frequency = 12)),
    .groups = "drop"
  )

forecasts <- balances_ts |>
  mutate(
    forecast_model = map(ts_data, auto.arima),
    forecast_result = map(forecast_model, ~ forecast(.x, h = 3))
  )

# Optimization Suggestions ---------------------------
spending_summary <- spending |>
  group_by(category, month) |>
  summarise(total = sum(amount), .groups = "drop") |>
  group_by(category) |>
  mutate(monthly_change = total - lag(total)) |>
  filter(monthly_change > 100)

# Export Summary Data ---------------------------
write.csv(data_issues, "outputs/data_issues.csv", row.names = FALSE)
write.csv(anomalies, "outputs/anomalies.csv", row.names = FALSE)
write.csv(missing_categories, "outputs/missing_categories.csv", row.names = FALSE)
write.csv(spending_summary, "outputs/opportunities.csv", row.names = FALSE)

# Save Forecasts ---------------------------
saveRDS(forecasts, "outputs/balance_forecasts.rds")

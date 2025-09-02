library(tidyverse)
library(dplyr)

setwd("/Users/ryangerda/Development/Finances")
raw <- readxl::read_excel(
  "Data/Joint Spending Plan 2025.xlsx",
  sheet = 'raw spending'
) %>%
  janitor::clean_names()
log <- readxl::read_excel(
  "Data/Joint Spending Plan 2025.xlsx",
  sheet = 'month log'
) %>%
  janitor::clean_names()

today <- as.Date('2025-08-31') #lubridate::today()
first_day_this_month <- lubridate::floor_date(today, unit = 'month')
last_day_this_month <- lubridate::ceiling_date(today, unit = 'month') - 1
first_day_this_year <- lubridate::floor_date(today, unit = 'year')
last_day_this_year <- lubridate::ceiling_date(today, unit = 'year') - 1
last_day_last_month <- first_day_this_month - 1
first_day_last_month <- lubridate::floor_date(
  last_day_last_month,
  unit = 'month'
)
first_day_next_month <- last_day_this_month + 1
last_day_next_month <- lubridate::ceiling_date(
  first_day_next_month,
  unit = 'month'
) -
  1

missing_bills <- 0

# Joint Income
income <- raw %>%
  dplyr::filter(in_out == 'In') %>%
  dplyr::filter(name == 'Joint') %>%
  group_by(lubridate::year(date), lubridate::month(date), name) %>%
  reframe(amount = sum(amount, na.rm = T))

mean(income$amount, na.rm = T)

# joint Spending
outcome <- raw %>%
  dplyr::filter(in_out == 'Out') %>%
  dplyr::filter(name == 'Joint') %>%
  dplyr::filter(primary_category == 'Checking') %>%
  mutate(
    Year = lubridate::year(date),
    Month = lubridate::month(date, label = TRUE),
    date = lubridate::floor_date(date, unit = 'month')
  ) |>
  dplyr::filter(date >= today - 93) %>%
  group_by(date, type) %>%
  reframe(amount = sum(amount, na.rm = T)) %>%
  arrange(type, date) |>
  pivot_wider(names_from = 'date', values_from = 'amount')

print(outcome, n = 'all')
# Get numeric column sums
numeric_sums <- sapply(
  outcome[, sapply(outcome, is.numeric), drop = FALSE],
  sum,
  na.rm = TRUE
)

# Convert to long-format data frame with 'date' as the name column
summary_df <- data.frame(
  date = names(numeric_sums),
  total = as.numeric(numeric_sums),
  row.names = NULL
) |>
  arrange(date)

print(summary_df)


three_months_ago <- last_day_this_month %m-% months(3)
# ryan Spending
ryan_spend <- raw %>%
  filter(date > three_months_ago) |>
  dplyr::filter(in_out == 'Out') %>%
  dplyr::filter(name == 'Ryan') %>%
  dplyr::filter(primary_category == 'Checking') %>%
  mutate(
    Year = lubridate::year(date),
    Month = lubridate::month(date, label = TRUE),
    date = lubridate::floor_date(date, unit = 'month')
  ) |>
  group_by(date, type) %>%
  reframe(amount = sum(amount, na.rm = T)) %>%
  arrange(type, date) |>
  pivot_wider(names_from = 'date', values_from = 'amount')

# Get numeric column sums
numeric_sums <- sapply(
  ryan_spend[, sapply(ryan_spend, is.numeric), drop = FALSE],
  sum,
  na.rm = TRUE
)

# Convert to long-format data frame with 'date' as the name column
summary_df <- data.frame(
  date = names(numeric_sums),
  total = as.numeric(numeric_sums),
  row.names = NULL
) |>
  arrange(date)

print(summary_df)

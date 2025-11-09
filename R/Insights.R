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

today <- as.Date('2025-10-31') #lubridate::today()
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

# MONTH IN VS. OUT
raw %>%
  dplyr::filter(name == 'Joint') %>%
  dplyr::filter(primary_category == 'Checking', type != "Nest Egg") %>%
  mutate(
    Year = lubridate::year(date),
    Month = lubridate::month(date, label = TRUE),
    date = lubridate::floor_date(date, unit = 'month')
  ) |>
  dplyr::filter(date >= '2025-01-01') |>
  group_by(date, in_out) %>%
  reframe(amount = sum(amount, na.rm = T)) %>%
  arrange(in_out, date) |>
  pivot_wider(names_from = 'in_out', values_from = 'amount')

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
  dplyr::filter(date >= '2025-01-01') %>%
  group_by(date, type) %>%
  reframe(amount = sum(amount, na.rm = T)) %>%
  arrange(type, date) |>
  pivot_wider(names_from = 'date', values_from = 'amount') |>
  mutate(
    want_need = ifelse(
      type %in%
        c(
          'Amazon',
          'Books',
          'Coffee',
          'Drinks',
          'Eating Out',
          'Fun',
          'Gifts',
          'Gym',
          'Home Improvement',
          'Other',
          'Spotify',
          'TV',
          'Vacation'
        ),
      'Wants',
      ifelse(type %in% c('Brokerage', 'Baby', 'Nest Egg'), 'Savings', 'Needs')
    )
  )

print(outcome, n = 'all')
# Get numeric column sums
numeric_sums <- sapply(
  outcome[, sapply(outcome, is.numeric), drop = FALSE],
  sum,
  na.rm = TRUE
)
print(numeric_sums)
# Convert to long-format data frame with 'date' as the name column
summary_df <- data.frame(
  date = names(numeric_sums),
  total = as.numeric(numeric_sums),
  row.names = NULL
) |>
  arrange(date)

print(summary_df)

long_df <- outcome %>%
  pivot_longer(
    cols = -c(type, want_need), # all the date columns
    names_to = "date",
    values_to = "amount",
    values_drop_na = TRUE
  ) %>%
  mutate(date = as.Date(date)) # turn names into Date

# sum by want_need for each date
totals_by_need <- long_df %>%
  group_by(date, want_need) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
  group_by(date) %>%
  mutate(
    month_total = sum(total, na.rm = TRUE),
    pct = total / month_total * 100
  ) %>%
  ungroup()

totals_by_need

ggplot(
  totals_by_need,
  aes(x = date, y = pct, color = want_need, group = want_need)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Monthly Spend Share",
    x = "Month",
    y = "Percentage of Monthly Total",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b'%y", # try "%b %Y" if you prefer "Jan 2025"
    expand = expansion(mult = c(0.01, 0.03))
  )

# Ryans Income
ryan_income <- raw %>%
  dplyr::filter(in_out == 'In') %>%
  dplyr::filter(name == 'Ryan') %>%
  group_by(lubridate::year(date), lubridate::month(date), name) %>%
  reframe(amount = sum(amount, na.rm = T))

mean(ryan_income$amount, na.rm = T)


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
  pivot_wider(names_from = 'date', values_from = 'amount') |>
  mutate(
    want_need = ifelse(
      type %in%
        c(
          'Amazon',
          'Books',
          'Coffee',
          'Drinks',
          'Eating Out',
          'Fun',
          'Gifts',
          'Gym',
          'Home Improvement',
          'Other',
          'Spotify',
          'TV',
          'Vacation'
        ),
      'Wants',
      ifelse(type %in% c('Brokerage', 'Baby', 'Nest Egg'), 'Savings', 'Needs')
    )
  )
print(ryan_spend, n = 'all')

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

long_df <- ryan_spend %>%
  pivot_longer(
    cols = -c(type, want_need), # all the date columns
    names_to = "date",
    values_to = "amount",
    values_drop_na = TRUE
  ) %>%
  mutate(date = as.Date(date)) # turn names into Date

# sum by want_need for each date
totals_by_need <- long_df %>%
  group_by(date, want_need) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
  group_by(date) %>%
  mutate(
    month_total = sum(total, na.rm = TRUE),
    pct = total / month_total * 100
  ) %>%
  ungroup()

totals_by_need |> filter(want_need == 'Wants')

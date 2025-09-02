library(tidyverse)

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


########################################################################
# Beginning of the Month

beginning <- log %>% filter(month == first_day_this_month)


########################################################################
# Spending

month_out_raw <- raw %>%
  filter(in_out == 'Out') %>%
  filter(date >= first_day_this_month & date <= last_day_this_month) %>%
  group_by(name, primary_category, secondary_category, type, in_out) %>%
  reframe(amount_out = sum(amount, na.rm = T))

month_out_agg <- month_out_raw %>%
  group_by(name, primary_category, secondary_category) %>%
  reframe(amount_out = sum(amount_out, na.rm = T))

month_in_raw <- raw %>%
  filter(in_out == 'In') %>%
  filter(date >= first_day_this_month & date <= last_day_this_month) %>%
  group_by(name, primary_category, secondary_category, type, in_out) %>%
  reframe(amount_in = sum(amount, na.rm = T))

month_in_agg <- month_in_raw %>%
  group_by(name, primary_category, secondary_category) %>%
  reframe(amount_in = sum(amount_in, na.rm = T))


########
end_auto <- beginning %>%
  left_join(
    month_out_agg,
    by = c(
      'name' = 'name',
      'primary_category' = 'primary_category',
      'secondary_category' = 'secondary_category'
    )
  ) %>%
  left_join(
    month_in_agg,
    by = c(
      'name' = 'name',
      'primary_category' = 'primary_category',
      'secondary_category' = 'secondary_category'
    )
  ) %>%
  mutate(amount_out = amount_out * -1) %>%
  mutate(month_end = select(., amount:amount_in) %>% rowSums(na.rm = TRUE))

square_auto <- end_auto %>%
  filter(primary_category %in% c('Checking', 'Savings')) %>%
  group_by(name, primary_category) %>%
  reframe(total = sum(month_end, na.rm = T)) %>%
  mutate(
    end_goal = ifelse(
      name == 'Joint' & primary_category == 'Checking',
      5000,
      NA
    )
  ) %>%
  mutate(difference = total - end_goal)

transfers <- readxl::read_excel(
  "Data/Joint Spending Plan 2025.xlsx",
  sheet = 'transfers'
) %>%
  janitor::clean_names() %>%
  filter(date == last_day_this_month)

square_manual <- transfers %>%
  group_by(name, primary_category, secondary_category) %>%
  reframe(amount = sum(amount, na.rm = T))


square_auto %>%
  left_join(
    square_manual,
    by = c('name' = 'name', 'primary_category' = 'primary_category')
  ) %>%
  mutate(final = total + amount)

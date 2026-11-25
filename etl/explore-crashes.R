# explore-crashes.R: grouping and summarizing the clean crash data
library(tidyverse)

# 1. Read clean data + lookups from public Raw links
crashes  <- read_csv("https://raw.githubusercontent.com/mmalo5/stat440-chicago-crashes/main/data/crashes-clean.csv")
weather_lookup <- read_csv("https://raw.githubusercontent.com/mmalo5/stat440-chicago-crashes/main/data/weather-lookup.csv")
str(crashes)

# 2. New time columns from crash_date (Week 4 Dates, inside mutate)
crashes_time <- crashes |>
  mutate(
    year    = as.numeric(format(crash_date, "%Y")),
    month   = as.numeric(format(crash_date, "%m")),
    hour    = as.numeric(format(crash_date, "%H")),
    weekday = format(crash_date, "%A") 
  )
str(crashes_time)
# Hint: lubridate has functions named after each part (the same names as the
# columns), or use Week 4's format() with codes like "%Y", "%m", "%H".
# Don't convert time zones: the clock times are already Chicago local time.

# 3. An injury flag: decide YOUR definition and write why
#    e.g. injuries_total > 0? (What about NA injuries_total?)

# 4. First summary: crashes per year (one row per YEAR)
crashes_by_year <- crashes_time |>
  filter(year >= 2018, year <= 2025) |>
  group_by(year) |>
  summarize(n_crashes = n())
crashes_by_year
# VALIDATE: 2018 should be 118,952 (matches the year table from Nov 23)

#crashes by month 2019-2021:
crashes_by_month_2019_to_2021 <- crashes_time |>
  filter(year >= 2019, year <= 2021) |>
  group_by(year, month) |>
  summarize(n_crashes = n())
print(crashes_by_month_2019_to_2021, n = 36)

#Mean crashes per DAY, March-April 2019 vs 2020
# Step 1: one row per DAY (count crashes each day)
daily_counts <- crashes_time |>
  filter(2020>=year, year>=2019, 4>=month, month>=3) |>
  mutate(date = as.Date(crash_date)) |>
  group_by(year, date) |>
  summarize(n_crashes = n(), .groups = "drop")
nrow(daily_counts)   # VALIDATE: 122 rows (61 days x 2 years)
as.data.frame(daily_counts)

# Step 2: one row per YEAR (average the daily counts)
daily_compare <- daily_counts |>
  group_by(year) |>
  summarize(
    mean_per_day = round(mean(n_crashes),2),
    sd_per_day = round(sd(n_crashes),2),
    n_days = n()
  )
as.data.frame(daily_compare)

covid_split <- daily_counts |>
  filter(year == 2020) |>
  mutate(period = case_when(
    date <  as.Date("2020-03-21") ~ "before order",
    .default = "after order"
  )) |>
  group_by(period) |>
  summarize(mean_per_day = round(mean(n_crashes), 2),
            sd_per_day = round(sd(n_crashes), 2),
            n_days = n())
as.data.frame(covid_split)
# VALIDATE: mean_per_day x 61 should equal that year's March + April total
#   (2019: 9,748 + 9,446)

# 5. Labels for weather: join the lookup on its shared key (Week 7 joins)
#    VALIDATE: row count is unchanged after the join (still 977,907)
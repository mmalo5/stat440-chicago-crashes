# =====================================================================
# etl-crashes.R
# STAT 440 Final Portfolio: Chicago Traffic Crashes
# Author: Miles Maloney
# Source: Chicago Data Portal, "Traffic Crashes - Crashes"
#         https://data.cityofchicago.org/d/85ca-t3if
# Raw snapshot (GitHub Release, downloaded Nov 22, 2026, full export):
#   https://github.com/mmalo5/stat440-chicago-crashes/releases/download/raw-data-v1/Traffic_Crashes_-_Crashes_20261122-FULL.csv
# Window: Jan 1st 2018 to Nov 21 2026
# Why: citywide reporting began Sept 2017, so I am starting 2018 for the full year. Full years for yearly totals, and 2026 is kept for year-to-date comparisons. The most recent data I could get at the start of the project was Nov 21st. 
# =====================================================================


# ---- 0. Packages ----------------------------------------------------
# TODO: load every package this script uses (cue sheet section 0). Run first.
library(tidyverse)
library(jsonlite)
library(openxlsx)
library(rvest)
library(DBI)
library(RMariaDB)
library(shiny)
library(DT)
library(glue)

# ---- 1. EXTRACT -----------------------------------------------------
# Week 12: Extracting = importing/reading in data from any source.
# Week 1: read from a public URL, never a local path.

# TODO: store the Release link in a snake_case object, e.g. <raw_url>
read_crashes_url <- "https://github.com/mmalo5/stat440-chicago-crashes/releases/download/raw-data-v1/Traffic_Crashes_-_Crashes_20261122-FULL.csv"
# Development only (commented out, like the professor's NBA app):
# "~/Library/CloudStorage/Dropbox/STAT 440/Traffic_Crashes_-_Crashes_20261122-FULL.csv"

# TODO: read the data with the comma-delimited reader (cue sheet section 1)
raw_crashes <- read_csv(read_crashes_url)

# TODO: str(<raw_crashes>)
str(raw_crashes)
# VALIDATE: expected 1,115,837 rows x 49 columns -> got: <...>
# Parsing issues? If yes, run problems() and note what it says: <...>


# ---- 2. INSPECT (before changing anything) --------------------------
# Week 4: modes, missing values, duplicates. Answer each as a comment.

# TODO 2a. Wrong modes? Which columns should be dates/numbers but aren't?
# TODO 2b. Disguised missing values ("UNKNOWN", blanks, 0 coordinates)?
#          Hint: count() a categorical column, sorted.
# TODO 2c. Duplicate ROWS? (anyDuplicated or count(across(everything())))
# TODO 2d. Duplicate CRASH IDs? (count crash_record_id, keep n > 1)
#          Note: 2c and 2d are different questions.
# TODO 2e. How many categories in each categorical column? Any messy or nearly identical?


# ---- 3. TRANSFORM ---------------------------------------------------
# Week 12: Transforming = wrangling data to be cleaned.
# 3a. Cast CRASH_DATE to date-time INSIDE mutate()
#     It looks like "07/28/2020 09:37:00 PM" -> which Week 4 function matches?
#     Store as <crashes_dated>
#     VALIDATE: min() and max() of CRASH_DATE
crashes_dated <- raw_crashes |>
  mutate(CRASH_DATE = mdy_hms(CRASH_DATE))

min(crashes_dated$CRASH_DATE, na.rm = TRUE)
max(crashes_dated$CRASH_DATE, na.rm = TRUE)
# 3b. Filter to the window: on/after Jan 1 2018 AND before Nov 22 2026
#     Store as <crashes_window>
#     VALIDATE: expect 977,907 rows
crashes_window <- crashes_dated |>
  filter(CRASH_DATE >= ymd_hms("2018-01-01 00:00:00"), CRASH_DATE < ymd_hms("2026-11-22 00:00:00"))
nrow(crashes_window)

# 3c. Select the 18 columns
#     VALIDATE: 18 columns
crashes_selected <- crashes_window |>
  select(CRASH_RECORD_ID, CRASH_DATE, LATITUDE, LONGITUDE, STREET_NAME,
         POSTED_SPEED_LIMIT, WEATHER_CONDITION, LIGHTING_CONDITION,
         ROADWAY_SURFACE_COND, TRAFFIC_CONTROL_DEVICE, FIRST_CRASH_TYPE,
         PRIM_CONTRIBUTORY_CAUSE, HIT_AND_RUN_I, CRASH_TYPE, MOST_SEVERE_INJURY,
         INJURIES_TOTAL, INJURIES_FATAL, INJURIES_INCAPACITATING)
ncol(crashes_selected)

# 3d. Rename to snake_case
#     Hint: names are already UPPER_CASE_WITH_UNDERSCORES, so what's the
#     only change needed? (rename_with + one function)
crashes_renamed <- crashes_selected |>
  rename_with(~ tolower(.x))
names(crashes_renamed)

# duplicate rows:
anyDuplicated(crashes_renamed)
# duplicate crash IDs:
crashes_renamed |>
  count(crash_record_id) |>
  filter(n > 1)



# 3e. Coordinates: round latitude & longitude to 5 decimals (about 1 meter)
#     and turn 0 coordinates into NA (they're missing, not real places)
#     VALIDATE: how many rows have NA latitude now?
#     Decision (write 1 sentence): keep those rows (they still count in
#     totals and rates; the map just skips them) or drop them?
crashes_coords <- crashes_renamed |>
  mutate(
    latitude  = case_when(latitude==0 ~ NA, .default = round(latitude, 5)),
    longitude = case_when(longitude==0 ~ NA, .default = round(longitude, 5))
  )
sum(is.na(crashes_coords$latitude))
sum(is.na(crashes_coords$longitude))
#note: can keep 8282 rows with no coordiates but map will skip them

# 3f. Lookup codes, ONE column first (weather_condition):
#     - mutate a new column weather_id = a ranking of weather_condition
#       where equal values get the SAME number and there are no gaps
#       (Week 8 ranking; the tidyverse version has the same name as SQL's)
#     - make the lookup table: one row per (weather_id, weather_condition)
#       Hint: which Week 4 function keeps only unique combinations?
#     VALIDATE: the lookup has 12 rows (13 if missing values got a row)
#     Then repeat for the other 7: lighting_condition, roadway_surface_cond,
#     traffic_control_device, first_crash_type, prim_contributory_cause,
#     crash_type, most_severe_injury
#     Finally, remove the 8 text columns from the main table (keep the _id ones)
crashes_coded <- crashes_coords |>
  mutate(weather_id = dense_rank(weather_condition),
         lighting_id = dense_rank(lighting_condition),
         surface_id = dense_rank(roadway_surface_cond),
         control_id = dense_rank(traffic_control_device),
         first_crash_type_id = dense_rank(first_crash_type),
         cause_id = dense_rank(prim_contributory_cause),
         crash_type_id = dense_rank(crash_type),
         injury_id = dense_rank(most_severe_injury))
crashes_coded

# 3f (part 2). One lookup table per category
weather_lookup <- crashes_coded |>
  distinct(weather_id, weather_condition) |>
  arrange(weather_id)
weather_lookup

lighting_lookup <- crashes_coded |>
  distinct(lighting_id, lighting_condition) |>
  arrange(lighting_id)

surface_lookup <- crashes_coded |>
  distinct(surface_id, roadway_surface_cond) |>
  arrange(surface_id)
surface_lookup

control_lookup <- crashes_coded |>
  distinct(control_id, traffic_control_device) |>
  arrange(control_id)
control_lookup

first_crash_type_lookup <- crashes_coded |>
  distinct(first_crash_type_id, first_crash_type) |>
  arrange(first_crash_type_id)  

cause_lookup <- crashes_coded |>
  distinct(cause_id, prim_contributory_cause) |>
  arrange(cause_id)

crash_type_lookup <- crashes_coded |>
  distinct(crash_type_id, crash_type) |>
  arrange(crash_type_id)

injury_lookup <- crashes_coded |>
  distinct(injury_id, most_severe_injury) |>
  arrange(injury_id)  
nrow(lighting_lookup); nrow(surface_lookup); nrow(control_lookup); nrow(first_crash_type_lookup)
nrow(cause_lookup); nrow(crash_type_lookup); nrow(injury_lookup)

# 3i. Final clean table: drop crash_record_id and the 8 text columns
#     (the text now lives in the lookup tables; the ID was only needed for the
#     duplicate check and is ~115 MB on its own)
crashes_clean <- crashes_coded |>
  select(-crash_record_id, -weather_condition, -lighting_condition, -roadway_surface_cond, 
         -traffic_control_device, -first_crash_type, -prim_contributory_cause, -crash_type, -most_severe_injury)

# VALIDATE:
str(crashes_clean)

# 3g. (later) Clean the LABELS in the lookup tables, e.g. "UNKNOWN" -> NA.
#     Much easier on a 12-row table than on 977,907 rows!


# ---- 4. LOAD --------------------------------------------------------
# Week 12: Loading = exporting clean data to a repository/public space.

# TODO 4a. Summary tables (group_by + summarize) -> write_csv() into data/
write_csv(crashes_clean, "data/crashes-clean.csv")
# TODO 4b. Detail file -> write_csv() into data/ (one file per year if > 100 MB)
write_csv(weather_lookup, "data/weather-lookup.csv")
write_csv(lighting_lookup, "data/lighting-lookup.csv")
write_csv(surface_lookup, "data/surface-lookup.csv")
write_csv(control_lookup, "data/control-lookup.csv")
write_csv(first_crash_type_lookup, "data/first-crash-type-lookup.csv")
write_csv(cause_lookup, "data/cause-lookup.csv")
write_csv(crash_type_lookup, "data/crash-type-lookup.csv")
write_csv(injury_lookup, "data/injury-lookup.csv")

# TODO 4c. Check file sizes in the Files pane (each < 100 MB), then commit + push
# TODO 4d. (Optional) Load into MySQL: dbConnect() with Sys.getenv(), dbWriteTable()


# ---- 5. VERIFY ------------------------------------------------------
# TODO: Restart R, read one clean file back from its GitHub Raw link,
#       and confirm rows/columns match what you wrote.
check_clean  <- read_csv("https://raw.githubusercontent.com/mmalo5/stat440-chicago-crashes/main/data/crashes-clean.csv")
check_lookup <- read_csv("https://raw.githubusercontent.com/mmalo5/stat440-chicago-crashes/main/data/weather-lookup.csv")
dim(check_clean)    # expect 977907 17
dim(check_lookup) 




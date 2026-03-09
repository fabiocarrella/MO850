library(jsonlite)
library(ndjson)
library(dplyr)
library(lubridate)
library(stringr)

# Step 1: read the full file
file_path <- ""
df <- ndjson::stream_in(file_path)

# Step 2: convert timestamp, create time bins
df_binned <- df %>%
  mutate(
    date_parsed = as.POSIXct(date / 1000, origin = "1970-01-01"),
    time_bin = floor_date(date_parsed, unit = "month")
  )

# Step 3: check distribution across bins
df_binned %>% count(time_bin)

# Step 4: filter out first two months
df_binned <- df_binned %>%
  filter(time_bin >= as.Date("2020-03-01"))

# Step 5: stratified sample of 1000 per month
sampled <- df_binned %>%
  group_by(time_bin) %>%
  slice_sample(n = 1000) %>%
  ungroup()

nrow(sampled)

# Step 6: save
write_excel_csv(sampled, "timebin_sampled_telegram.csv")
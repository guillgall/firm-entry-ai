# =============================================================================
# 02_clean_bfs.R
# Parse the Census BFS monthly file (bfs_monthly.csv) and produce a tidy
# long panel of SEASONALLY ADJUSTED business applications by NAICS-2 sector.
#
# Actual file structure (wide by month, one row per sa/naics/series/geo/year):
#   sa           – "S" = seasonally adjusted, "U" = unadjusted
#   naics_sector – "TOTAL", "NAICS11", "NAICS21", …
#   series       – "BA_BA" = business applications, etc.
#   geo          – "US" (national); state codes also present
#   year         – integer year
#   jan … dec    – monthly values
#
# OUTPUT: Data/Output/bfs_panel.rds  with columns:
#   naics2  – 2-digit NAICS code (character); "US" = national aggregate
#   date    – first day of the month (Date)
#   ba      – seasonally adjusted business applications
#   ba_idx  – ba indexed to January 2020 = 100
# =============================================================================

library(tidyverse)
library(lubridate)

setwd("/Users/ggallacher/Documents/GitHub/firm-entry-ai")
ROOT <- getwd()

raw_path <- file.path(ROOT, "Data", "Input", "bfs_monthly.csv")

if (!file.exists(raw_path)) {
  stop("Raw BFS file not found. Run 01_download_bfs.R first.")
}

# ---------------------------------------------------------------------------
# 1. Read
# ---------------------------------------------------------------------------
raw <- read_csv(raw_path, show_col_types = FALSE)
message("Raw dims: ", nrow(raw), " x ", ncol(raw))

# ---------------------------------------------------------------------------
# 2. Filter: seasonally adjusted, business applications, national level
# ---------------------------------------------------------------------------
month_cols <- c("jan","feb","mar","apr","may","jun",
                "jul","aug","sep","oct","nov","dec")

panel <- raw |>
  filter(
    sa     == "A",          # seasonally adjusted only ("A" = adjusted)
    series == "BA_BA",      # total business applications
    geo    == "US"          # national level
  ) |>
  # Extract 2-digit NAICS: "NAICS11" → "11", "TOTAL" → "US"
  mutate(
    naics2 = case_when(
      naics_sector == "TOTAL" ~ "US",
      TRUE ~ str_pad(str_extract(naics_sector, "\\d+"), 2, pad = "0")
    )
  ) |>
  select(naics2, year, all_of(month_cols)) |>
  # Pivot months → long
  mutate(across(all_of(month_cols), as.numeric)) |>   # force numeric (mixed types from CSV)
  pivot_longer(
    cols      = all_of(month_cols),
    names_to  = "month_name",
    values_to = "ba"
  ) |>
  filter(!is.na(ba)) |>
  mutate(
    month_num = match(month_name, tolower(month.abb)),
    date      = ymd(paste(year, month_num, "01", sep = "-"))
  ) |>
  select(naics2, date, ba) |>
  arrange(naics2, date)

message("Panel rows: ",      nrow(panel))
message("Sectors (naics2): ", paste(sort(unique(panel$naics2)), collapse = ", "))
message("Date range: ",      min(panel$date), " to ", max(panel$date))

# ---------------------------------------------------------------------------
# 3. Index to January 2020 = 100
# ---------------------------------------------------------------------------
base_values <- panel |>
  filter(date == as.Date("2020-01-01")) |>
  group_by(naics2) |>
  summarise(ba_base = first(ba), .groups = "drop")

panel <- panel |>
  left_join(base_values, by = "naics2") |>
  mutate(
    ba_idx = if_else(!is.na(ba_base) & ba_base > 0,
                     100 * ba / ba_base,
                     NA_real_)
  ) |>
  select(naics2, date, ba, ba_idx)

missing_base <- panel |>
  group_by(naics2) |>
  summarise(has_base = any(!is.na(ba_idx)), .groups = "drop") |>
  filter(!has_base)

if (nrow(missing_base) > 0) {
  warning("Sectors missing Jan-2020 base: ",
          paste(missing_base$naics2, collapse = ", "))
}

# ---------------------------------------------------------------------------
# 4. Save
# ---------------------------------------------------------------------------
out_path <- file.path(ROOT, "Data", "Output", "bfs_panel.rds")
saveRDS(panel, out_path)
message("Saved: ", out_path)
print(panel |> filter(naics2 == "US") |> tail(12))

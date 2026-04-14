# =============================================================================
# 02_clean_bfs.R
# Parse the Census BFS monthly file (bfs_monthly.csv) and produce:
#   1. bfs_panel.rds  – NAICS-2 x time panel with SA BA and alternative series
#      Columns: naics2, date, ba, hba, wba, bf4q, ba_idx
#   2. bfs_state.rds  – state x time panel with SA total BA
#      Columns: state, date, ba, ba_idx
#
# Actual file structure (wide by month, one row per sa/naics/series/geo/year):
#   sa           – "A" = seasonally adjusted, "U" = unadjusted
#   naics_sector – "TOTAL", "NAICS11", "NAICS21", …
#   series       – "BA_BA", "BA_HBA", "BA_WBA", "BF_BF4Q", etc.
#   geo          – "US" (national) or 2-letter state abbreviation
#   year         – integer year
#   jan … dec    – monthly values
# =============================================================================

library(tidyverse)
library(lubridate)
library(here)

ROOT <- here::here()

raw_path <- file.path(ROOT, "Data", "Input", "bfs_monthly.csv")
if (!file.exists(raw_path)) stop("Raw BFS file not found. Run 01_download_bfs.R first.")

# ---------------------------------------------------------------------------
# 1. Read
# ---------------------------------------------------------------------------
raw <- read_csv(raw_path, show_col_types = FALSE)
message("Raw dims: ", nrow(raw), " x ", ncol(raw))
message("Series available: ", paste(sort(unique(raw$series)), collapse = ", "))

month_cols <- c("jan","feb","mar","apr","may","jun",
                "jul","aug","sep","oct","nov","dec")

# ---------------------------------------------------------------------------
# Helper: wide month columns → long, return date + value
# ---------------------------------------------------------------------------
to_long <- function(df) {
  df |>
    mutate(across(all_of(month_cols), \(x) suppressWarnings(as.numeric(x)))) |>
    pivot_longer(
      cols      = all_of(month_cols),
      names_to  = "month_name",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    mutate(
      month_num = match(month_name, tolower(month.abb)),
      date      = ymd(paste(year, month_num, "01", sep = "-"))
    ) |>
    select(-year, -month_name, -month_num)
}

# ---------------------------------------------------------------------------
# Helper: index a numeric vector relative to Jan 2020 = 100
# ---------------------------------------------------------------------------
index_jan2020 <- function(dates, values) {
  idx    <- which(dates == as.Date("2020-01-01"))
  base   <- if (length(idx) > 0) values[idx[1]] else NA_real_
  if (is.na(base) || base == 0) return(rep(NA_real_, length(values)))
  100 * values / base
}

# ---------------------------------------------------------------------------
# 2. NAICS-level panel (national): BA, HBA, WBA, BF4Q
# ---------------------------------------------------------------------------
naics_series <- c(
  "BA_BA",    # Total business applications
  "BA_CBA",   # Corrected business applications (removes likely non-employer apps)
  "BA_HBA",   # High-propensity business applications
  "BA_WBA",   # Applications with planned wages
  "BF_BF4Q",  # Formations within 4 quarters
  "BF_BF8Q",  # Formations within 8 quarters
  "BF_PBF4Q", # Projected formations within 4 quarters
  "BF_PBF8Q", # Projected formations within 8 quarters
  "BF_SBF4Q", # Seasonally-adjusted formations within 4 quarters
  "BF_SBF8Q", # Seasonally-adjusted formations within 8 quarters
  "BF_DUR4Q", # Median weeks from application to formation (4Q window)
  "BF_DUR8Q"  # Median weeks from application to formation (8Q window)
)

naics_long <- raw |>
  filter(sa == "A", series %in% naics_series, geo == "US") |>
  mutate(
    # BFS aggregates some groups into non-numeric codes:
    #   NAICSMNF = Manufacturing (NAICS 31-33 combined)
    #   NAICSRET = Retail Trade  (NAICS 44-45 combined)
    #   NAICSTW  = Transportation & Warehousing (NAICS 48-49 combined)
    #   NONAICS  = Unclassified / no NAICS code assigned
    naics2 = case_when(
      naics_sector == "TOTAL"    ~ "US",
      naics_sector == "NAICSMNF" ~ "MNF",
      naics_sector == "NAICSRET" ~ "RET",
      naics_sector == "NAICSTW"  ~ "TW",
      naics_sector == "NONAICS"  ~ "NONAIC",
      TRUE ~ str_pad(str_extract(naics_sector, "\\d+"), 2, pad = "0")
    ),
    series_clean = case_when(
      series == "BA_BA"   ~ "ba",
      series == "BA_HBA"  ~ "hba",
      series == "BA_WBA"  ~ "wba",
      series == "BF_BF4Q"  ~ "bf4q",
      series == "BF_BF8Q"  ~ "bf8q",
      series == "BA_CBA"   ~ "cba",
      series == "BF_PBF4Q" ~ "pbf4q",
      series == "BF_PBF8Q" ~ "pbf8q",
      series == "BF_SBF4Q" ~ "sbf4q",
      series == "BF_SBF8Q" ~ "sbf8q",
      series == "BF_DUR4Q" ~ "dur4q",
      series == "BF_DUR8Q" ~ "dur8q",
      TRUE                 ~ series
    )
  ) |>
  select(naics2, series_clean, year, all_of(month_cols)) |>
  to_long()

# Collapse any duplicate naics2/series/date cells by summing
# (duplicates arise when multiple raw rows share the same key, e.g. vintage
# overlaps in the BF series; summing is wrong for rates so we take the mean)
dups <- naics_long |>
  count(naics2, series_clean, date) |>
  filter(n > 1)
if (nrow(dups) > 0) {
  message("Duplicate naics2/series/date cells: ", nrow(dups),
          " — collapsing by mean.")
  naics_long <- naics_long |>
    group_by(naics2, series_clean, date) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

message("NAICS-level series found: ",
        paste(sort(unique(naics_long$series_clean)), collapse = ", "))

naics_panel <- naics_long |>
  pivot_wider(
    id_cols     = c(naics2, date),
    names_from  = series_clean,
    values_from = value
  ) |>
  arrange(naics2, date)

# Index BA to Jan 2020 = 100
naics_panel <- naics_panel |>
  group_by(naics2) |>
  mutate(ba_idx = index_jan2020(date, ba)) |>
  ungroup()

message("NAICS panel rows: ", nrow(naics_panel))
message("Sectors (naics2): ",
        paste(sort(unique(naics_panel$naics2)), collapse = ", "))
message("Date range: ", min(naics_panel$date), " to ", max(naics_panel$date))
message("Columns: ", paste(names(naics_panel), collapse = ", "))

saveRDS(naics_panel, file.path(ROOT, "Data", "Output", "bfs_panel.rds"))
message("Saved: Data/Output/bfs_panel.rds")
print(naics_panel |> filter(naics2 == "US") |> tail(6))

# ---------------------------------------------------------------------------
# 3. State-level panel: multiple SA series, TOTAL, by state
#    BF / formation series are national-only in BFS; state level has BA variants
# ---------------------------------------------------------------------------
state_series <- c("BA_BA", "BA_HBA", "BA_WBA", "BA_CBA")

state_long <- raw |>
  filter(sa == "A", series %in% state_series,
         naics_sector == "TOTAL", geo != "US") |>
  rename(state = geo) |>
  mutate(series_clean = case_when(
    series == "BA_BA"  ~ "ba",
    series == "BA_HBA" ~ "hba",
    series == "BA_WBA" ~ "wba",
    series == "BA_CBA" ~ "cba",
    TRUE               ~ series
  )) |>
  select(state, series_clean, year, all_of(month_cols)) |>
  to_long()

# Deduplicate within series
dups_s <- state_long |> count(state, series_clean, date) |> filter(n > 1)
if (nrow(dups_s) > 0) {
  message("Duplicate state/series/date rows (", nrow(dups_s), "); collapsing by mean.")
  state_long <- state_long |>
    group_by(state, series_clean, date) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

message("State series found: ",
        paste(sort(unique(state_long$series_clean)), collapse = ", "))

state_panel <- state_long |>
  pivot_wider(
    id_cols     = c(state, date),
    names_from  = series_clean,
    values_from = value
  ) |>
  group_by(state) |>
  mutate(ba_idx = index_jan2020(date, ba)) |>
  ungroup() |>
  arrange(state, date)

message("\nState panel rows: ", nrow(state_panel))
message("States: ", n_distinct(state_panel$state), " — ",
        paste(sort(unique(state_panel$state)), collapse = ", "))
message("Date range: ", min(state_panel$date), " to ", max(state_panel$date))

saveRDS(state_panel, file.path(ROOT, "Data", "Output", "bfs_state.rds"))
message("Saved: Data/Output/bfs_state.rds")
print(state_panel |> filter(state == "CA") |> tail(6))

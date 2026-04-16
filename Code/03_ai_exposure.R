# =============================================================================
# 03_ai_exposure.R
# Download Felten, Raj & Seamans AIOE / AIIE data from:
#   https://github.com/AIOE-Data/AIOE
#
# File: AIOE_DataAppendix.xlsx
# Relevant sheet: "Data Appendix B" – AIIE Scores by 4-digit NAICS industry
# We aggregate to 2-digit NAICS to match the BFS panel.
# =============================================================================

library(tidyverse)
library(httr)
library(readxl)
library(haven)
library(here)

# State name → BFS abbreviation crosswalk (matches geo codes in bfs_monthly.csv)
state_crosswalk <- tribble(
  ~state_name,            ~state,
  "Alabama",              "AL",
  "Alaska",               "AK",
  "Arizona",              "AZ",
  "Arkansas",             "AR",
  "California",           "CA",
  "Colorado",             "CO",
  "Connecticut",          "CT",
  "Delaware",             "DE",
  "District of Columbia", "DC",
  "Florida",              "FL",
  "Georgia",              "GA",
  "Hawaii",               "HI",
  "Idaho",                "ID",
  "Illinois",             "IL",
  "Indiana",              "IN",
  "Iowa",                 "IA",
  "Kansas",               "KS",
  "Kentucky",             "KY",
  "Louisiana",            "LA",
  "Maine",                "ME",
  "Maryland",             "MD",
  "Massachusetts",        "MA",
  "Michigan",             "MI",
  "Minnesota",            "MN",
  "Mississippi",          "MS",
  "Missouri",             "MO",
  "Montana",              "MT",
  "Nebraska",             "NE",
  "Nevada",               "NV",
  "New Hampshire",        "NH",
  "New Jersey",           "NJ",
  "New Mexico",           "NM",
  "New York",             "NY",
  "North Carolina",       "NC",
  "North Dakota",         "ND",
  "Ohio",                 "OH",
  "Oklahoma",             "OK",
  "Oregon",               "OR",
  "Pennsylvania",         "PA",
  "Rhode Island",         "RI",
  "South Carolina",       "SC",
  "South Dakota",         "SD",
  "Tennessee",            "TN",
  "Texas",                "TX",
  "Utah",                 "UT",
  "Vermont",              "VT",
  "Virginia",             "VA",
  "Washington",           "WA",
  "West Virginia",        "WV",
  "Wisconsin",            "WI",
  "Wyoming",              "WY",
  "Puerto Rico",          "PR"
)

ROOT <- here::here()

# ---------------------------------------------------------------------------
# 1. Download
# ---------------------------------------------------------------------------
AIOE_URL   <- paste0("https://raw.githubusercontent.com/AIOE-Data/AIOE/",
                     "main/AIOE_DataAppendix.xlsx")
local_path <- file.path(ROOT, "Data", "Input", "AIOE_DataAppendix.xlsx")

message("Downloading AIOE Data Appendix...")
resp <- GET(AIOE_URL,
            write_disk(local_path, overwrite = TRUE),
            progress(),
            timeout(120))

if (http_error(resp)) {
  stop(
    "Download failed (HTTP ", status_code(resp), ").\n",
    "Please download AIOE_DataAppendix.xlsx manually from:\n",
    "  https://github.com/AIOE-Data/AIOE\n",
    "and save to Data/Input/AIOE_DataAppendix.xlsx"
  )
}
message("Saved: ", local_path,
        "  (", round(file.size(local_path) / 1024, 1), " KB)")

# ---------------------------------------------------------------------------
# 2. Inspect sheets
# ---------------------------------------------------------------------------
sheets <- excel_sheets(local_path)
message("Sheets: ", paste(sheets, collapse = " | "))

# Target: the AIIE-by-industry sheet (Data Appendix B)
sheet_use <- sheets[str_detect(tolower(sheets), "appendix.?b|aiie|industry")][1]
if (is.na(sheet_use)) sheet_use <- sheets[2]   # fallback: second sheet
message("Using sheet: ", sheet_use)

# ---------------------------------------------------------------------------
# 3. Read
# ---------------------------------------------------------------------------
raw <- read_excel(local_path, sheet = sheet_use, skip = 0)
message("Raw dims: ", nrow(raw), " x ", ncol(raw))
print(head(raw, 5))
message("Column names: ", paste(names(raw), collapse = ", "))

# ---------------------------------------------------------------------------
# 4. Identify NAICS and AIIE columns
# ---------------------------------------------------------------------------
naics_col <- names(raw)[str_detect(tolower(names(raw)),
                                   "naics|ind_?code|industry.?code|sic")][1]
aiie_col  <- names(raw)[str_detect(tolower(names(raw)),
                                   "aiie|aioe|exposure|ai.?score|ai_exp")][1]

if (is.na(naics_col)) {
  # Last resort: assume first column is NAICS
  naics_col <- names(raw)[1]
  warning("Could not detect NAICS column; using first column: ", naics_col)
}
if (is.na(aiie_col)) {
  # Last resort: assume last numeric column is the score
  num_cols <- names(raw)[sapply(raw, is.numeric)]
  aiie_col <- tail(num_cols, 1)
  warning("Could not detect AIIE column; using: ", aiie_col)
}

message("NAICS column : ", naics_col)
message("AIIE column  : ", aiie_col)

# ---------------------------------------------------------------------------
# 5. Download OES employment by 4-digit NAICS from Felten et al.'s own repo
# This is the same file they used to construct the AIIE, so NAICS codes are
# guaranteed to match Appendix B.
# ---------------------------------------------------------------------------
OES_URL      <- paste0("https://raw.githubusercontent.com/AIOE-Data/AIOE/",
                       "main/Input/oes_4dig_naics.zip")
oes_zip_path <- file.path(ROOT, "Data", "Input", "oes_4dig_naics.zip")
oes_dir      <- file.path(ROOT, "Data", "Input", "oes_4dig_naics")

if (!dir.exists(oes_dir)) {
  if (!file.exists(oes_zip_path)) {
    message("Downloading OES 4-digit NAICS employment from AIOE repo...")
    resp_oes <- GET(OES_URL,
                    write_disk(oes_zip_path, overwrite = TRUE),
                    progress(), timeout(120))
    if (http_error(resp_oes))
      stop("OES download failed (HTTP ", status_code(resp_oes), ").")
    message("Saved: ", oes_zip_path)
  }
  unzip(oes_zip_path, exdir = oes_dir)
}

oes_file <- list.files(oes_dir, pattern = "\\.(csv|xlsx?|dta)$",
                       full.names = TRUE, recursive = TRUE)[1]
message("Reading OES file: ", oes_file)

oes_raw <- if (str_detect(oes_file, "\\.csv$")) {
  read_csv(oes_file, show_col_types = FALSE)
} else if (str_detect(oes_file, "\\.dta$")) {
  read_dta(oes_file)
} else {
  read_excel(oes_file)
}

message("OES dims: ", nrow(oes_raw), " x ", ncol(oes_raw))
message("OES columns: ", paste(names(oes_raw), collapse = ", "))

# Extract 4-digit NAICS and total employment.
# OES NAICS codes are 6-digit strings (e.g., "113300"); first 4 chars = 4-digit.
# occ_code "00-0000" = all-occupations total for that industry.
# Use all ownership codes (own_code) to match Felten et al.'s industry totals.
oes_emp <- oes_raw |>
  rename_with(tolower) |>
  filter(occ_code == "00-0000") |>           # all-occupations industry total
  mutate(
    naics4 = substr(as.character(naics), 1, 4),
    emp    = suppressWarnings(as.numeric(tot_emp))
  ) |>
  filter(!is.na(emp), nchar(naics4) == 4) |>
  group_by(naics4) |>
  summarise(emp = sum(emp, na.rm = TRUE), .groups = "drop") |>
  mutate(naics2 = str_pad(substr(naics4, 1, 2), 2, pad = "0"))

message("OES rows (industry totals): ", nrow(oes_emp))

# ---------------------------------------------------------------------------
# 5b. Clean AIIE and merge employment weights
# ---------------------------------------------------------------------------
ai_exp <- raw |>
  rename(naics_raw = all_of(naics_col),
         aiie      = all_of(aiie_col))  |>
  mutate(
    naics_raw = as.character(naics_raw),
    # Strip trailing text / dashes (some cells: "11-Agriculture")
    naics_num = str_extract(naics_raw, "^\\d+"),
    naics4    = str_pad(str_extract(naics_num, "^\\d{1,4}"), 4, pad = "0"),
    naics2    = str_pad(substr(naics_num, 1, 2), 2, pad = "0"),
    aiie      = as.numeric(aiie)
  ) |>
  filter(!is.na(aiie), !is.na(naics2),
         naics2 != "NA", naics2 != "00") |>
  left_join(oes_emp |> select(naics4, emp), by = "naics4")

message("Rows after filter: ", nrow(ai_exp))
message("Unique 2-digit NAICS: ", n_distinct(ai_exp$naics2))
message("Rows with OES employment weight: ", sum(!is.na(ai_exp$emp)))

# Aggregate to 2-digit using 2019 OES employment weights (consistent with
# Felten et al., who use 2019 employment to construct AIIE at 4-digit NAICS).
# Fall back to unweighted mean for any 4-digit codes not matched in OES.
ai_exp2 <- ai_exp |>
  group_by(naics2) |>
  summarise(
    aiie = if (any(!is.na(emp)))
             weighted.mean(aiie, w = replace_na(emp, 0), na.rm = TRUE)
           else
             mean(aiie, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 5c. Add BFS combined-sector codes
# The BFS aggregates Manufacturing (31-33), Retail (44-45), and
# Transportation & Warehousing (48-49) into single codes MNF, RET, TW.
# Use employment-weighted averages of the constituent 2-digit AIIE scores,
# weighting by total 2019 OES employment within each 2-digit sector.
oes_naics2_emp <- oes_emp |>
  group_by(naics2) |>
  summarise(emp2 = sum(emp, na.rm = TRUE), .groups = "drop")

combined_sectors <- tribble(
  ~naics2, ~components,
  "MNF",   c("31", "32", "33"),
  "RET",   c("44", "45"),
  "TW",    c("48", "49")
)

combined_aioe <- combined_sectors |>
  mutate(
    aiie = map_dbl(components, ~ {
      sub <- ai_exp2 |>
        filter(naics2 %in% .x) |>
        left_join(oes_naics2_emp, by = "naics2")
      if (nrow(sub) == 0 || all(is.na(sub$emp2))) {
        mean(sub$aiie, na.rm = TRUE)
      } else {
        weighted.mean(sub$aiie, w = replace_na(sub$emp2, 0), na.rm = TRUE)
      }
    })
  ) |>
  select(naics2, aiie) |>
  filter(!is.na(aiie))

ai_exp2 <- bind_rows(ai_exp2, combined_aioe)
message("Added combined-sector codes (employment-weighted): ",
        paste(combined_aioe$naics2, collapse = ", "))

# ---------------------------------------------------------------------------
# 6. Normalise and add quartile
# ---------------------------------------------------------------------------
ai_exp2 <- ai_exp2 |>
  mutate(
    aioe_norm     = (aiie - min(aiie)) / (max(aiie) - min(aiie)),
    aioe_quartile = ntile(aiie, 4)
  ) |>
  rename(aioe = aiie) |>
  arrange(desc(aioe))

message("\nAI exposure summary (2-digit NAICS):")
print(summary(ai_exp2$aioe))
message("\nAll sectors:")
print(ai_exp2, n = Inf)

# ---------------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------------
out_path <- file.path(ROOT, "Data", "Output", "ai_exposure_naics.rds")
saveRDS(ai_exp2, out_path)
message("Saved: ", out_path)

# ---------------------------------------------------------------------------
# 8. State-level AIGE from Data Appendix C
# ---------------------------------------------------------------------------
message("\nExtracting state-level AIGE from Appendix C...")
aige_raw <- read_excel(local_path, sheet = "Appendix C", skip = 0)
message("Appendix C dims: ", nrow(aige_raw), " x ", ncol(aige_raw))

# State rows have FIPS codes divisible by 1000 (e.g., 1000 = Alabama)
state_aige <- aige_raw |>
  rename(fips = 1, geo_name = 2, aige = 3) |>
  mutate(fips = as.integer(fips)) |>
  filter(!is.na(fips), fips %% 1000 == 0) |>
  inner_join(state_crosswalk, by = c("geo_name" = "state_name")) |>
  mutate(
    aige_norm     = (aige - min(aige)) / (max(aige) - min(aige)),
    aige_quartile = ntile(aige, 4)
  ) |>
  select(state, geo_name, fips, aige, aige_norm, aige_quartile) |>
  arrange(desc(aige))

message("State AIGE rows: ", nrow(state_aige))
message("\nState AI Geographic Exposure (AIGE) summary:")
print(summary(state_aige$aige))
message("\nTop 10 states by AIGE:")
print(head(state_aige, 10))

out_path_state <- file.path(ROOT, "Data", "Output", "state_aige.rds")
saveRDS(state_aige, out_path_state)
message("Saved: ", out_path_state)

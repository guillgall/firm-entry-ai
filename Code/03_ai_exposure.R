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

setwd("/Users/ggallacher/Documents/GitHub/firm-entry-ai")
ROOT <- getwd()

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
# 5. Clean and aggregate to 2-digit NAICS
# ---------------------------------------------------------------------------
ai_exp <- raw |>
  rename(naics_raw = all_of(naics_col),
         aiie      = all_of(aiie_col))  |>
  mutate(
    naics_raw = as.character(naics_raw),
    # Strip trailing text / dashes (some cells: "11-Agriculture")
    naics_num = str_extract(naics_raw, "^\\d+"),
    naics2    = str_pad(substr(naics_num, 1, 2), 2, pad = "0"),
    aiie      = as.numeric(aiie)
  ) |>
  filter(!is.na(aiie), !is.na(naics2),
         naics2 != "NA", naics2 != "00")

message("Rows after filter: ", nrow(ai_exp))
message("Unique 2-digit NAICS: ", n_distinct(ai_exp$naics2))

# Aggregate to 2-digit (mean across 4-digit sub-industries)
ai_exp2 <- ai_exp |>
  group_by(naics2) |>
  summarise(aiie = mean(aiie, na.rm = TRUE), .groups = "drop")

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

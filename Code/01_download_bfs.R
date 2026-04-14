# =============================================================================
# 01_download_bfs.R
# Download Census Bureau Business Formation Statistics (BFS)
# Monthly SEASONALLY ADJUSTED national series (bfs_monthly.csv)
# Source: https://www.census.gov/econ/bfs/current/index.html
# =============================================================================

library(tidyverse)
library(httr)
library(here)

ROOT <- here::here()

# Census BFS monthly SA CSV (national aggregate, 2004-present):
BFS_URL  <- "https://www.census.gov/econ/bfs/csv/bfs_monthly.csv"
raw_path <- file.path(ROOT, "Data", "Input", "bfs_monthly.csv")

# Download ----------------------------------------------------------------

message("Downloading BFS monthly SA data from Census Bureau...")

resp <- GET(
  BFS_URL,
  write_disk(raw_path, overwrite = TRUE),
  progress(),
  timeout(120)
)

if (http_error(resp)) {
  stop(
    "Download failed (HTTP ", status_code(resp), "). ",
    "Please visit https://www.census.gov/econ/bfs/current/index.html ",
    "and download bfs_monthly.csv manually to Data/Input/bfs_monthly.csv, ",
    "then re-run 02_clean_bfs.R."
  )
}

message("Saved to: ", raw_path)
message("File size: ", round(file.size(raw_path) / 1024, 1), " KB")

# Preview -----------------------------------------------------------------
preview <- read_csv(raw_path, n_max = 5, show_col_types = FALSE)
message("Columns (", ncol(preview), "): ", paste(names(preview), collapse = ", "))
print(preview)

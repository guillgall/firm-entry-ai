# =============================================================================
# 00_setup.R
# Initialise renv and install all required packages.
# Run this once before running any other script.
# =============================================================================

# Bootstrap renv if not already present
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

library(renv)

# Initialise renv in the project root (one level above Code/)
renv::init(project = here::here(), restart = FALSE)

# Required packages
pkgs <- c(
  "tidyverse",    # data wrangling + ggplot2
  "fixest",       # two-way FE + clustered SEs
  "modelsummary", # regression tables → LaTeX
  "here",         # portable file paths
  "lubridate",    # date handling
  "readxl",       # read Excel files from Census / AIOE
  "httr",         # HTTP downloads
  "jsonlite",     # parse GitHub API responses
  "ggplot2",      # figures (loaded via tidyverse but explicit here)
  "Cairo"         # cairo_pdf device for PDF output
)

renv::install(pkgs)
renv::snapshot()

message("\nSetup complete. You can now run the numbered scripts in order:\n",
        "  source(here::here('Code', '01_download_bfs.R'))\n",
        "  source(here::here('Code', '02_clean_bfs.R'))\n",
        "  source(here::here('Code', '03_ai_exposure.R'))\n",
        "  source(here::here('Code', '04_merge_and_analysis.R'))\n")

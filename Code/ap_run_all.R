# =============================================================================
# 05_run_all.R
# Master script: run the full pipeline in order.
# Usage:  Rscript Code/05_run_all.R   (from project root)
#      or source("Code/05_run_all.R") (from within R/RStudio)
# =============================================================================

setwd("/Users/ggallacher/Documents/GitHub/firm-entry-ai")

steps <- list(
  list(script = "Code/01_download_bfs.R",      label = "01 Download BFS (monthly SA)"),
  list(script = "Code/02_clean_bfs.R",          label = "02 Clean BFS → panel"),
  list(script = "Code/03_ai_exposure.R",         label = "03 AI exposure (AIOE/AIIE)"),
  list(script = "Code/04_merge_and_analysis.R",  label = "04 Merge, regressions & figures")
)

for (s in steps) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("  RUNNING:", s$label, "\n")
  cat(strrep("=", 60), "\n", sep = "")
  source(s$script, echo = FALSE)
  cat("  DONE:", s$label, "\n")
}

cat("\n", strrep("=", 60), "\n", sep = "")
cat("  Pipeline complete. Figures and tables are in Draft/\n")
cat(strrep("=", 60), "\n", sep = "")

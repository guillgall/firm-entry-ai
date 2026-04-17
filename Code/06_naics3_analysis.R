# =============================================================================
# 06_naics3_analysis.R
# Scatter plot of AI exposure vs. change in weekly business applications
# at 3-DIGIT NAICS — mirrors the 2-digit scatter in 04_merge_and_analysis.R
#
# Data source: https://www.census.gov/econ/bfs/csv/naics3.csv
# Outputs written to Draft/:
#   fig_scatter_naics3.pdf
#   tab_scatter_naics3.tex
# =============================================================================

library(tidyverse)
library(lubridate)
library(modelsummary)
library(ggplot2)
library(ggrepel)
library(httr)
library(readxl)
library(here)

ROOT <- here::here()

CHATGPT_WEEK <- as.Date("2022-11-28")   # Monday of ISO week 48 / 2022

# ---------------------------------------------------------------------------
# 0. Helper: Monday of ISO week w in year y
#    Uses arithmetic only — avoids %G/%V/%u which are not portable on Windows.
#    Jan 4 is always in ISO week 1, so we find the Monday of week 1 then offset.
# ---------------------------------------------------------------------------
yw_to_date <- function(y, w) {
  jan4   <- as.Date(paste0(y, "-01-04"))
  wday   <- as.integer(format(jan4, "%u"))   # 1 = Monday, 7 = Sunday
  mon_w1 <- jan4 - (wday - 1L)              # Monday of ISO week 1
  mon_w1 + (as.integer(w) - 1L) * 7L        # Monday of ISO week w
}

# ---------------------------------------------------------------------------
# 1. Download / read naics3.csv
# ---------------------------------------------------------------------------
NAICS3_URL <- "https://www.census.gov/econ/bfs/csv/naics3.csv"
raw_path   <- file.path(ROOT, "Data", "Input", "bfs_naics3.csv")

if (!file.exists(raw_path)) {
  message("Downloading 3-digit NAICS weekly BFS from Census Bureau...")
  resp <- GET(NAICS3_URL,
              write_disk(raw_path, overwrite = TRUE),
              progress(), timeout(120))
  if (http_error(resp))
    stop("Download failed (HTTP ", status_code(resp),
         "). Manually save ", NAICS3_URL, " to ", raw_path)
  message("Saved: ", raw_path)
} else {
  message("Using cached file: ", raw_path)
}

# ---------------------------------------------------------------------------
# 2. Read wide → pivot long
# ---------------------------------------------------------------------------
raw <- read_csv(raw_path, show_col_types = FALSE)
message("Raw dims: ", nrow(raw), " x ", ncol(raw))

naics_col <- names(raw)[str_detect(tolower(names(raw)), "^naics")][1]
desc_col  <- names(raw)[str_detect(tolower(names(raw)), "description|title|name")][1]
if (is.na(naics_col)) naics_col <- names(raw)[1]
if (is.na(desc_col))  desc_col  <- names(raw)[2]

week_cols <- names(raw)[str_detect(names(raw), "^\\d{4}w\\d{1,2}$")]
message("Week columns found: ", length(week_cols),
        "  (", week_cols[1], " ... ", tail(week_cols, 1), ")")

bfs3 <- raw |>
  rename(naics3 = all_of(naics_col), description = all_of(desc_col)) |>
  mutate(naics3 = as.character(naics3)) |>
  filter(str_detect(naics3, "^\\d{3}$")) |>
  select(naics3, description, all_of(week_cols)) |>
  pivot_longer(cols = all_of(week_cols),
               names_to = "yw_str", values_to = "ba") |>
  mutate(
    year = as.integer(str_extract(yw_str, "^\\d{4}")),
    week = as.integer(str_extract(yw_str, "\\d{1,2}$")),
    date = yw_to_date(year, week),
    ba   = suppressWarnings(as.numeric(ba))
  ) |>
  filter(!is.na(ba), !is.na(date))

message("NAICS-3 panel rows: ", nrow(bfs3))
message("Distinct 3-digit sectors: ", n_distinct(bfs3$naics3))
message("Date range: ", min(bfs3$date), " to ", max(bfs3$date))

# ---------------------------------------------------------------------------
# 3. AI exposure at 3-digit NAICS
#    Reuses files downloaded by 03_ai_exposure.R; downloads them if absent.
# ---------------------------------------------------------------------------
aioe_path <- file.path(ROOT, "Data", "Input", "AIOE_DataAppendix.xlsx")
oes_dir   <- file.path(ROOT, "Data", "Input", "oes_4dig_naics")
oes_zip   <- file.path(ROOT, "Data", "Input", "oes_4dig_naics.zip")

if (!file.exists(aioe_path)) {
  message("Downloading AIOE Data Appendix...")
  GET(paste0("https://raw.githubusercontent.com/AIOE-Data/AIOE/",
             "main/AIOE_DataAppendix.xlsx"),
      write_disk(aioe_path, overwrite = TRUE),
      progress(), timeout(120))
}
if (!dir.exists(oes_dir)) {
  if (!file.exists(oes_zip)) {
    message("Downloading OES 4-digit NAICS employment...")
    GET(paste0("https://raw.githubusercontent.com/AIOE-Data/AIOE/",
               "main/Input/oes_4dig_naics.zip"),
        write_disk(oes_zip, overwrite = TRUE),
        progress(), timeout(120))
  }
  unzip(oes_zip, exdir = oes_dir)
}

# 3a. AIIE by 4-digit NAICS
sheets    <- excel_sheets(aioe_path)
sheet_use <- sheets[str_detect(tolower(sheets), "appendix.?b|aiie|industry")][1]
if (is.na(sheet_use)) sheet_use <- sheets[2]

aiie_raw <- read_excel(aioe_path, sheet = sheet_use)

naics_col_a <- names(aiie_raw)[str_detect(tolower(names(aiie_raw)),
                                          "naics|ind_?code|industry.?code")][1]
aiie_col    <- names(aiie_raw)[str_detect(tolower(names(aiie_raw)),
                                          "aiie|aioe|exposure|ai.?score")][1]
if (is.na(naics_col_a)) naics_col_a <- names(aiie_raw)[1]
if (is.na(aiie_col)) {
  num_cols <- names(aiie_raw)[sapply(aiie_raw, is.numeric)]
  aiie_col <- tail(num_cols, 1)
}

aiie4 <- aiie_raw |>
  rename(naics_raw = all_of(naics_col_a), aiie = all_of(aiie_col)) |>
  mutate(
    naics_raw = as.character(naics_raw),
    naics_num = str_extract(naics_raw, "^\\d+"),
    naics4    = str_pad(str_extract(naics_num, "^\\d{1,4}"), 4, pad = "0"),
    naics3    = str_pad(substr(naics_num, 1, 3), 3, pad = "0"),
    aiie      = as.numeric(aiie)
  ) |>
  filter(!is.na(aiie), str_detect(naics4, "^\\d{4}$"),
         str_detect(naics3, "^\\d{3}$"))

# 3b. OES 4-digit employment weights
oes_file <- list.files(oes_dir,
                       pattern = "\\.(csv|xlsx?|dta)$",
                       full.names = TRUE, recursive = TRUE)[1]
message("Reading OES file: ", oes_file)

oes_raw <- if (str_detect(oes_file, "\\.csv$")) {
  read_csv(oes_file, show_col_types = FALSE)
} else if (str_detect(oes_file, "\\.dta$")) {
  haven::read_dta(oes_file)
} else {
  read_excel(oes_file)
}

oes4 <- oes_raw |>
  rename_with(tolower) |>
  filter(occ_code == "00-0000") |>
  mutate(
    naics4 = substr(as.character(naics), 1, 4),
    emp    = suppressWarnings(as.numeric(tot_emp))
  ) |>
  filter(!is.na(emp), nchar(naics4) == 4) |>
  group_by(naics4) |>
  summarise(emp = sum(emp, na.rm = TRUE), .groups = "drop")

# 3c. Aggregate 4-digit AIIE to 3-digit using OES weights
aiie3 <- aiie4 |>
  left_join(oes4, by = "naics4") |>
  group_by(naics3) |>
  summarise(
    aioe = if (any(!is.na(emp)))
             weighted.mean(aiie, w = replace_na(emp, 0), na.rm = TRUE)
           else
             mean(aiie, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(aioe)) |>
  mutate(aioe_norm = (aioe - min(aioe)) / (max(aioe) - min(aioe))) |>
  arrange(desc(aioe))

message("3-digit AIIE constructed for ", nrow(aiie3), " sectors.")

# ---------------------------------------------------------------------------
# 4. Merge BFS-3 with AIIE-3
# ---------------------------------------------------------------------------
df <- bfs3 |>
  inner_join(aiie3 |> select(naics3, aioe, aioe_norm), by = "naics3")

message("Merged rows: ", nrow(df))
message("Sectors kept: ", n_distinct(df$naics3))

# ---------------------------------------------------------------------------
# 5. Scatter: AIOE vs. % change in BA
#    Pre:  all weeks in December 2022
#    Post: all weeks in December 2025
# ---------------------------------------------------------------------------
pre_df <- df |>
  filter(format(date, "%Y-%m") == "2022-11", ba > 0) |>
  group_by(naics3, aioe_norm, description) |>
  summarise(ba_pre = mean(ba, na.rm = TRUE), .groups = "drop") |>
  mutate(label = str_trunc(description, 30))

post_df <- df |>
  filter(format(date, "%Y-%m") == "2025-11", ba > 0) |>
  group_by(naics3) |>
  summarise(ba_post = mean(ba, na.rm = TRUE), .groups = "drop")

scatter_df <- pre_df |>
  inner_join(post_df, by = "naics3") |>
  mutate(pct_change = 100 * (ba_post - ba_pre) / ba_pre)

message("Scatter sectors: ", nrow(scatter_df))

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 11)
  )

fig_scatter <- ggplot(scatter_df, aes(x = aioe_norm, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_smooth(aes(colour = "OLS", fill = "OLS"),
              method = "lm", se = TRUE, alpha = 0.12, linewidth = 0.8) +
  geom_smooth(aes(weight = ba_pre, colour = "WLS", fill = "WLS"),
              method = "lm", se = TRUE, alpha = 0.12, linewidth = 0.8) +
  geom_point(aes(size = ba_pre), colour = "grey30", alpha = 0.7) +
  geom_text_repel(aes(label = label), size = 2.2, colour = "grey25",
                  max.overlaps = Inf, segment.size = 0.3,
                  segment.colour = "grey60", box.padding = 0.3) +
  scale_colour_manual(name = NULL,
                      values = c("OLS" = "steelblue", "WLS" = "tomato")) +
  scale_fill_manual(name = NULL,
                    values = c("OLS" = "steelblue", "WLS" = "tomato")) +
  scale_size_continuous(name = "Pre-period BA", labels = scales::comma,
                        range = c(0.5, 6)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", ".25", ".5", ".75", "1")) +
  labs(
    title    = "AI Exposure vs. Change in Weekly Business Applications (3-digit NAICS)",
    subtitle = paste0("November 2022 vs. November 2025. N = ",
                      nrow(scatter_df), " sectors."),
    x        = "AI Industry Exposure (normalised)",
    y        = "% change in weekly business applications",
    caption  = "Sources: Census BFS; Felten, Raj & Seamans (2021)."
  ) +
  theme_paper +
  theme(
    plot.subtitle = element_text(size = 8, colour = "grey40"),
    plot.caption  = element_text(size = 7, hjust = 0, colour = "grey40")
  )

ggsave(file.path(ROOT, "Draft", "fig_scatter_naics3.pdf"), fig_scatter,
       width = 10, height = 7, device = "pdf")
message("Saved: Draft/fig_scatter_naics3.pdf")

# ---------------------------------------------------------------------------
# 6. Companion regression table (OLS + WLS weighted by pre-period mean BA)
# ---------------------------------------------------------------------------
scatter_weights <- pre_df |>
  select(naics3, weight = ba_pre)

scatter_reg_df <- scatter_df |> left_join(scatter_weights, by = "naics3")
scatter_ols    <- lm(pct_change ~ aioe_norm, data = scatter_reg_df)
scatter_wls    <- lm(pct_change ~ aioe_norm, data = scatter_reg_df,
                     weights = weight)

modelsummary(
  list("OLS" = scatter_ols, "WLS" = scatter_wls),
  output   = file.path(ROOT, "Draft", "tab_scatter_naics3.tex"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "aioe_norm"   = "AI Exposure (AIOE, normalised)",
    "(Intercept)" = "Constant"
  ),
  gof_map = list(
    list(raw = "nobs",      clean = "Sectors (3-digit)", fmt = 0),
    list(raw = "r.squared", clean = "R$^2$",             fmt = 3)
  ),
  title  = "Cross-Sectional Regression: AI Exposure vs.\\ Change in Weekly Applications (3-digit NAICS)",
  notes  = paste(
    "OLS and WLS (weighted by pre-period mean weekly BA) at 3-digit NAICS.",
    "Outcome: percentage change in mean weekly business applications",
    "from November 2022 to November 2025."
  ),
  escape = FALSE
)
message("Saved: Draft/tab_scatter_naics3.tex")

message("\n=========================================================")
message("  3-digit NAICS scatter complete.")
message("  fig_scatter_naics3.pdf + tab_scatter_naics3.tex in Draft/")
message("=========================================================")

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

# ---------------------------------------------------------------------------
# 7. Seasonal adjustment — STL per sector on log scale
#    Multiplicative SA: decompose log(BA), remove seasonal component,
#    exponentiate back.  Handles sector-specific seasonal cycles that
#    would otherwise oscillate into the event-study bins.
# ---------------------------------------------------------------------------
library(fixest)
library(broom)

sa_by_sector <- function(ba, freq = 52L) {
  n <- length(ba)
  if (n < 2L * freq) return(rep(NA_real_, n))

  # Log scale with floor to avoid log(0)
  y <- log(pmax(ba, 0.5))

  # Linear interpolation for any NAs mid-series
  if (anyNA(y)) {
    ok  <- which(!is.na(y))
    if (length(ok) < 4L) return(rep(NA_real_, n))
    nas <- which(is.na(y))
    y[nas] <- approx(ok, y[ok], xout = nas, rule = 2L)$y
  }

  fit <- tryCatch(
    stl(ts(y, frequency = freq), s.window = "periodic", robust = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(NA_real_, n))

  # Subtract seasonal component and back-transform
  exp(y - as.numeric(fit$time.series[, "seasonal"]))
}

# Apply per sector (data must be sorted by date within sector)
df_sa <- df |>
  arrange(naics3, date) |>
  group_by(naics3) |>
  mutate(ba_sa = sa_by_sector(ba)) |>
  ungroup()

n_sa_ok <- n_distinct(df_sa$naics3[!is.na(df_sa$ba_sa)])
message("Sectors with SA series: ", n_sa_ok)

# ---------------------------------------------------------------------------
# 8. Event study on SA weekly data, 4-week bins, year-week FE
# ---------------------------------------------------------------------------

# Sector weight = pre-ChatGPT mean weekly BA_SA
sector_weights <- df_sa |>
  filter(!is.na(ba_sa), ba_sa > 0, date < CHATGPT_WEEK) |>
  group_by(naics3) |>
  summarise(w = mean(ba_sa, na.rm = TRUE), .groups = "drop")

df_reg <- df_sa |>
  filter(!is.na(ba_sa), ba_sa > 0) |>
  mutate(
    log_ba  = log(ba_sa),
    event_t = as.integer((date - CHATGPT_WEEK) / 7),
    yw      = as.integer(format(date, "%G%V"))
  ) |>
  left_join(sector_weights, by = "naics3")

message("Panel rows: ", nrow(df_reg))
message("Pre-ChatGPT obs: ",  sum(df_reg$event_t <  0))
message("Post-ChatGPT obs: ", sum(df_reg$event_t >= 0))

BIN_WEEKS <- 4L
ES_PRE_W  <- -156L
ES_POST_W <-  156L
REF_BIN   <- -BIN_WEEKS

df_es <- df_reg |>
  filter(between(event_t, ES_PRE_W, ES_POST_W)) |>
  mutate(
    event_bin = floor(event_t / BIN_WEEKS) * BIN_WEEKS,
    event_bin = relevel(factor(event_bin), ref = as.character(REF_BIN))
  )

es_model <- feols(
  log_ba ~ i(event_bin, aioe_norm, ref = as.character(REF_BIN)) | naics3 + yw,
  data    = df_es,
  weights = ~w,
  cluster = ~naics3
)

print(summary(es_model))

# Tidy coefficients
es_tidy <- tidy(es_model, conf.int = TRUE) |>
  filter(str_detect(term, "event_bin")) |>
  mutate(t = as.numeric(str_extract(term, "-?\\d+")))

block_to_date <- function(t_val) CHATGPT_WEEK + weeks(as.integer(t_val))

es_df <- es_tidy |>
  select(t, coef = estimate, ci_lo = conf.low, ci_hi = conf.high,
         pval = p.value) |>
  bind_rows(tibble(t = REF_BIN * 1.0, coef = 0, ci_lo = 0, ci_hi = 0,
                   pval = NA_real_)) |>
  arrange(t) |>
  mutate(
    date = block_to_date(t),
    sig  = case_when(
      is.na(pval) ~ "Reference",
      pval < 0.05 ~ "p < 0.05",
      pval < 0.10 ~ "p < 0.10",
      TRUE        ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.05", "p < 0.10", "n.s.", "Reference"))
  )

# ---------------------------------------------------------------------------
# 9. Event-study plot
# ---------------------------------------------------------------------------
sig_colours <- c(
  "p < 0.05"  = "#2ca02c",
  "p < 0.10"  = "#98df8a",
  "n.s."      = "#d62728",
  "Reference" = "grey50"
)

fig_es_naics3 <- ggplot(es_df, aes(x = date, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = CHATGPT_WEEK, linetype = "dashed",
             colour = "black", linewidth = 0.4) +
  annotate("text", x = CHATGPT_WEEK + 10, y = Inf,
           label = "ChatGPT", vjust = 1.4, hjust = 0, size = 3,
           colour = "grey30") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "grey70") +
  geom_line(colour = "grey40", linewidth = 0.6) +
  geom_point(aes(colour = sig), size = 1.8) +
  scale_colour_manual(values = sig_colours, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Event Study: AI Exposure \u00d7 Post on log(SA Weekly BA) \u2014 3-digit NAICS",
    subtitle = paste0("4-week bins; sector + year-week FE; STL-SA per sector; ",
                      "weighted by pre-period mean BA; SEs clustered by NAICS-3."),
    x = NULL, y = "Coefficient on AI Exposure \u00d7 Event Bin"
  ) +
  theme_paper +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"))

ggsave(file.path(ROOT, "Draft", "fig_event_study_naics3.pdf"), fig_es_naics3,
       width = 8, height = 4.5, device = "pdf")
message("Saved: Draft/fig_event_study_naics3.pdf")

# ---------------------------------------------------------------------------
# 10. Event-study regression table
# ---------------------------------------------------------------------------
es_coef_names <- names(coef(es_model))
es_coef_map   <- setNames(
  sapply(es_coef_names, function(nm) {
    t_val    <- as.integer(str_extract(nm, "-?\\d+"))
    cal_date <- block_to_date(t_val)
    paste0(format(cal_date, "%b %Y"), " ($t = ", t_val, "$w)")
  }),
  es_coef_names
)

modelsummary(
  list("Event Study (3-digit NAICS, 4w blocks)" = es_model),
  output   = file.path(ROOT, "Draft", "tab_event_study_naics3.tex"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = es_coef_map,
  gof_map  = list(
    list(raw = "nobs",       clean = "Observations", fmt = 0),
    list(raw = "r.squared",  clean = "R$^2$",        fmt = 3),
    list(raw = "FE: naics3", clean = "Sector FE",    fmt = 0),
    list(raw = "FE: yw",     clean = "Year-Week FE", fmt = 0)
  ),
  title  = "Event-Study Estimates \\textemdash\\ 3-digit NAICS, SA Weekly, 4-week blocks",
  notes  = paste(
    "Outcome: log(seasonally adjusted weekly business applications).",
    "Weekly BA seasonally adjusted per sector using STL decomposition",
    "(multiplicative, log scale, periodic window).",
    "Event time in weeks relative to ChatGPT launch (28~Nov~2022),",
    "binned into 4-week blocks. Reference block: $t \\in [-4, -1]$.",
    "Observations weighted by sector pre-period mean weekly BA.",
    "Sector + ISO year-week fixed effects. SEs clustered by NAICS-3."
  ),
  escape = FALSE
)
message("Saved: Draft/tab_event_study_naics3.tex")

message("Saved: Draft/tab_event_study_naics3.tex")

message("\n=========================================================")
message("  3-digit NAICS analysis complete.")
message("  Outputs in Draft/ with _naics3 suffix.")
message("=========================================================")

if (FALSE) {
# ---------------------------------------------------------------------------
# 11. Faceted event study — all available 3-digit NAICS series
#     Mirrors fig_es_robust.pdf from the 2-digit analysis.
#     Census publishes BA at NAICS-3 weekly. HBA and WBA may also exist as
#     separate files — try to download them, skip gracefully if absent.
# ---------------------------------------------------------------------------

# 11a. Helper: download a wide naics3 series CSV, parse → long data frame.
#      Returns NULL (with a message) if the file doesn't exist on Census servers.
read_naics3_series <- function(col_name, url_suffix) {
  url      <- paste0("https://www.census.gov/econ/bfs/csv/", url_suffix, ".csv")
  tmp_path <- file.path(ROOT, "Data", "Input",
                        paste0("bfs_", url_suffix, ".csv"))

  if (!file.exists(tmp_path)) {
    resp <- tryCatch(
      GET(url, write_disk(tmp_path, overwrite = TRUE), timeout(60)),
      error = function(e) NULL
    )
    if (is.null(resp) || http_error(resp)) {
      suppressWarnings(try(file.remove(tmp_path), silent = TRUE))
      message("  ", col_name, ": not available at Census (", url, ")")
      return(NULL)
    }
    message("  ", col_name, ": downloaded from ", url)
  } else {
    message("  ", col_name, ": using cached file")
  }

  raw_s <- tryCatch(read_csv(tmp_path, show_col_types = FALSE),
                    error = function(e) NULL)
  if (is.null(raw_s)) return(NULL)

  naics_c <- names(raw_s)[str_detect(tolower(names(raw_s)), "^naics")][1]
  wk_cols <- names(raw_s)[str_detect(names(raw_s), "^\\d{4}w\\d{1,2}$")]
  if (is.na(naics_c) || length(wk_cols) == 0) {
    message("  ", col_name, ": unexpected file format — skipping")
    return(NULL)
  }

  raw_s |>
    rename(naics3 = all_of(naics_c)) |>
    mutate(naics3 = as.character(naics3)) |>
    filter(str_detect(naics3, "^\\d{3}$")) |>
    select(naics3, all_of(wk_cols)) |>
    pivot_longer(cols      = all_of(wk_cols),
                 names_to  = "yw_str",
                 values_to = col_name) |>
    mutate(
      year          = as.integer(str_extract(yw_str, "^\\d{4}")),
      week          = as.integer(str_extract(yw_str, "\\d{1,2}$")),
      date          = yw_to_date(year, week),
      !!col_name   := suppressWarnings(as.numeric(.data[[col_name]]))
    ) |>
    filter(!is.na(.data[[col_name]]), !is.na(date)) |>
    select(naics3, date, all_of(col_name))
}

# 11b. Series catalogue — add rows here if Census releases more NAICS-3 files
series_meta3 <- tribble(
  ~col,   ~url_suffix,   ~label,
  "ba",   "naics3",      "Business Applications (BA)",
  "hba",  "hba_naics3",  "High-Propensity Apps (HBA)",
  "wba",  "wba_naics3",  "Apps w/ Planned Wages (WBA)",
  "cba",  "cba_naics3",  "Corrected Applications (CBA)"
)

# 11c. Build multi-series panel: SA each series per sector, join together
message("\nDownloading / checking NAICS-3 series:")
panel_list <- list()

for (i in seq_len(nrow(series_meta3))) {
  col <- series_meta3$col[i]
  sfx <- series_meta3$url_suffix[i]

  raw_long <- read_naics3_series(col, sfx)
  if (is.null(raw_long)) next

  # Seasonal adjustment per sector on log scale (same as ba_sa in section 7)
  sa_long <- raw_long |>
    arrange(naics3, date) |>
    group_by(naics3) |>
    mutate(!!paste0(col, "_sa") := sa_by_sector(.data[[col]])) |>
    ungroup() |>
    select(naics3, date, all_of(paste0(col, "_sa")))

  panel_list[[col]] <- sa_long
  message("  ", col, "_sa ready: ", nrow(sa_long), " rows")
}

# Merge all available series onto the base df_reg panel
df_multi <- df_reg |>
  select(naics3, aioe_norm, date, log_ba, event_t, yw, w)

for (col in names(panel_list)) {
  sa_col <- paste0(col, "_sa")
  df_multi <- df_multi |>
    left_join(panel_list[[col]], by = c("naics3", "date")) |>
    mutate(!!paste0("log_", col) := log(pmax(.data[[sa_col]], 0.01))) |>
    select(-all_of(sa_col))
}

message("Multi-series panel cols: ", paste(names(df_multi), collapse = ", "))

# 11d. Helper: run event study for one log-outcome column
run_es_naics3 <- function(log_col, data) {
  df_run <- data |>
    filter(!is.na(.data[[log_col]]),
           is.finite(.data[[log_col]]),
           between(event_t, ES_PRE_W, ES_POST_W)) |>
    mutate(
      event_bin = floor(event_t / BIN_WEEKS) * BIN_WEEKS,
      event_bin = relevel(factor(event_bin), ref = as.character(REF_BIN))
    )

  if (nrow(df_run) < 100 || n_distinct(df_run$naics3) < 5) {
    message("  Skipping ", log_col, " — too few rows/sectors")
    return(NULL)
  }

  mod <- tryCatch(
    feols(
      reformulate(
        paste0("i(event_bin, aioe_norm, ref = '", REF_BIN, "')"),
        response = log_col
      ),
      fixef   = c("naics3", "yw"),
      data    = df_run,
      weights = ~w,
      cluster = ~naics3
    ),
    error = function(e) {
      message("  ", log_col, " failed: ", e$message)
      NULL
    }
  )
  if (is.null(mod)) return(NULL)

  tidy(mod, conf.int = TRUE) |>
    filter(str_detect(term, "event_bin")) |>
    mutate(t = as.numeric(str_extract(term, "-?\\d+"))) |>
    select(t, coef = estimate, ci_lo = conf.low, ci_hi = conf.high,
           pval = p.value) |>
    bind_rows(tibble(t = REF_BIN * 1.0, coef = 0, ci_lo = 0, ci_hi = 0,
                     pval = NA_real_)) |>
    arrange(t) |>
    mutate(
      date = block_to_date(t),
      sig  = case_when(
        is.na(pval) ~ "Reference",
        pval < 0.05 ~ "p < 0.05",
        pval < 0.10 ~ "p < 0.10",
        TRUE        ~ "n.s."
      ),
      sig = factor(sig, levels = c("p < 0.05", "p < 0.10", "n.s.", "Reference"))
    )
}

} # end if (FALSE) — section 11 disabled (only BA available at NAICS-3)

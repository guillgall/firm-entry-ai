# =============================================================================
# 04_merge_and_analysis.R
# Merge BFS panel with AI exposure; run DiD and event study; produce outputs
# =============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(ggplot2)
library(broom)
library(here)

ROOT <- here::here()

# ---------------------------------------------------------------------------
# 0. Load data
# ---------------------------------------------------------------------------
bfs <- readRDS(file.path(ROOT, "Data", "Output", "bfs_panel.rds"))
ai  <- readRDS(file.path(ROOT, "Data", "Output", "ai_exposure_naics.rds"))

# Guard: the BFS panel must have NAICS-2 sector rows (not just "US" aggregate)
# for the DiD / event-study to be identified.
# bfs_monthly.csv provides NAICS-level SA columns (BA_SA_11, BA_SA_21, …);
# if 02_clean_bfs.R only found a national total the merge below will be empty.
bfs_sectors <- setdiff(unique(bfs$naics2), "US")
if (length(bfs_sectors) == 0) {
  stop(
    "The BFS panel contains only the national aggregate (naics2 == 'US').\n",
    "The Census monthly SA file (bfs_monthly.csv) may not include NAICS-level\n",
    "columns (BA_SA_11, BA_SA_21, …).  Options:\n",
    "  1. Check the actual column names printed by 02_clean_bfs.R and update\n",
    "     the SA column regex in that script.\n",
    "  2. Fall back to the weekly NAICS-2 file (naics2.csv) for sector data."
  )
}
message("BFS sectors available: ", paste(sort(bfs_sectors), collapse = ", "))

# ---------------------------------------------------------------------------
# 1. Merge
# ---------------------------------------------------------------------------
df <- bfs |>
  filter(naics2 != "US") |>   # keep sector rows only for the regression panel
  inner_join(ai |> select(naics2, aioe, aioe_norm, aioe_quartile),
             by = "naics2") |>
  mutate(
    # ChatGPT public launch: November 30, 2022
    post      = as.integer(date >= as.Date("2022-11-01")),
    # Numeric time index for event study (months relative to Nov 2022)
    event_t   = interval(as.Date("2022-11-01"), date) %/% months(1),
    # Panel identifiers
    naics2    = factor(naics2),
    ym        = as.integer(format(date, "%Y%m"))  # YYYYMM integer for FE
  )

message("Merged rows: ", nrow(df))
message("Sectors: ",     n_distinct(df$naics2))
message("Date range: ",  min(df$date), " to ", max(df$date))

# ---------------------------------------------------------------------------
# 2. DiD: BA_it = alpha_i + gamma_t + beta*(post * aioe_norm) + eps
# ---------------------------------------------------------------------------
# Use log(ba) to reduce skewness; drop observations with ba == 0
df_reg <- df |>
  filter(ba > 0) |>
  mutate(log_ba = log(ba))

did_main <- feols(
  log_ba ~ post:aioe_norm | naics2 + ym,
  data    = df_reg,
  cluster = ~naics2
)

# Indexed BA (ba_idx) as alternative outcome
did_idx <- feols(
  ba_idx ~ post:aioe_norm | naics2 + ym,
  data    = df_reg |> filter(!is.na(ba_idx)),
  cluster = ~naics2
)

# Raw BA
did_raw <- feols(
  ba ~ post:aioe_norm | naics2 + ym,
  data    = df_reg,
  cluster = ~naics2
)

print(summary(did_main))

# ---------------------------------------------------------------------------
# 2b. Marginal effects table: implied % change at selected AIOE percentiles
# ---------------------------------------------------------------------------
beta_main <- coef(did_main)[["post:aioe_norm"]]

aioe_pcts <- quantile(ai$aioe_norm, c(0.10, 0.25, 0.50, 0.75, 0.90),
                      na.rm = TRUE)

# Reference: P25 sector; implied effect vs. P25 at each percentile
mfx_df <- tibble(
  percentile  = c("P10", "P25", "P50", "P75", "P90"),
  aioe_norm   = aioe_pcts,
  delta_vs_p25 = (aioe_pcts - aioe_pcts["25%"]) * beta_main * 100
)

message("\nMarginal effects table:")
print(mfx_df)

# Export as LaTeX
mfx_rows <- mfx_df |>
  mutate(row_tex = sprintf(
    "  %s & %.3f & %+.1f\\%%  \\\\",
    percentile, aioe_norm, delta_vs_p25
  )) |>
  pull(row_tex)

tex_mfx <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Implied Effect of AI Exposure on Business Applications}",
  "\\label{tab:mfx}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "  Percentile & AIOE (normalised) & Effect vs.\ P25 sector (\\%) \\\\",
  "\\midrule",
  mfx_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("  \\item \\textit{Notes:} Implied percentage difference in business",
         " applications in the post-ChatGPT period relative to a sector at the",
         " 25th percentile of normalised AI Industry Exposure (AIOE).",
         " Computed as $(\\text{AIOE}_p - \\text{AIOE}_{P25}) \\times \\hat{\\beta} \\times 100$,",
         " where $\\hat{\\beta}$ is the DiD estimate from column~(1) of",
         " Table~\\ref{tab:did}."),
  "\\end{tablenotes}",
  "\\end{table}"
)

writeLines(tex_mfx, file.path(ROOT, "Draft", "tab_mfx.tex"))
message("Saved: Draft/tab_mfx.tex")

# ---------------------------------------------------------------------------
# 3. Event study: dynamic effects relative to Nov 2022
# ---------------------------------------------------------------------------
# Bin event-time: cap at pre=-36 / post=42 (covers data through ~May 2026)
df_es <- df_reg |>
  filter(between(event_t, -36, 42)) |>
  mutate(
    event_bin = case_when(
      event_t <= -36 ~ -36L,
      event_t >=  42 ~  42L,
      TRUE           ~ as.integer(event_t)
    ),
    event_bin = relevel(factor(event_bin), ref = "-1")
  )

es_model <- feols(
  log_ba ~ i(event_bin, aioe_norm, ref = "-1") | naics2 + ym,
  data    = df_es,
  cluster = ~naics2
)

# Extract event-study coefficients via broom::tidy
es_tidy <- tidy(es_model, conf.int = TRUE) |>
  filter(str_detect(term, "event_bin")) |>
  mutate(
    t = as.numeric(str_extract(term, "-?\\d+"))
  )

es_df <- tibble(
  t     = es_tidy$t,
  coef  = es_tidy$estimate,
  ci_lo = es_tidy$conf.low,
  ci_hi = es_tidy$conf.high,
  pval  = es_tidy$p.value
) |>
  # Add the omitted reference period (t = -1, coef = 0)
  bind_rows(tibble(t = -1, coef = 0, ci_lo = 0, ci_hi = 0, pval = NA_real_)) |>
  arrange(t) |>
  mutate(
    sig = case_when(
      is.na(pval) ~ "Reference",
      pval < 0.05 ~ "p < 0.05",
      pval < 0.10 ~ "p < 0.10",
      TRUE        ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.05", "p < 0.10", "n.s.", "Reference"))
  )

# ---------------------------------------------------------------------------
# 3b. Timing of statistical significance
# ---------------------------------------------------------------------------
# Identify the first post-launch month at which the effect becomes statistically
# significant, both in isolation and persistently (all subsequent months also sig).

es_timing <- es_tidy |>
  mutate(
    sig_10 = p.value < 0.10,
    sig_05 = p.value < 0.05
  ) |>
  filter(t >= 0) |>
  arrange(t)

# First post-launch month significant at each level
first_sig_10 <- es_timing |> filter(sig_10) |> slice(1)
first_sig_05 <- es_timing |> filter(sig_05) |> slice(1)

# Persistent onset: first t such that all subsequent months are also sig at 5%
# (exclude the endpoint bin, which may be sparse due to sample truncation)
max_t <- max(es_timing$t)

persistent_onset_05 <- es_timing |>
  filter(t < max_t) |>
  mutate(
    all_sig_from_here = map_lgl(t, function(t0) {
      all(es_timing$sig_05[es_timing$t >= t0 & es_timing$t < max_t])
    })
  ) |>
  filter(all_sig_from_here) |>
  slice(1)

event_to_date <- function(t_val) {
  as.Date("2022-11-01") %m+% months(as.integer(t_val))
}

message("\n--- Timing of statistical significance (post-ChatGPT) ---")
if (nrow(first_sig_10) > 0)
  message("First significant at p<0.10:  t = ", first_sig_10$t,
          "  (", format(event_to_date(first_sig_10$t), "%B %Y"), ")",
          "  coef = ", round(first_sig_10$estimate, 3),
          "  p = ", round(first_sig_10$p.value, 3))
if (nrow(first_sig_05) > 0)
  message("First significant at p<0.05:  t = ", first_sig_05$t,
          "  (", format(event_to_date(first_sig_05$t), "%B %Y"), ")",
          "  coef = ", round(first_sig_05$estimate, 3),
          "  p = ", round(first_sig_05$p.value, 3))
if (nrow(persistent_onset_05) > 0)
  message("Persistent onset (p<0.05):    t = ", persistent_onset_05$t,
          "  (", format(event_to_date(persistent_onset_05$t), "%B %Y"), ")")
message("---------------------------------------------------------\n")

# ---------------------------------------------------------------------------
# 4. Figures
# ---------------------------------------------------------------------------

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    plot.title        = element_text(face = "bold", size = 11)
  )

chatgpt_date  <- as.Date("2022-11-01")

# Shared x-axis scale for all event-study plots: actual month-year labels
es_t_breaks <- seq(-36, 42, by = 12)
es_t_labels <- format(as.Date("2022-11-01") %m+% months(es_t_breaks), "%b %Y")
scale_x_es  <- scale_x_continuous(breaks = es_t_breaks, labels = es_t_labels)
agentic_date  <- as.Date("2025-05-01")   # agentic AI tools (t ≈ +30 from ChatGPT)

## 4a. Sector spotlight: Professional Services vs Manufacturing vs Construction ----
# Mirror the FRED motivating chart. Manufacturing = sum of NAICS 31, 32, 33.
# Uses seasonally-adjusted ba values from the monthly SA file.
# Aggregate SA counts across sub-sectors, then re-index to Nov 2022 = 100.
reindex <- function(df) {
  base <- df |> filter(date == as.Date("2022-11-01")) |> pull(ba)
  if (length(base) == 0 || is.na(base) || base == 0) return(df |> mutate(ba_idx = NA_real_))
  df |> mutate(ba_idx = 100 * ba / base)
}

# Use sector rows only (exclude the "US" aggregate added by 02_clean_bfs.R)
bfs_sectors_df <- bfs |> filter(naics2 != "US")

spotlight_sectors <- bfs_sectors_df |>
  filter(date >= as.Date("2016-01-01")) |>
  mutate(
    sector_label = case_when(
      naics2 == "54"  ~ "Professional Services (NAICS 54)",
      naics2 == "23"  ~ "Construction (NAICS 23)",
      TRUE            ~ NA_character_
    )
  ) |>
  filter(!is.na(sector_label)) |>
  group_by(date, sector_label) |>
  summarise(ba = sum(ba, na.rm = TRUE), .groups = "drop") |>
  group_by(sector_label) |>
  group_modify(~ reindex(.x)) |>
  ungroup()

# Total: prefer the pre-built "US" aggregate row (already SA); fall back to sum
if ("US" %in% bfs$naics2) {
  total_line <- bfs |>
    filter(naics2 == "US", date >= as.Date("2016-01-01")) |>
    reindex() |>
    mutate(sector_label = "Total (all NAICS)")
} else {
  total_line <- bfs_sectors_df |>
    filter(date >= as.Date("2016-01-01")) |>
    group_by(date) |>
    summarise(ba = sum(ba, na.rm = TRUE), .groups = "drop") |>
    reindex() |>
    mutate(sector_label = "Total (all NAICS)")
}

spotlight_df <- bind_rows(spotlight_sectors, total_line) |>
  mutate(
    sector_label = factor(sector_label, levels = c(
      "Professional Services (NAICS 54)",
      "Total (all NAICS)",
      "Construction (NAICS 23)"
    ))
  ) |>
  arrange(sector_label, date)

# Colours approximate FRED: blue, red, teal, black
sector_colours <- c(
  "Professional Services (NAICS 54)"  = "#1f77b4",
  "Total (all NAICS)"                 = "#d62728",
  "Construction (NAICS 23)"           = "#111111"
)

fig_spotlight <- ggplot(spotlight_df,
                        aes(x = date, y = ba_idx,
                            colour = sector_label,
                            linewidth = sector_label)) +
  geom_line() +
  geom_vline(xintercept = chatgpt_date, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  annotate("text", x = chatgpt_date + 45, y = Inf,
           label = "ChatGPT\nlaunch", vjust = 1.4, hjust = 0, size = 2.8,
           colour = "grey30") +
  scale_colour_manual(values = sector_colours, name = NULL) +
  scale_linewidth_manual(
    values = c(
      "Professional Services (NAICS 54)" = 0.9,
      "Total (all NAICS)"                = 0.9,
      "Construction (NAICS 23)"          = 0.9
    ), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title   = "Business Applications by Sector (Nov 2022 = 100)",
    x       = NULL,
    y       = "Index (Nov 2022 = 100)",
    caption = "Source: Census Bureau Business Formation Statistics."
  ) +
  theme_paper +
  theme(
    legend.text    = element_text(size = 8),
    plot.caption   = element_text(size = 7, hjust = 0, colour = "grey40")
  )

ggsave(file.path(ROOT, "Draft", "fig_spotlight.pdf"), fig_spotlight,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_spotlight.pdf")

## 4b. Descriptive: indexed BA by sector, coloured by AI quartile -----
fig_desc <- df |>
  filter(!is.na(ba_idx), !is.na(aioe_quartile)) |>
  mutate(
    quartile_label = factor(aioe_quartile,
                            labels = c("Q1 (Low AI)", "Q2", "Q3",
                                       "Q4 (High AI)"))
  ) |>
  group_by(date, quartile_label) |>
  summarise(ba_idx_mean = mean(ba_idx, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = date, y = ba_idx_mean, colour = quartile_label)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = chatgpt_date, linetype = "dashed",
             colour = "black", linewidth = 0.6) +
  annotate("text", x = chatgpt_date + 30, y = Inf,
           label = "ChatGPT\nlaunch", vjust = 1.3, hjust = 0, size = 3) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1,
                      name = "AI Exposure Quartile") +
  labs(
    title   = "Business Applications by AI Exposure Quartile (Jan 2020 = 100)",
    x       = NULL,
    y       = "Index (Jan 2020 = 100)"
  ) +
  theme_paper

ggsave(file.path(ROOT, "Draft", "fig_desc.pdf"), fig_desc,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_desc.pdf")

## 4b. Event-study plot -----------------------------------------------
sig_colours <- c(
  "p < 0.05"  = "#2ca02c",   # green
  "p < 0.10"  = "#98df8a",   # light green
  "n.s."      = "#d62728",   # red
  "Reference" = "grey50"
)

fig_es <- ggplot(es_df, aes(x = t, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "grey70") +
  geom_line(colour = "grey40", linewidth = 0.6) +
  geom_point(aes(colour = sig), size = 2) +
  scale_colour_manual(values = sig_colours, name = NULL) +
  scale_x_es +
  labs(
    title = "Event Study: Effect of AI Exposure on log(Business Applications)",
    x     = NULL,
    y     = "Coefficient on AI Exposure"
  ) +
  theme_paper +
  theme(legend.position = "bottom")

ggsave(file.path(ROOT, "Draft", "fig_event_study.pdf"), fig_es,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_event_study.pdf")

## 4c. Faceted robustness event-study: BA, HBA, WBA, BF4Q ----------------
# Helper: run event-study feols for a given log-outcome column and return
# a tidy tibble of (t, coef, ci_lo, ci_hi) ready for ggplot.
run_es <- function(outcome_col, data, use_log = TRUE) {
  df_run <- data |>
    filter(!is.na(.data[[outcome_col]]), .data[[outcome_col]] > 0) |>
    mutate(log_outcome = if (use_log) log(.data[[outcome_col]])
                         else .data[[outcome_col]]) |>
    filter(between(event_t, -36, 42)) |>
    mutate(
      event_bin = case_when(
        event_t <= -36 ~ -36L,
        event_t >=  42 ~  42L,
        TRUE           ~ as.integer(event_t)
      )
    )

  if (nrow(df_run) < 50) {
    message("  Skipping ", outcome_col, " – too few rows")
    return(NULL)
  }

  # Reference period: prefer t = -1; fall back to last available pre-period
  # (BF8Q lacks recent observations due to the 8-quarter follow-up window)
  all_bins <- sort(unique(df_run$event_bin))
  pre_bins <- all_bins[all_bins < 0]
  if (length(pre_bins) == 0) {
    message("  Skipping ", outcome_col, " – no pre-period observations")
    return(NULL)
  }
  ref_bin <- if (-1L %in% all_bins) -1L else max(pre_bins)
  message("  ", outcome_col, ": reference period t = ", ref_bin)

  df_run <- df_run |>
    mutate(event_bin = relevel(factor(event_bin), ref = as.character(ref_bin)))

  mod <- tryCatch(
    feols(log_outcome ~ i(event_bin, aioe_norm, ref = as.character(ref_bin)) |
            naics2 + ym,
          data = df_run, cluster = ~naics2),
    error = function(e) { message("  Error in ", outcome_col, ": ", e$message); NULL }
  )
  if (is.null(mod)) return(NULL)

  out <- tidy(mod, conf.int = TRUE) |>
    filter(str_detect(term, "event_bin")) |>
    mutate(t = as.numeric(str_extract(term, "-?\\d+"))) |>
    select(t, coef = estimate, ci_lo = conf.low, ci_hi = conf.high, pval = p.value) |>
    bind_rows(tibble(t = ref_bin * 1.0, coef = 0, ci_lo = 0, ci_hi = 0, pval = NA_real_)) |>
    arrange(t) |>
    mutate(
      sig = case_when(
        is.na(pval) ~ "Reference",
        pval < 0.05 ~ "p < 0.05",
        pval < 0.10 ~ "p < 0.10",
        TRUE        ~ "n.s."
      ),
      sig = factor(sig, levels = c("p < 0.05", "p < 0.10", "n.s.", "Reference"))
    )

  out
}

# Labels for facet strips
series_meta <- tribble(
  ~col,     ~label,                                          ~use_log,
  "ba",     "Business Applications (BA)",                    TRUE,
  "cba",    "Corrected Applications (CBA)",                  TRUE,
  "hba",    "High-Propensity Applications (HBA)",            TRUE,
  "wba",    "Applications w/ Planned Wages (WBA)",           TRUE,
  "sbf4q",  "SA Formations \u22644Q (SBF4Q)",                TRUE,
  "sbf8q",  "SA Formations \u22648Q (SBF8Q)",                TRUE
)

# Use the merged df (df_reg already has naics2, ym, event_t, aioe_norm)
es_facet_list <- series_meta |>
  filter(col %in% names(df_reg)) |>
  mutate(data = map2(col, use_log, \(c, l) run_es(c, df_reg, use_log = l))) |>
  filter(!map_lgl(data, is.null)) |>
  unnest(data) |>
  mutate(label = factor(label, levels = series_meta$label))

if (nrow(es_facet_list) > 0) {
  fig_es_robust <- ggplot(es_facet_list, aes(x = t, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept =  0, linetype = "dashed", colour = "black",
               linewidth = 0.45) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                alpha = 0.15, fill = "grey70") +
    geom_line(colour = "grey40", linewidth = 0.6) +
    geom_point(aes(colour = sig), size = 1.2) +
    scale_colour_manual(values = sig_colours, name = NULL) +
    scale_x_es +
    facet_wrap(~ label, ncol = 3, scales = "free_y") +
    labs(
      title = "Event Study: AI Exposure \u00d7 Post — Robustness across BFS Series",
      subtitle = "Outcome: log(series). Sector + month FE. 95% CI, SEs clustered by NAICS-2.",
      x = NULL,
      y = "Coefficient on AI Exposure"
    ) +
    theme_paper +
    theme(
      strip.background = element_rect(fill = "grey93", colour = NA),
      strip.text       = element_text(size = 8.5, face = "bold"),
      plot.subtitle    = element_text(size = 8, colour = "grey40"),
      legend.position  = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7)
    )

  ggsave(file.path(ROOT, "Draft", "fig_es_robust.pdf"), fig_es_robust,
         width = 10, height = 12, device = "pdf")
  message("Saved: Draft/fig_es_robust.pdf")
} else {
  message("No alternative series available yet for faceted event study.",
          " Re-run after 02_clean_bfs.R update.")
}

# ---------------------------------------------------------------------------
# 5. Regression tables (modelsummary → LaTeX)
# ---------------------------------------------------------------------------

# gof_map: FE rows use fmt=0; the post-processing helper below replaces the
# raw "X" that tabularray emits for logical TRUE with a LaTeX checkmark.
gof_map_sector <- list(
  list(raw = "nobs",       clean = "Observations", fmt = 0),
  list(raw = "r.squared",  clean = "R$^2$",        fmt = 3),
  list(raw = "FE: naics2", clean = "Sector FE",    fmt = 0),
  list(raw = "FE: ym",     clean = "Month FE",     fmt = 0)
)

# Post-processing helper: injects label, sets width=\linewidth, and converts
# the "X" that tinytable/tabularray emits for boolean TRUE → $\checkmark$.
fix_tex_table <- function(path, label) {
  lines <- readLines(path)

  # 1. Inject \label after \caption
  cap_i <- grep("^caption=", lines)
  if (length(cap_i) > 0 && !any(grepl(paste0("label=\\{", label, "\\}"), lines)))
    lines <- append(lines, paste0("label={", label, "},"), after = cap_i[1])

  # 2. Inject width=\linewidth before colspec (tabularray body options)
  col_i <- grep("^colspec=", lines)
  if (length(col_i) > 0 && !any(grepl("^width=", lines)))
    lines <- append(lines, "width=\\linewidth,", after = col_i[1] - 1)

  # 3. Replace boolean "X" cells emitted by tinytable with $\checkmark$
  #
  # Two bugs to avoid:
  #   a) Backslashes: fixed=TRUE writes the R string literally, so
  #      "$\\checkmark$" (one R escape = one real backslash) is correct LaTeX.
  #      "$\\\\checkmark$" would write TWO backslashes and break compilation.
  #   b) Overlapping delimiters: "& X & X &" — the shared "&" means gsub
  #      skips every other cell in a single pass.  Loop until stable.
  while (any(grepl("& X &", lines, fixed = TRUE)))
    lines <- gsub("& X &", "& $\\checkmark$ &", lines, fixed = TRUE)

  # Last column ends with " \\" (tabularray row terminator)
  lines <- gsub("& X \\\\", "& $\\checkmark$ \\\\", lines, fixed = TRUE)

  writeLines(lines, path)
}

models <- list(
  "(1) log(BA)"  = did_main,
  "(2) BA Index" = did_idx,
  "(3) BA Level" = did_raw
)

modelsummary(
  models,
  output     = file.path(ROOT, "Draft", "tab_did.tex"),
  stars      = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map    = gof_map_sector,
  coef_map   = c(
    "post:aioe_norm" = "Post $\\times$ AI Exposure"
  ),
  title      = "Difference-in-Differences Estimates",
  notes      = "Standard errors clustered by 2-digit NAICS sector.",
  escape     = FALSE
)
message("Saved: Draft/tab_did.tex")

fix_tex_table(file.path(ROOT, "Draft", "tab_did.tex"), "tab:did")

# Build coef_map: replace "event_bin = N × aioe_norm" with "Mon YYYY (t = N)"
es_coef_names <- names(coef(es_model))
es_coef_map <- setNames(
  sapply(es_coef_names, function(nm) {
    t_val    <- as.integer(str_extract(nm, "-?\\d+"))
    cal_date <- as.Date("2022-11-01") %m+% months(t_val)
    paste0(format(cal_date, "%b %Y"), " ($t = ", t_val, "$)")
  }),
  es_coef_names
)

modelsummary(
  list("Event Study" = es_model),
  output   = file.path(ROOT, "Draft", "tab_event_study.tex"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = es_coef_map,
  title    = "Event-Study Estimates",
  notes    = "Standard errors clustered by 2-digit NAICS sector.",
  escape   = FALSE
)
message("Saved: Draft/tab_event_study.tex")
fix_tex_table(file.path(ROOT, "Draft", "tab_event_study.tex"), "tab:event_study")

message("\nAll outputs written to Draft/.")

# ---------------------------------------------------------------------------
# 6. Robustness: alternative BFS series (HBA, WBA, BF4Q)
# ---------------------------------------------------------------------------
# Each model uses the same sector + month FE and NAICS-level AI exposure,
# but swaps out the outcome variable.

# Check which alternative series are present after the 02_clean_bfs.R update
# All count-type series that can be log-transformed
log_series <- c("hba", "cba", "wba", "bf4q", "bf8q", "pbf4q", "pbf8q", "sbf4q", "sbf8q")
# Duration series: kept in levels (weeks), lower = faster formation
dur_series <- c("dur4q", "dur8q")
all_robust_series <- c(log_series, dur_series)

series_present <- function(col) col %in% names(df_reg) && sum(!is.na(df_reg[[col]])) > 0
message("Alternative series available: ",
        paste(all_robust_series[sapply(all_robust_series, series_present)], collapse = ", "))

# Column labels for the robustness table
col_labels <- c(
  ba    = "log(BA)",    cba   = "log(CBA)",   hba   = "log(HBA)",
  wba   = "log(WBA)",   bf4q  = "log(BF4Q)",  bf8q  = "log(BF8Q)",
  pbf4q = "log(PBF4Q)", pbf8q = "log(PBF8Q)",
  sbf4q = "log(SBF4Q)", sbf8q = "log(SBF8Q)",
  dur4q = "DUR4Q (wks)", dur8q = "DUR8Q (wks)"
)

robust_models <- list("(1) log(BA)" = did_main)
col_num <- 2L

run_did <- function(col, data, label) {
  if (!series_present(col)) return(invisible(NULL))
  is_dur <- col %in% dur_series

  df_col <- if (is_dur) {
    data |> filter(!is.na(.data[[col]]), .data[[col]] > 0) |>
      mutate(outcome = .data[[col]])           # levels for duration
  } else {
    data |> filter(.data[[col]] > 0) |>
      mutate(outcome = log(.data[[col]]))       # log for counts
  }

  # Skip if too few post-period obs (right-truncated series like BF8Q, PBF8Q)
  n_post <- sum(df_col$post == 1, na.rm = TRUE)
  if (n_post < 20) {
    message("  Skipping ", col, " DiD — only ", n_post,
            " post-period obs (right-truncated series).")
    return(invisible(NULL))
  }

  mod <- tryCatch(
    feols(outcome ~ post:aioe_norm | naics2 + ym,
          data = df_col, cluster = ~naics2),
    error = function(e) {
      message("  ", col, " DiD failed: ", e$message)
      NULL
    }
  )
  mod
}

# Tab only shows 5 core series to keep the table page-width.
# Full set of series (incl. pbf, sbf, dur) appears in fig_es_robust.
table_series <- c("cba", "hba", "wba", "sbf4q", "sbf8q")
for (col in table_series) {
  if (!series_present(col)) next
  mod <- run_did(col, df_reg, col_labels[col])
  if (!is.null(mod)) {
    lbl <- paste0("(", col_num, ") ", col_labels[col])
    robust_models[[lbl]] <- mod
    col_num <- col_num + 1L
  }
}

if (length(robust_models) > 1) {
  modelsummary(
    robust_models,
    output   = file.path(ROOT, "Draft", "tab_robust.tex"),
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map  = gof_map_sector,
    coef_map = c("post:aioe_norm" = "Post $\\times$ AI Exposure"),
    title    = "Robustness: Alternative BFS Series",
    notes    = paste(
      "Standard errors clustered by 2-digit NAICS sector.",
      "All specifications include sector and month fixed effects.",
      "BA = total business applications; CBA = corrected business applications;",
      "HBA = high-propensity applications; WBA = applications with planned wages;",
      "SBF4Q/SBF8Q = seasonally-adjusted formations within 4/8 quarters."
    ),
    escape   = FALSE
  )

  fix_tex_table(file.path(ROOT, "Draft", "tab_robust.tex"), "tab:robust")
  message("Saved: Draft/tab_robust.tex")
} else {
  message("Only BA_BA series found; skipping tab_robust.tex.",
          " Re-run after 02_clean_bfs.R update.")
}

# ---------------------------------------------------------------------------
# 7. Summary statistics table: AIIE and BFS by sector
# ---------------------------------------------------------------------------

sector_labels <- tribble(
  ~naics2,   ~sector,
  "11",      "Agriculture, Forestry, Fishing",
  "21",      "Mining, Quarrying, Oil \\& Gas",
  "22",      "Utilities",
  "23",      "Construction",
  "MNF",     "Manufacturing (NAICS 31--33)",
  "42",      "Wholesale Trade",
  "RET",     "Retail Trade (NAICS 44--45)",
  "TW",      "Transportation \\& Warehousing (48--49)",
  "51", "Information",
  "52", "Finance \\& Insurance",
  "53", "Real Estate \\& Rental",
  "54", "Professional, Scientific, Technical Svcs",
  "55", "Management of Companies",
  "56", "Admin.~\\& Support Services",
  "61", "Educational Services",
  "62", "Health Care \\& Social Assistance",
  "71", "Arts, Entertainment, \\& Recreation",
  "72", "Accommodation \\& Food Services",
  "81", "Other Services",
  "92", "Public Administration"
)

bfs_stats <- bfs |>
  filter(!naics2 %in% c("US", "NONAIC"), !is.na(ba)) |>
  group_by(naics2) |>
  summarise(
    mean_ba = mean(ba, na.rm = TRUE),
    sd_ba   = sd(ba,   na.rm = TRUE),
    min_ba  = min(ba,  na.rm = TRUE),
    max_ba  = max(ba,  na.rm = TRUE),
    n_obs   = n(),
    .groups = "drop"
  )

tab_summ <- ai |>
  select(naics2, aioe) |>
  inner_join(bfs_stats, by = "naics2") |>
  left_join(sector_labels, by = "naics2") |>
  arrange(desc(aioe)) |>
  mutate(
    sector = if_else(is.na(sector), paste0("NAICS~", naics2), sector)
  )

tab_rows <- tab_summ |>
  mutate(row_tex = sprintf(
    "  %s & %s & %.2f & %.1f & %.1f & %.1f & %.1f & %d \\\\",
    sector, naics2, aioe,
    mean_ba / 1000, sd_ba / 1000,
    min_ba  / 1000, max_ba / 1000,
    as.integer(n_obs)
  )) |>
  pull(row_tex)

tex_summ <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\small",
  "\\caption{AI Industry Exposure and Business Formation Statistics by Sector}",
  "\\label{tab:summary}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcrrrrrr}",
  "\\toprule",
  paste0("  Sector & NAICS & AIIE",
         " & Mean & SD & Min & Max & $N$ \\\\"),
  paste0("  & & ",
         " & \\multicolumn{4}{c}{Monthly BA (thousands)}",
         " & (months) \\\\"),
  "\\cmidrule(lr){4-7}",
  "\\midrule",
  tab_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("  \\item \\textit{Notes:} AIIE = AI Industry Exposure index from",
         " \\citet{felten2021occupational}, constructed from O*NET task descriptions",
         " and aggregated from 4-digit to 2-digit NAICS by employment-weighted averaging.",
         " Mean, SD, Min, and Max refer to monthly seasonally adjusted business",
         " applications (thousands) over the full sample period",
         " (January~2006 to early~2026). $N$ is the number of monthly observations.",
         " Sectors sorted by AIIE score descending."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_summ, file.path(ROOT, "Draft", "tab_summary.tex"))
message("Saved: Draft/tab_summary.tex")

# ---------------------------------------------------------------------------
# 8. Scatter plot: AI exposure vs. % change in applications
# ---------------------------------------------------------------------------
# X-axis: AIOE (normalised)
# Y-axis: % change from pre-ChatGPT average to latest available average
# One dot per sector; OLS fit line; faceted by BFS series.
# Pre period:  May 2022 – Oct 2022  (6 months before ChatGPT launch)
# Post period: last 6 available months in the data

pre_date_sc  <- as.Date("2022-11-01")          # ChatGPT released end of Nov 2022; Nov used as pre-period
latest_date  <- max(df$date, na.rm = TRUE)     # most recent month in data

message("Scatter pre:  ", pre_date_sc)
message("Scatter post: ", latest_date)

# Short sector abbreviations for dot labels
sector_abbrev <- tribble(
  ~naics2, ~abbrev,
  "11",    "Ag",
  "21",    "Mining",
  "22",    "Utilities",
  "23",    "Constr.",
  "MNF",   "Mfg",
  "42",    "Wholesale",
  "RET",   "Retail",
  "TW",    "Transport",
  "51",    "Info",
  "52",    "Finance",
  "53",    "Real Est.",
  "54",    "Prof. Svcs",
  "55",    "Mgmt",
  "56",    "Admin",
  "61",    "Education",
  "62",    "Health",
  "71",    "Arts",
  "72",    "Food/Hosp",
  "81",    "Other Svcs",
  "92",    "Gov't"
)

scatter_df <- df |>
  filter(!naics2 %in% c("US", "NONAIC"), ba > 0) |>
  select(naics2, date, aioe_norm, ba) |>
  filter(date == pre_date_sc | date == latest_date) |>
  mutate(period = if_else(date == pre_date_sc, "pre", "post")) |>
  group_by(naics2, aioe_norm, period) |>
  summarise(ba = mean(ba, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = ba) |>
  filter(!is.na(pre), !is.na(post), pre > 0) |>
  mutate(pct_change = 100 * (post - pre) / pre) |>
  left_join(sector_abbrev, by = "naics2") |>
  mutate(abbrev = if_else(is.na(abbrev), naics2, abbrev))

message("Scatter sectors: ", nrow(scatter_df))

fig_scatter <- ggplot(scatter_df, aes(x = aioe_norm, y = pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue",
              fill = "steelblue", alpha = 0.15, linewidth = 0.8) +
  geom_point(colour = "grey25", size = 2) +
  geom_text(aes(label = abbrev), size = 2.5, vjust = -0.7,
            colour = "grey25", check_overlap = TRUE) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", ".25", ".5", ".75", "1")) +
  labs(
    title    = "AI Exposure vs. Change in Business Applications",
    subtitle = paste0("Nov 2022 vs. ", format(latest_date, "%b %Y")),
    x        = "AI Industry Exposure",
    y        = "% change in business applications",
    caption  = "Sources: Census Bureau Business Formation Statistics; Felten, Raj & Seamans (2021)."
  ) +
  theme_paper +
  theme(
    plot.subtitle = element_text(size = 8, colour = "grey40"),
    plot.caption  = element_text(size = 7, hjust = 0, colour = "grey40")
  )

ggsave(file.path(ROOT, "Draft", "fig_scatter.pdf"), fig_scatter,
       width = 7, height = 5, device = "pdf")
message("Saved: Draft/fig_scatter.pdf")

# Companion regression table for the scatter
# Unweighted OLS and WLS weighted by pre-period mean BA (sector size)
scatter_reg_df <- scatter_df |>
  left_join(
    df |>
      filter(!naics2 %in% c("US", "NONAIC"), date <= pre_date_sc, ba > 0) |>
      group_by(naics2) |>
      summarise(weight = mean(ba, na.rm = TRUE), .groups = "drop"),
    by = "naics2"
  )

scatter_ols <- lm(pct_change ~ aioe_norm, data = scatter_reg_df)
scatter_wls <- lm(pct_change ~ aioe_norm, data = scatter_reg_df,
                  weights = weight)

modelsummary(
  list("OLS" = scatter_ols, "WLS" = scatter_wls),
  output  = file.path(ROOT, "Draft", "tab_scatter.tex"),
  stars   = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "aioe_norm"   = "AI Exposure (AIOE, normalised)",
    "(Intercept)" = "Constant"
  ),
  gof_map = list(
    list(raw = "nobs",      clean = "Sectors",  fmt = 0),
    list(raw = "r.squared", clean = "R$^2$",    fmt = 3)
  ),
  title  = "Cross-Sectional Regression: AI Exposure and Change in Business Applications",
  notes  = paste(
    "OLS and WLS (weighted by sector mean BA, Nov 2022) estimates.",
    "Outcome: percentage change in business applications from November~2022",
    "to the most recent month available.",
    "Each observation is a 2-digit NAICS sector ($N = 19$)."
  ),
  escape = FALSE
)

fix_tex_table(file.path(ROOT, "Draft", "tab_scatter.tex"), "tab:scatter")
message("Saved: Draft/tab_scatter.tex")

# =============================================================================
# 08_event_study_regressions.R
# Event study via MONTHLY CROSS-SECTIONAL REGRESSIONS (no DiD / no FE).
#
# For each month t, run a single cross-sectional OLS across 2-digit NAICS
# sectors:
#
#     Dlog_BA_{i,t} = alpha_t + beta_t * AIOE_i + eps_{i,t}
#
# where Dlog_BA_{i,t} = log(BA_{i,t}) - log(BA_{i, Nov 2022}).
#
# The path of beta_t traces the dynamic effect of AI exposure on business
# applications relative to the ChatGPT baseline month, without imposing any
# two-way FE / DiD structure.  t = Nov 2022 is the omitted reference (beta = 0
# by construction).
#
# Outputs written to Draft/:
#   fig_event_study_regs.pdf       main coefficient path (OLS + WLS)
#   tab_event_study_regs.tex       selected-horizon regression table
# =============================================================================

library(tidyverse)
library(lubridate)
library(modelsummary)
library(ggplot2)
library(broom)
library(sandwich)
library(here)

ROOT <- here::here()

# ---------------------------------------------------------------------------
# 0. Load data (same inputs as 04_merge_and_analysis.R)
# ---------------------------------------------------------------------------
bfs <- readRDS(file.path(ROOT, "Data", "Output", "bfs_panel.rds"))
ai  <- readRDS(file.path(ROOT, "Data", "Output", "ai_exposure_naics.rds"))

REF_DATE <- as.Date("2022-11-01")   # ChatGPT baseline month (omitted bin)

# ---------------------------------------------------------------------------
# 1. Build sector-month panel: log(BA) and log-change vs. Nov 2022
# ---------------------------------------------------------------------------
panel <- bfs |>
  filter(naics2 != "US", !is.na(ba), ba > 0) |>
  inner_join(ai |> select(naics2, aioe, aioe_norm, aioe_quartile),
             by = "naics2") |>
  mutate(log_ba = log(ba))

# Baseline log(BA) at Nov 2022, one per sector
base_df <- panel |>
  filter(date == REF_DATE) |>
  select(naics2, log_ba_base = log_ba, ba_base = ba)

message("Sectors with Nov-2022 baseline: ", nrow(base_df))

# Long-difference panel: Dlog(BA)_{i,t} = log(BA_{i,t}) - log(BA_{i,Nov2022})
dpanel <- panel |>
  inner_join(base_df, by = "naics2") |>
  mutate(
    dlog_ba = log_ba - log_ba_base,
    t_mo    = interval(REF_DATE, date) %/% months(1)
  ) |>
  filter(!is.na(dlog_ba), is.finite(dlog_ba))

message("Panel rows: ", nrow(dpanel),
        "  | sectors: ", n_distinct(dpanel$naics2),
        "  | months: ", n_distinct(dpanel$date))

# ---------------------------------------------------------------------------
# 2. Run one cross-sectional regression per month, vs. Nov 2022
#    Spec 1: OLS                Dlog(BA) ~ AIOE
#    Spec 2: WLS (weighted by pre-period BA)
#    Spec 3: OLS with AI-exposure quartile dummies (Q4 vs Q1 reference)
#    Spec 4: OLS with %-change outcome  100*(BA/BA_{Nov2022} - 1) ~ AIOE
# ---------------------------------------------------------------------------
run_month_reg <- function(df_m, spec = c("ols", "wls", "quartile", "pct")) {
  spec <- match.arg(spec)
  if (nrow(df_m) < 5) return(NULL)

  if (spec == "ols") {
    mod  <- lm(dlog_ba ~ aioe_norm, data = df_m)
    coef_row <- "aioe_norm"
  } else if (spec == "wls") {
    mod  <- lm(dlog_ba ~ aioe_norm, data = df_m, weights = ba_base)
    coef_row <- "aioe_norm"
  } else if (spec == "quartile") {
    df_q <- df_m |> mutate(q = factor(aioe_quartile,
                                      levels = 1:4,
                                      labels = c("Q1", "Q2", "Q3", "Q4")))
    mod  <- lm(dlog_ba ~ q, data = df_q)
    coef_row <- "qQ4"
  } else {
    df_p <- df_m |> mutate(pct = 100 * (exp(dlog_ba) - 1))
    mod  <- lm(pct ~ aioe_norm, data = df_p)
    coef_row <- "aioe_norm"
  }

  # HC1 robust SEs (cross-sectional, N = 19 sectors per regression)
  vc <- tryCatch(sandwich::vcovHC(mod, type = "HC1"),
                 error = function(e) vcov(mod))
  cf <- coef(mod)
  if (!coef_row %in% names(cf)) return(NULL)

  est <- unname(cf[coef_row])
  se  <- sqrt(vc[coef_row, coef_row])
  df_resid <- mod$df.residual
  tstat    <- est / se
  pv       <- 2 * pt(abs(tstat), df = df_resid, lower.tail = FALSE)
  tcrit    <- qt(0.975, df_resid)

  tibble(
    coef   = est,
    se     = se,
    ci_lo  = est - tcrit * se,
    ci_hi  = est + tcrit * se,
    pval   = pv,
    n      = nobs(mod),
    r2     = summary(mod)$r.squared
  )
}

# Months to loop over — all months in the panel with >= 5 sectors
month_list <- dpanel |>
  group_by(date) |>
  summarise(n_sec = n_distinct(naics2), .groups = "drop") |>
  filter(n_sec >= 5) |>
  arrange(date) |>
  pull(date)

message("Regressions to run per spec: ", length(month_list))

specs <- c("ols", "wls", "quartile", "pct")
spec_labels <- c(
  ols      = "(1) OLS: Dlog(BA) on AIOE",
  wls      = "(2) WLS (weight = BA_{Nov2022})",
  quartile = "(3) OLS: Dlog(BA) on Q4 dummy (ref = Q1)",
  pct      = "(4) OLS: %-change on AIOE"
)

es_path <- map_dfr(specs, function(sp) {
  map_dfr(month_list, function(d) {
    df_m <- dpanel |> filter(date == d)
    res  <- run_month_reg(df_m, spec = sp)
    if (is.null(res)) return(NULL)
    res |> mutate(date = d, t_mo = interval(REF_DATE, d) %/% months(1),
                  spec = sp)
  })
}) |>
  mutate(spec_label = spec_labels[spec])

# Reference month gets coef = 0 by construction (omitted bin)
ref_rows <- tibble(
  date = REF_DATE, t_mo = 0,
  coef = 0, se = 0, ci_lo = 0, ci_hi = 0, pval = NA_real_,
  n = NA_integer_, r2 = NA_real_,
  spec = specs, spec_label = spec_labels[specs]
)
es_path <- bind_rows(es_path, ref_rows) |>
  arrange(spec, date) |>
  mutate(
    sig = case_when(
      is.na(pval)  ~ "Reference",
      pval < 0.05  ~ "p < 0.05",
      pval < 0.10  ~ "p < 0.10",
      TRUE         ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.05", "p < 0.10", "n.s.", "Reference"))
  )

message("\nPeak |beta_t| by spec:")
print(
  es_path |>
    filter(sig != "Reference") |>
    group_by(spec_label) |>
    slice_max(abs(coef), n = 1) |>
    select(spec_label, date, coef, se, pval, n) |>
    ungroup()
)

# ---------------------------------------------------------------------------
# 3. Figure: coefficient path (OLS only, points coloured by significance)
# ---------------------------------------------------------------------------
theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 8, colour = "grey40"),
    plot.caption     = element_text(size = 7, hjust = 0, colour = "grey40")
  )

sig_colours <- c(
  "p < 0.05"  = "#2ca02c",
  "p < 0.10"  = "#98df8a",
  "n.s."      = "#d62728",
  "Reference" = "grey50"
)

# --- 3a. Main figure: OLS only, coloured by significance -----------------
PLOT_START <- as.Date("2019-11-01")

main_df <- es_path |>
  filter(spec == "ols", date >= PLOT_START)

# x-axis: one tick per November (matches the t = -36, -24, ..., +36 breaks
# used by the original 04_merge_and_analysis.R event-study plot)
plot_end    <- max(main_df$date, na.rm = TRUE)
nov_breaks  <- seq(PLOT_START,
                   as.Date(paste0(format(plot_end, "%Y"), "-11-01")),
                   by = "1 year")
nov_breaks  <- nov_breaks[nov_breaks <= plot_end]

fig_main <- ggplot(main_df, aes(x = date, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = REF_DATE, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  annotate("text", x = REF_DATE + 20, y = Inf,
           label = "ChatGPT\n(Nov 2022)", vjust = 1.3, hjust = 0, size = 3,
           colour = "grey30") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "grey70") +
  geom_line(colour = "grey40", linewidth = 0.6) +
  geom_point(aes(colour = sig), size = 2) +
  scale_colour_manual(values = sig_colours, name = NULL) +
  scale_x_date(breaks = nov_breaks, date_labels = "%b %Y",
               limits = c(PLOT_START, plot_end)) +
  labs(
    title    = "Event Study via Monthly Cross-Sectional Regressions",
    subtitle = "2-digit NAICS industries",
    x        = NULL,
    y        = "Coefficient on AI Exposure (beta_t)",
    caption  = "Own calculation based on Census BFS and Felten, Raj & Seamans (2021)."
  ) +
  theme_paper

ggsave(file.path(ROOT, "Draft", "fig_event_study_regs.pdf"), fig_main,
       width = 8, height = 4.5, device = "pdf")
message("Saved: Draft/fig_event_study_regs.pdf")

# --- 3b. Faceted figure: all 4 specs ----------------------------------
facet_df <- es_path |>
  mutate(
    spec_label = factor(
      spec_label,
      levels = spec_labels[c("ols", "wls", "quartile", "pct")]
    )
  )

fig_facet <- ggplot(facet_df, aes(x = date, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = REF_DATE, linetype = "dashed", colour = "black",
             linewidth = 0.45) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "grey70") +
  geom_line(colour = "grey40", linewidth = 0.5) +
  geom_point(aes(colour = sig), size = 1.2) +
  scale_colour_manual(values = sig_colours, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~ spec_label, ncol = 2, scales = "free_y") +
  labs(
    title    = "Event Study via Cross-Sectional Regressions \u2014 Four Specifications",
    subtitle = paste0("One regression per month across 2-digit NAICS sectors. ",
                      "Outcome baseline = Nov 2022. HC1 robust 95% CI."),
    x        = NULL,
    y        = "Coefficient on AI Exposure"
  ) +
  theme_paper +
  theme(strip.background = element_rect(fill = "grey93", colour = NA),
        strip.text       = element_text(size = 8.5, face = "bold"))

ggsave(file.path(ROOT, "Draft", "fig_event_study_regs_facet.pdf"), fig_facet,
       width = 10, height = 7, device = "pdf")
message("Saved: Draft/fig_event_study_regs_facet.pdf")

# ---------------------------------------------------------------------------
# 4. LaTeX table: selected horizons vs. Nov 2022 baseline
#    Columns = horizons; rows = the four specifications (Spec. 1-4)
# ---------------------------------------------------------------------------
# Horizons (months after Nov 2022) that actually exist in the data
desired_t <- c(6, 12, 18, 24, 30, 36)
avail_t   <- sort(unique(es_path$t_mo))
use_t     <- desired_t[desired_t %in% avail_t]

# Re-fit the selected-horizon models so modelsummary can render them.
# (We keep lm objects rather than re-using the coefficient path tibble, so
# that the table reports model-level goodness of fit correctly.)
fit_selected <- function(d, spec) {
  df_m <- dpanel |> filter(date == d)
  if (nrow(df_m) < 5) return(NULL)
  if (spec == "ols") {
    lm(dlog_ba ~ aioe_norm, data = df_m)
  } else if (spec == "wls") {
    lm(dlog_ba ~ aioe_norm, data = df_m, weights = ba_base)
  } else if (spec == "quartile") {
    df_q <- df_m |> mutate(q = factor(aioe_quartile, levels = 1:4,
                                      labels = c("Q1", "Q2", "Q3", "Q4")))
    lm(dlog_ba ~ q, data = df_q)
  } else {
    df_p <- df_m |> mutate(pct = 100 * (exp(dlog_ba) - 1))
    lm(pct ~ aioe_norm, data = df_p)
  }
}

for (sp in specs) {
  models_sp <- list()
  for (tt in use_t) {
    d  <- REF_DATE %m+% months(tt)
    md <- fit_selected(d, sp)
    if (is.null(md)) next
    lab <- paste0(format(d, "%b %Y"), " (t=+", tt, ")")
    models_sp[[lab]] <- md
  }
  if (length(models_sp) == 0) next

  coef_map_sp <- switch(sp,
    ols      = c("aioe_norm" = "AI Exposure (AIOE)", "(Intercept)" = "Constant"),
    wls      = c("aioe_norm" = "AI Exposure (AIOE)", "(Intercept)" = "Constant"),
    quartile = c(
      "qQ2" = "AIOE Quartile 2 (vs Q1)",
      "qQ3" = "AIOE Quartile 3 (vs Q1)",
      "qQ4" = "AIOE Quartile 4 (vs Q1)",
      "(Intercept)" = "Constant (Q1)"
    ),
    pct      = c("aioe_norm" = "AI Exposure (AIOE)", "(Intercept)" = "Constant")
  )

  title_sp <- switch(sp,
    ols      = "Cross-Sectional Event Study (Spec. 1: OLS, outcome $\\Delta \\log BA$)",
    wls      = "Cross-Sectional Event Study (Spec. 2: WLS weighted by pre-period BA, outcome $\\Delta \\log BA$)",
    quartile = "Cross-Sectional Event Study (Spec. 3: OLS on AIOE Quartile dummies)",
    pct      = "Cross-Sectional Event Study (Spec. 4: OLS, outcome \\%-change)"
  )

  out_path <- file.path(ROOT, "Draft",
                        paste0("tab_event_study_regs_", sp, ".tex"))
  tab_label <- paste0("tab:es_regs_", sp)

  modelsummary(
    models_sp,
    output   = out_path,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    vcov     = "HC1",
    coef_map = coef_map_sp,
    gof_map  = list(
      list(raw = "nobs",      clean = "Sectors", fmt = 0),
      list(raw = "r.squared", clean = "R$^2$",   fmt = 3)
    ),
    title    = title_sp,
    notes    = paste(
      "Each column is a separate cross-sectional OLS regression across",
      "2-digit NAICS sectors in the month indicated, with outcome defined",
      "relative to the November 2022 baseline (the reference month).",
      "HC1 heteroskedasticity-robust standard errors in parentheses."
    ),
    escape   = FALSE
  )

  # Inject \label{...} into the tabularray caption so \ref{} works from main.tex
  tab_lines <- readLines(out_path)
  cap_i <- grep("^caption=", tab_lines)
  if (length(cap_i) > 0 &&
      !any(grepl(paste0("label=\\{", tab_label, "\\}"), tab_lines))) {
    tab_lines <- append(tab_lines,
                        paste0("label={", tab_label, "},"),
                        after = cap_i[1])
    writeLines(tab_lines, out_path)
  }
  message("Saved: Draft/", basename(out_path))
}

# ---------------------------------------------------------------------------
# 5. Save the full coefficient path so other scripts / drafts can reuse it
# ---------------------------------------------------------------------------
saveRDS(es_path, file.path(ROOT, "Data", "Output",
                           "event_study_regs_path.rds"))
message("Saved: Data/Output/event_study_regs_path.rds")

message("\n=========================================================")
message("  Cross-sectional regression event study complete.")
message("  Figures:  fig_event_study_regs.pdf, fig_event_study_regs_facet.pdf")
message("  Tables:   tab_event_study_regs_{ols,wls,quartile,pct}.tex")
message("=========================================================")

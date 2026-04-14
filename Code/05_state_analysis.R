# =============================================================================
# 05_state_analysis.R
# State-level DiD: uses state x time panel with state FE + month FE.
#
# Outcome:    log(total business applications) by state
# Treatment:  AI Geographic Exposure (AIGE) index from Felten et al.
#             Appendix C — constructed from occupation-level AIOE scores
#             and state employment-weighted occupational mix.
# Fixed effects: state + calendar month-year
# Clustering:    state (56 units incl. DC and PR)
#
# This serves as a robustness check on the main NAICS-sector DiD:
#   • Different level of geographic aggregation (state vs. sector)
#   • Different exposure measure (AIGE vs. AIIE)
#   • State FE absorbs permanent state-level differences in entrepreneurship
#   • Month FE absorbs national macro shocks
# =============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(broom)
library(ggplot2)
library(here)

ROOT <- here::here()

# ---------------------------------------------------------------------------
# 0. Load data
# ---------------------------------------------------------------------------
bfs_state  <- readRDS(file.path(ROOT, "Data", "Output", "bfs_state.rds"))
state_aige <- readRDS(file.path(ROOT, "Data", "Output", "state_aige.rds"))

message("State BFS rows: ",  nrow(bfs_state))
message("States in BFS: ",   n_distinct(bfs_state$state))
message("States in AIGE: ",  nrow(state_aige))

# ---------------------------------------------------------------------------
# 1. Merge
# ---------------------------------------------------------------------------
df_state <- bfs_state |>
  inner_join(
    state_aige |> select(state, aige, aige_norm, aige_quartile),
    by = "state"
  ) |>
  mutate(
    post    = as.integer(date >= as.Date("2022-11-01")),
    event_t = interval(as.Date("2022-11-01"), date) %/% months(1),
    state   = factor(state),
    ym      = as.integer(format(date, "%Y%m"))
  )

message("Merged rows: ", nrow(df_state))
message("States matched: ", n_distinct(df_state$state))
message("Date range: ", min(df_state$date), " to ", max(df_state$date))

# ---------------------------------------------------------------------------
# 2. DiD models
# ---------------------------------------------------------------------------
df_sreg <- df_state |>
  filter(ba > 0) |>
  mutate(log_ba = log(ba))

# 2a. Baseline: Post x AIGE, state + month FE
did_state_main <- feols(
  log_ba ~ post:aige_norm | state + ym,
  data    = df_sreg,
  cluster = ~state
)

# 2b. Two-period: ChatGPT window vs. agentic AI window
df_sreg2 <- df_sreg |>
  mutate(
    post_chatgpt = as.integer(date >= as.Date("2022-11-01") &
                                date <  as.Date("2025-05-01")),
    post_agentic = as.integer(date >= as.Date("2025-05-01"))
  )

did_state_two <- feols(
  log_ba ~ post_chatgpt:aige_norm + post_agentic:aige_norm | state + ym,
  data    = df_sreg2,
  cluster = ~state
)

# 2c. BA index as alternative outcome
did_state_idx <- feols(
  ba_idx ~ post:aige_norm | state + ym,
  data    = df_sreg |> filter(!is.na(ba_idx)),
  cluster = ~state
)

print(summary(did_state_main))

# ---------------------------------------------------------------------------
# 3. Event study: state-level
# ---------------------------------------------------------------------------
df_ses <- df_sreg |>
  filter(between(event_t, -36, 36)) |>
  mutate(
    event_bin = case_when(
      event_t <= -36 ~ -36L,
      event_t >=  36 ~  36L,
      TRUE           ~ as.integer(event_t)
    ),
    event_bin = relevel(factor(event_bin), ref = "-1")
  )

es_state <- feols(
  log_ba ~ i(event_bin, aige_norm, ref = "-1") | state + ym,
  data    = df_ses,
  cluster = ~state
)

# ---------------------------------------------------------------------------
# 3b. Descriptive figure: indexed BA by AIGE quartile across states
# ---------------------------------------------------------------------------
chatgpt_date <- as.Date("2022-11-01")
agentic_date <- as.Date("2025-05-01")

fig_desc_state <- df_state |>
  filter(!is.na(ba_idx), !is.na(aige_quartile)) |>
  mutate(
    quartile_label = factor(aige_quartile,
                            labels = c("Q1 (Low AI)", "Q2", "Q3",
                                       "Q4 (High AI)"))
  ) |>
  group_by(date, quartile_label) |>
  summarise(ba_idx_mean = mean(ba_idx, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = date, y = ba_idx_mean, colour = quartile_label)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = chatgpt_date, linetype = "dashed",
             colour = "black", linewidth = 0.6) +
  geom_vline(xintercept = agentic_date, linetype = "dashed",
             colour = "black", linewidth = 0.6) +
  annotate("text", x = chatgpt_date + 30, y = Inf,
           label = "ChatGPT\nlaunch", vjust = 1.3, hjust = 0, size = 3) +
  annotate("text", x = agentic_date + 30, y = Inf,
           label = "Agentic\nAI", vjust = 1.3, hjust = 0, size = 3) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1,
                      name = "AI Geographic Exposure Quartile") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title   = "Business Applications by State AI Exposure Quartile (Jan 2020 = 100)",
    subtitle = "States grouped by AIGE quartile; lines show cross-state means.",
    x       = NULL,
    y       = "Index (Jan 2020 = 100)"
  ) +
  theme_paper +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"))

ggsave(file.path(ROOT, "Draft", "fig_desc_state.pdf"), fig_desc_state,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_desc_state.pdf")

# Extract coefficients
es_state_df <- tidy(es_state, conf.int = TRUE) |>
  filter(str_detect(term, "event_bin")) |>
  mutate(t = as.numeric(str_extract(term, "-?\\d+"))) |>
  select(t, coef = estimate, ci_lo = conf.low, ci_hi = conf.high) |>
  bind_rows(tibble(t = -1, coef = 0, ci_lo = 0, ci_hi = 0)) |>
  arrange(t)

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 11)
  )

fig_es_state <- ggplot(es_state_df, aes(x = t, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept =  0, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  geom_vline(xintercept = 30, linetype = "dashed", colour = "grey40",
             linewidth = 0.5) +
  annotate("text", x = 30.5, y = Inf, label = "Agentic\nAI",
           vjust = 1.4, hjust = 0, size = 2.8, colour = "grey30") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.2, fill = "darkorange") +
  geom_line(colour = "darkorange", linewidth = 0.8) +
  geom_point(colour = "darkorange", size = 1.5) +
  labs(
    title    = "Event Study: Effect of AI Exposure on log(Business Applications) - State Level",
    subtitle = "State + month FE. 95% CI, SEs clustered by state (n = 56).",
    x        = "Months relative to ChatGPT launch (Nov 2022)",
    y        = "Coefficient on AI Geographic Exposure (AIGE)"
  ) +
  theme_paper +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"))

ggsave(file.path(ROOT, "Draft", "fig_es_state.pdf"), fig_es_state,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_es_state.pdf")

# ---------------------------------------------------------------------------
# 3c. Faceted event study across BFS series — state level
# ---------------------------------------------------------------------------
# Mirrors fig_es_robust (industry-level) but uses state x time panel.
# Series available at state level: BA, HBA, WBA, CBA.
# BF / formation series are national-only in BFS.

series_meta_state <- tribble(
  ~col,    ~label,
  "ba",    "BA (Total)",
  "hba",   "HBA (High-Propensity)",
  "wba",   "WBA (Planned Wages)",
  "cba",   "CBA (Corrected)"
)

run_es_state <- function(outcome_col, df_base) {
  df_use <- df_base |>
    filter(!is.na(.data[[outcome_col]]), .data[[outcome_col]] > 0) |>
    mutate(
      log_y     = log(.data[[outcome_col]]),
      event_bin = as.integer(case_when(
        event_t <= -36L ~ -36L,
        event_t >=  36L ~  36L,
        TRUE            ~ as.integer(event_t)
      ))
    ) |>
    filter(between(event_t, -36, 36))

  n_post <- df_use |> filter(event_t >= 0) |> nrow()
  if (n_post < 20) {
    message("  Skipping ", outcome_col, ": only ", n_post, " post-period obs.")
    return(NULL)
  }

  tryCatch({
    mod <- feols(
      log_y ~ i(event_bin, aige_norm, ref = -1L) | state + ym,
      data    = df_use,
      cluster = ~state
    )
    tidy(mod, conf.int = TRUE) |>
      filter(str_detect(term, "event_bin")) |>
      mutate(t = as.integer(str_extract(term, "-?\\d+"))) |>
      select(t, coef = estimate, ci_lo = conf.low, ci_hi = conf.high) |>
      bind_rows(tibble(t = -1L, coef = 0, ci_lo = 0, ci_hi = 0)) |>
      arrange(t)
  }, error = function(e) {
    message("  ", outcome_col, " state ES failed: ", conditionMessage(e))
    NULL
  })
}

df_es_base <- df_state |>
  filter(ba > 0) |>
  mutate(ym = as.integer(format(date, "%Y%m")))

es_state_series <- purrr::map(
  setNames(series_meta_state$col, series_meta_state$col),
  ~run_es_state(.x, df_es_base)
) |>
  purrr::keep(Negate(is.null)) |>
  dplyr::bind_rows(.id = "col") |>
  left_join(series_meta_state, by = "col") |>
  mutate(label = factor(label, levels = series_meta_state$label))

message("State series ES rows: ", nrow(es_state_series))
message("Series recovered: ", paste(unique(es_state_series$label), collapse = ", "))

if (nrow(es_state_series) == 0)
  stop("All state series event studies failed — check messages above.")

fig_es_state_facet <- ggplot(es_state_series, aes(x = t, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept =  0, linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  geom_vline(xintercept = 30, linetype = "dashed", colour = "grey40",
             linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.7) +
  geom_point(colour = "steelblue", size = 1.2) +
  facet_wrap(~label, ncol = 2, scales = "free_y") +
  labs(
    title    = "State Event Study: Effect of AIGE on log(Business Applications)",
    subtitle = "Each panel: log(series) ~ i(event_bin, AIGE) | state + month FE. 95% CI, SEs clustered by state.",
    x        = "Months relative to ChatGPT launch (Nov 2022)",
    y        = "Coefficient on AI Geographic Exposure (AIGE)"
  ) +
  theme_paper +
  theme(
    plot.subtitle    = element_text(size = 8, colour = "grey40"),
    strip.background = element_rect(fill = "grey92", colour = NA),
    strip.text       = element_text(face = "bold", size = 9)
  )

ggsave(file.path(ROOT, "Draft", "fig_es_state_facet.pdf"),
       fig_es_state_facet, width = 8, height = 6, device = "pdf")
message("Saved: Draft/fig_es_state_facet.pdf")

# ---------------------------------------------------------------------------
# 4. Regression table: tab_state.tex
# ---------------------------------------------------------------------------
gof_map_state <- list(
  list(raw = "nobs",      clean = "Observations", fmt = 0),
  list(raw = "r.squared", clean = "R$^2$",        fmt = 3),
  list(raw = "FE: state", clean = "State FE",     fmt = 0),
  list(raw = "FE: ym",    clean = "Month FE",     fmt = 0)
)

fix_tex_table <- function(path, label) {
  lines <- readLines(path)
  cap_i <- grep("^caption=", lines)
  if (length(cap_i) > 0 && !any(grepl(paste0("label=\\{", label, "\\}"), lines)))
    lines <- append(lines, paste0("label={", label, "},"), after = cap_i[1])
  col_i <- grep("^colspec=", lines)
  if (length(col_i) > 0 && !any(grepl("^width=", lines)))
    lines <- append(lines, "width=\\linewidth,", after = col_i[1] - 1)
  while (any(grepl("& X &", lines, fixed = TRUE)))
    lines <- gsub("& X &", "& $\\checkmark$ &", lines, fixed = TRUE)
  lines <- gsub("& X \\\\", "& $\\checkmark$ \\\\", lines, fixed = TRUE)
  writeLines(lines, path)
}

state_models <- list(
  "(1) log(BA)"    = did_state_main,
  "(2) BA Index"   = did_state_idx,
  "(3) Two-period" = did_state_two
)

modelsummary(
  state_models,
  output   = file.path(ROOT, "Draft", "tab_state.tex"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map  = gof_map_state,
  coef_map = c(
    "post:aige_norm"         = "Post$_{\\text{ChatGPT}}$ $\\times$ AI Exposure",
    "post_chatgpt:aige_norm" = "Post$_{\\text{ChatGPT}}$ $\\times$ AI Exposure",
    "post_agentic:aige_norm" = "Post$_{\\text{Agentic}}$ $\\times$ AI Exposure"
  ),
  title    = "State-Level Difference-in-Differences Estimates",
  notes    = paste(
    "Standard errors clustered by state.",
    "AI exposure is the AI Geographic Exposure (AIGE) index",
    "from \\citet{felten2021occupational} Appendix~C,",
    "constructed from occupation-level AIOE scores weighted by",
    "the state employment-mix across occupations.",
    "Post$_{\\text{ChatGPT}}$: Nov 2022 -- Apr 2025.",
    "Post$_{\\text{Agentic}}$: May 2025 onwards."
  ),
  escape   = FALSE
)

fix_tex_table(file.path(ROOT, "Draft", "tab_state.tex"), "tab:state")
message("Saved: Draft/tab_state.tex")

message("\n05_state_analysis.R complete.")

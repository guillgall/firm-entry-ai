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

setwd("/Users/ggallacher/Documents/GitHub/firm-entry-ai")
ROOT <- getwd()

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

# Two-period DiD: separate coefficients for ChatGPT (Nov 2022) and agentic AI (May 2025)
df_reg2 <- df_reg |>
  mutate(
    post_chatgpt  = as.integer(date >= as.Date("2022-11-01") &
                                 date <  as.Date("2025-05-01")),
    post_agentic  = as.integer(date >= as.Date("2025-05-01"))
  )

did_two <- feols(
  log_ba ~ post_chatgpt:aioe_norm + post_agentic:aioe_norm | naics2 + ym,
  data    = df_reg2,
  cluster = ~naics2
)

print(summary(did_main))

# ---------------------------------------------------------------------------
# 3. Event study: dynamic effects relative to Nov 2022
# ---------------------------------------------------------------------------
# Bin event-time: cap at ±24 months, omit t = -1 (reference period)
df_es <- df_reg |>
  filter(between(event_t, -36, 36)) |>
  mutate(
    event_bin = case_when(
      event_t <= -36 ~ -36L,
      event_t >=  36 ~  36L,
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
  ci_hi = es_tidy$conf.high
) |>
  # Add the omitted reference period (t = -1, coef = 0)
  bind_rows(tibble(t = -1, coef = 0, ci_lo = 0, ci_hi = 0)) |>
  arrange(t)

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
agentic_date  <- as.Date("2025-05-01")   # agentic AI tools (t ≈ +30 from ChatGPT)

## 4a. Sector spotlight: Professional Services vs Manufacturing vs Construction ----
# Mirror the FRED motivating chart. Manufacturing = sum of NAICS 31, 32, 33.
# Uses seasonally-adjusted ba values from the monthly SA file.
# Aggregate SA counts across sub-sectors, then re-index to Jan 2020 = 100.
reindex <- function(df) {
  base <- df |> filter(date == as.Date("2020-01-01")) |> pull(ba)
  if (length(base) == 0 || is.na(base) || base == 0) return(df |> mutate(ba_idx = NA_real_))
  df |> mutate(ba_idx = 100 * ba / base)
}

# Use sector rows only (exclude the "US" aggregate added by 02_clean_bfs.R)
bfs_sectors_df <- bfs |> filter(naics2 != "US")

spotlight_sectors <- bfs_sectors_df |>
  filter(date >= as.Date("2016-01-01")) |>
  mutate(
    sector_label = case_when(
      naics2 == "54"                    ~ "Professional Services (NAICS 54)",
      naics2 %in% c("31", "32", "33")   ~ "Manufacturing (NAICS 31\u201333)",
      naics2 == "23"                    ~ "Construction (NAICS 23)",
      TRUE                              ~ NA_character_
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
      "Manufacturing (NAICS 31\u201333)",
      "Construction (NAICS 23)"
    ))
  ) |>
  arrange(sector_label, date)

# Colours approximate FRED: blue, red, teal, black
sector_colours <- c(
  "Professional Services (NAICS 54)"  = "#1f77b4",
  "Total (all NAICS)"                 = "#d62728",
  "Manufacturing (NAICS 31\u201333)"  = "#2ca02c",
  "Construction (NAICS 23)"           = "#111111"
)

fig_spotlight <- ggplot(spotlight_df,
                        aes(x = date, y = ba_idx,
                            colour = sector_label,
                            linewidth = sector_label)) +
  geom_line() +
  geom_vline(xintercept = chatgpt_date, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = agentic_date, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  annotate("text", x = chatgpt_date + 45, y = Inf,
           label = "ChatGPT\nlaunch", vjust = 1.4, hjust = 0, size = 2.8,
           colour = "grey30") +
  annotate("text", x = agentic_date + 45, y = Inf,
           label = "Agentic\nAI", vjust = 1.4, hjust = 0, size = 2.8,
           colour = "grey30") +
  scale_colour_manual(values = sector_colours, name = NULL) +
  scale_linewidth_manual(
    values = c(
      "Professional Services (NAICS 54)" = 0.9,
      "Total (all NAICS)"                = 0.9,
      "Manufacturing (NAICS 31\u201333)" = 0.9,
      "Construction (NAICS 23)"          = 0.9
    ), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Business Applications by Sector (Jan 2020 = 100)",
    x     = NULL,
    y     = "Index (Jan 2020 = 100)"
  ) +
  theme_paper +
  theme(legend.text = element_text(size = 8))

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
  geom_vline(xintercept = agentic_date, linetype = "dashed",
             colour = "black", linewidth = 0.6) +
  annotate("text", x = chatgpt_date + 30, y = Inf,
           label = "ChatGPT\nlaunch", vjust = 1.3, hjust = 0, size = 3) +
  annotate("text", x = agentic_date + 30, y = Inf,
           label = "Agentic\nAI", vjust = 1.3, hjust = 0, size = 3) +
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
fig_es <- ggplot(es_df, aes(x = t, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0,  linetype = "dashed", colour = "black",
             linewidth = 0.5) +
  geom_vline(xintercept = 30, linetype = "dashed", colour = "grey40",
             linewidth = 0.5) +
  annotate("text", x = 30.5, y = Inf, label = "Agentic\nAI",
           vjust = 1.4, hjust = 0, size = 2.8, colour = "grey30") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(colour = "steelblue", linewidth = 0.8) +
  geom_point(colour = "steelblue", size = 1.5) +
  labs(
    title = "Event Study: Effect of AI Exposure on log(Business Applications)",
    x     = "Months relative to ChatGPT launch (Nov 2022)",
    y     = "Coefficient on AI Exposure"
  ) +
  theme_paper

ggsave(file.path(ROOT, "Draft", "fig_event_study.pdf"), fig_es,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_event_study.pdf")

# ---------------------------------------------------------------------------
# 5. Regression tables (modelsummary → LaTeX)
# ---------------------------------------------------------------------------
models <- list(
  "(1) log(BA)"    = did_main,
  "(2) BA Index"   = did_idx,
  "(3) BA Level"   = did_raw,
  "(4) Two-period" = did_two
)

gof_map <- tribble(
  ~raw,          ~clean,              ~fmt,
  "nobs",        "Observations",      0,
  "r.squared",   "R$^2$",             3,
  "FE: naics2",  "Sector FE",         0,
  "FE: ym",      "Month FE",          0
)

modelsummary(
  models,
  output     = file.path(ROOT, "Draft", "tab_did.tex"),
  stars      = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map    = gof_map,
  coef_map   = c(
    "post:aioe_norm"         = "Post$_{\\text{ChatGPT}}$ $\\times$ AI Exposure",
    "post_chatgpt:aioe_norm" = "Post$_{\\text{ChatGPT}}$ $\\times$ AI Exposure",
    "post_agentic:aioe_norm" = "Post$_{\\text{Agentic}}$ $\\times$ AI Exposure"
  ),
  title      = "Difference-in-Differences Estimates",
  notes      = paste("Standard errors clustered by 2-digit NAICS sector.",
                     "Post$_{\\text{ChatGPT}}$: Nov 2022 -- Apr 2025.",
                     "Post$_{\\text{Agentic}}$: May 2025 onwards."),
  escape     = FALSE
)
message("Saved: Draft/tab_did.tex")

modelsummary(
  list("Event Study" = es_model),
  output   = file.path(ROOT, "Draft", "tab_event_study.tex"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title    = "Event-Study Estimates",
  notes    = "Standard errors clustered by 2-digit NAICS sector.",
  escape   = FALSE
)
message("Saved: Draft/tab_event_study.tex")

message("\nAll outputs written to Draft/.")

# =============================================================================
# 07_entry_validation.R
# Replicate Decker & Haltiwanger (2023, BPEA) Figure 2:
# "New Business Entry and New Business Applications"
#
# Five series, all indexed to 2019:Q1 = 1:
#   1. BDS firms          – annual, blue dotted    (Census BDS API)
#   2. BED establishments – quarterly SA, orange dashed (BLS BED API)
#   3. BED firms          – annual, black dash-dot (approximated from quarterly BED)
#   4. BFS applications   – quarterly, black long-dashed (bfs_panel.rds HBA)
#   5. BDS single-unit    – quarterly, red solid   (Census BDSQ; skipped if unavailable)
#
# Data sources:
#   BDS  – Census Bureau Business Dynamics Statistics (Census API)
#   BED  – BLS Business Employment Dynamics (BLS API v2, series BDS0000000000000000110003LQ5)
#   BFS  – Census Bureau Business Formation Statistics (bfs_panel.rds)
#
# Note: BDS single-unit quarterly and BED annual firm births are not available
# via public APIs. BDS firms uses the DHS firm birth rate computed from
# (FIRM_t - FIRM_{t-1} + FIRMDEATH_FIRMS_t) / avg_FIRM. BED annual is the
# sum of quarterly SA establishment births (approximation of firm births).
#
# BLS BED series ID: BDS0000000000000000110003LQ5
#   BD = prefix | S = SA | 00000+00+000 = national | 000000 = all industries
#   1 = establishments | 1 = count | 00 = all sizes | 03 = births/openings
#   L = level | Q = quarterly | 5 = private sector
# =============================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(here)

ROOT <- here::here()

# Helper: year + quarter → decimal (2019Q1 = 2019.0, 2019Q2 = 2019.25, …)
yq <- function(year, quarter) year + (quarter - 1) / 4

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.key.width  = unit(2.2, "cm"),
    plot.title        = element_text(face = "bold", size = 11),
    plot.caption      = element_text(size = 7.5, hjust = 0, colour = "grey40"),
    axis.text         = element_text(size = 9)
  )

# NBER recession bands (in decimal years)
recessions <- tibble(
  xmin = c(2001.0, 2007.75, 2020.0),
  xmax = c(2001.75, 2009.25, 2020.25)
)

# =============================================================================
# 1.  BFS — monthly HBA (seasonally adjusted) → quarterly totals
# =============================================================================
bfs_raw <- readRDS(file.path(ROOT, "Data", "Output", "bfs_panel.rds"))

bfs_q <- bfs_raw |>
  filter(naics2 == "US", !is.na(hba)) |>
  mutate(
    year    = as.integer(format(date, "%Y")),
    quarter = ceiling(as.integer(format(date, "%m")) / 3)
  ) |>
  group_by(year, quarter) |>
  summarise(value = sum(hba, na.rm = TRUE), .groups = "drop") |>
  mutate(t = yq(year, quarter), series = "BFS applications") |>
  filter(year >= 1993)

bfs_base <- bfs_q |> filter(year == 2019, quarter == 1) |> pull(value)
stopifnot(length(bfs_base) == 1 && bfs_base > 0)
bfs_q <- bfs_q |> mutate(idx = value / bfs_base)

message("BFS quarterly: ", nrow(bfs_q), " obs (",
        min(bfs_q$year), "Q", bfs_q$quarter[which.min(bfs_q$t)],
        "–", max(bfs_q$year), "Q", bfs_q$quarter[which.max(bfs_q$t)], ")")

# =============================================================================
# 2.  BDS — DHS firm birth rate, annual (Census BDS API)
#
# Identity: firm_births(t) = FIRM(t) - FIRM(t-1) + FIRMDEATH_FIRMS(t)
# DHS rate:  births / average(FIRM(t), FIRM(t-1))
# =============================================================================
parse_census <- function(resp) {
  if (is.null(resp)) return(NULL)
  ok <- tryCatch(!http_error(resp), error = function(e) FALSE)
  if (!ok) return(NULL)
  txt <- content(resp, "text", encoding = "UTF-8")
  mat <- tryCatch(fromJSON(txt), error = function(e) NULL)
  if (is.null(mat) || nrow(mat) < 2) return(NULL)
  df <- as_tibble(mat[-1, ], .name_repair = "minimal")
  names(df) <- mat[1, ]
  df
}

bds_raw <- tryCatch(
  GET(
    "https://api.census.gov/data/timeseries/bds",
    query = list(get = "YEAR,FIRM,FIRMDEATH_FIRMS", NAICS = "00", "for" = "us:*")
  ),
  error = function(e) NULL
)
bds_df <- parse_census(bds_raw)

bds_firm <- NULL
if (!is.null(bds_df) && all(c("YEAR", "FIRM", "FIRMDEATH_FIRMS") %in% names(bds_df))) {
  bds_firm <- bds_df |>
    transmute(
      year   = as.integer(YEAR),
      firms  = as.numeric(FIRM),
      deaths = as.numeric(FIRMDEATH_FIRMS)
    ) |>
    arrange(year) |>
    mutate(
      births = (firms - lag(firms)) + deaths,
      rate   = births / ((firms + lag(firms)) / 2),
      t      = yq(year, 1),
      series = "BDS firms"
    ) |>
    filter(year >= 1993, !is.na(rate))

  base_val <- bds_firm |> filter(year == 2019) |> pull(rate)
  if (length(base_val) > 0 && base_val > 0) {
    bds_firm <- bds_firm |> mutate(idx = rate / base_val)
    message("BDS firms: ", nrow(bds_firm), " obs (",
            min(bds_firm$year), "–", max(bds_firm$year), ")")
  } else {
    bds_firm <- NULL
  }
}
if (is.null(bds_firm)) warning("BDS firm data unavailable")

# =============================================================================
# 3.  BDS — single-unit firms, quarterly (Census BDSQ)
#     This endpoint is not publicly available; skip with a note.
# =============================================================================
bds_single <- NULL
message("Note: BDS quarterly single-unit data requires the Census BDSQ product,",
        " which is not available via the public Census API.")

# =============================================================================
# 4.  BED — establishment births (quarterly SA) from BLS API v2
#     Series: BDS0000000000000000110003LQ5
#     Registration key provides 10-year window; fetch in three chunks.
# =============================================================================
BLS_KEY  <- "af3787ed391040f4b3cb50bd3679db35"
BED_QSID <- "BDS0000000000000000110003LQ5"
BLS_URL  <- paste0("https://api.bls.gov/publicAPI/v2/timeseries/data/?registrationkey=", BLS_KEY)

fetch_bls_v2 <- function(series_id, start_year, end_year) {
  body <- toJSON(
    list(seriesid = list(series_id),
         startyear = as.character(start_year),
         endyear   = as.character(end_year)),
    auto_unbox = TRUE
  )
  resp <- tryCatch(
    POST(BLS_URL, body = body,
         add_headers("Content-Type" = "application/json"),
         timeout(45)),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  parsed <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                     error = function(e) NULL)
  if (is.null(parsed) || parsed$status != "REQUEST_SUCCEEDED") return(NULL)
  s <- parsed$Results$series
  if (is.data.frame(s)) s$data[[1]] else if (is.list(s)) s[[1]]$data else NULL
}

message("Downloading BLS BED quarterly establishment births…")
bed_chunks <- list(
  fetch_bls_v2(BED_QSID, 1993, 2002),
  fetch_bls_v2(BED_QSID, 2003, 2012),
  fetch_bls_v2(BED_QSID, 2013, 2023),
  fetch_bls_v2(BED_QSID, 2023, 2025)
)

bed_raw <- bind_rows(Filter(Negate(is.null), bed_chunks)) |>
  distinct()

bed_estab <- NULL
if (nrow(bed_raw) > 0) {
  bed_estab <- bed_raw |>
    filter(grepl("^Q", period)) |>
    transmute(
      year    = as.integer(year),
      quarter = as.integer(sub("Q0?", "", period)),
      value   = suppressWarnings(as.numeric(value)),
      t       = yq(year, quarter),
      series  = "BED establishments"
    ) |>
    filter(!is.na(value), year >= 1993) |>
    distinct(t, .keep_all = TRUE) |>
    arrange(t)

  base_val <- bed_estab |> filter(year == 2019, quarter == 1) |> pull(value)
  if (length(base_val) > 0 && base_val > 0) {
    bed_estab <- bed_estab |> mutate(idx = value / base_val)
    message("BED establishments: ", nrow(bed_estab), " obs (",
            min(bed_estab$year), "Q", bed_estab$quarter[which.min(bed_estab$t)],
            "–", max(bed_estab$year), "Q", bed_estab$quarter[which.max(bed_estab$t)], ")")
  } else {
    bed_estab <- NULL
    message("BED estab: no 2019:Q1 base value")
  }
} else {
  message("BED data unavailable — all chunks failed")
}

# =============================================================================
# 5.  BED "firms" — annual approximation from quarterly BED
#     Annual establishment births (sum of quarterly) indexed to 2019 = 1.
#     This approximates BED annual firm births (closely tracks single-unit firms).
# =============================================================================
bed_firms <- NULL
if (!is.null(bed_estab)) {
  bed_firms <- bed_estab |>
    group_by(year) |>
    filter(n() == 4) |>            # only complete calendar years
    summarise(value_ann = sum(value), .groups = "drop") |>
    mutate(t = yq(year, 1), series = "BED firms")

  base_val <- bed_firms |> filter(year == 2019) |> pull(value_ann)
  if (length(base_val) > 0 && base_val > 0) {
    bed_firms <- bed_firms |> mutate(idx = value_ann / base_val)
    message("BED firms (annual approx): ", nrow(bed_firms), " obs (",
            min(bed_firms$year), "–", max(bed_firms$year), ")")
  } else {
    bed_firms <- NULL
  }
}

# =============================================================================
# 6.  Save intermediate data
# =============================================================================
saveRDS(
  list(bfs      = bfs_q,
       bds_firm  = bds_firm,
       bds_single = bds_single,
       bed_estab  = bed_estab,
       bed_firms  = bed_firms),
  file.path(ROOT, "Data", "Output", "entry_validation_data.rds")
)

# =============================================================================
# 7.  Combine all available series
# =============================================================================
all_series <- list(bfs_q, bds_firm, bds_single, bed_estab, bed_firms)
plot_df <- all_series |>
  purrr::keep(~ !is.null(.x) && nrow(.x) > 0 && "idx" %in% names(.x)) |>
  purrr::map(~ .x |> select(t, idx, series)) |>
  bind_rows()

available <- unique(plot_df$series)
message("Series in plot: ", paste(available, collapse = ", "))

# =============================================================================
# 8.  Plot — matching D&H (2023) Figure 2 aesthetics
# =============================================================================
# Colour/linetype map matching the original figure
series_spec <- tribble(
  ~series,                  ~colour,   ~linetype,   ~lwd,
  "BDS firms",              "#1f77b4", "dotted",    1.05,
  "BED establishments",     "#e07b00", "dashed",    0.90,
  "BED firms",              "#222222", "dotdash",   0.85,
  "BFS applications",       "#111111", "longdash",  0.85,
  "BDS single-unit firms",  "#d62728", "solid",     0.85
) |>
  filter(series %in% available)

fig2 <- ggplot() +
  # NBER recession shading
  geom_rect(
    data        = recessions,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill        = "grey80",
    alpha       = 0.55,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 1, colour = "grey55", linewidth = 0.35) +
  geom_line(
    data = plot_df,
    aes(x = t, y = idx, colour = series, linetype = series, linewidth = series)
  ) +
  scale_colour_manual(
    values = setNames(series_spec$colour,   series_spec$series),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = setNames(series_spec$linetype, series_spec$series),
    name   = NULL
  ) +
  scale_linewidth_manual(
    values = setNames(series_spec$lwd,      series_spec$series),
    name   = NULL
  ) +
  scale_x_continuous(
    breaks = c(1996, 1999, seq(2002, 2023, 3)),
    limits = c(1993.5, 2025),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0.7, 1.6, 0.1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title   = "Figure 2.  New Business Entry and New Business Applications",
    x       = NULL,
    y       = "Entry rate indexes (2019:Q1=1)",
    caption = paste0(
      "Sources: Business Dynamics Statistics (BDS, Census Bureau); Business Employment Dynamics (BED, BLS); ",
      "Business Formation Statistics (BFS, Census Bureau).\n",
      "Note: BDS firm birth rate = (FIRM\u2093 \u2212 FIRM\u2093\u208b\u2081 + FIRMDEATH\u2093) / avg(FIRM). ",
      "BED quarterly: SA establishment births (series BDS0000000000000000110003LQ5). ",
      "BED annual = sum of quarterly SA births (approximates BED firm births). ",
      "BFS = likely-employer high-propensity applications (HBA, SA). ",
      "Gray bars = NBER recessions. ",
      "BDS single-unit quarterly not available via API."
    )
  ) +
  theme_paper +
  guides(
    colour    = guide_legend(nrow = 2),
    linetype  = guide_legend(nrow = 2),
    linewidth = guide_legend(nrow = 2)
  )

out_path <- file.path(ROOT, "Draft", "fig2_entry_validation.pdf")
ggsave(out_path, fig2, width = 7.5, height = 5, device = "pdf")
message("Saved: ", out_path)

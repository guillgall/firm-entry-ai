# =============================================================================
# 06_entry_validation.R
# Download BDS (Census) data and plot alongside BFS to replicate the
# Decker & Haltiwanger (2023) Figure 2 comparison of entry measures.
#
# Data sources:
#   BDS  – Business Dynamics Statistics, Census Bureau (annual, via API)
#   BFS  – Business Formation Statistics, already in bfs_panel.rds
#
# BED  – Business Employment Dynamics, BLS (quarterly, via BLS Public Data API v2)
# =============================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(here)

ROOT <- here::here()

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 11)
  )

# ---------------------------------------------------------------------------
# 1. BDS – establishment entry rate, annual (Census API)
# ---------------------------------------------------------------------------
bds_raw <- GET(
  "https://api.census.gov/data/timeseries/bds",
  query = list(
    get   = "YEAR,ESTABS_ENTRY,ESTABS_ENTRY_RATE,ESTABS_EXIT_RATE,FIRM,FIRMDEATH_FIRMS",
    NAICS = "00",
    "for" = "us:*"
  )
)
stop_for_status(bds_raw)

bds_mat <- fromJSON(content(bds_raw, "text", encoding = "UTF-8"))
bds <- as_tibble(bds_mat[-1, ], .name_repair = "minimal")
names(bds) <- bds_mat[1, ]

bds <- bds |>
  mutate(
    year             = as.integer(YEAR),
    estabs_entry     = as.numeric(ESTABS_ENTRY),
    estabs_entry_rate = as.numeric(ESTABS_ENTRY_RATE),
    estabs_exit_rate  = as.numeric(ESTABS_EXIT_RATE),
    firms            = as.numeric(FIRM),
    firm_deaths      = as.numeric(FIRMDEATH_FIRMS),
    firm_entry_rate  = 100 * firm_deaths / firms   # approximate: use deaths as proxy for entry rate symmetry
  ) |>
  filter(year >= 2000) |>
  select(year, estabs_entry, estabs_entry_rate, estabs_exit_rate, firms, firm_deaths)

message("BDS downloaded: ", nrow(bds), " annual observations (", min(bds$year), "-", max(bds$year), ")")
saveRDS(bds, file.path(ROOT, "Data", "Output", "bds_annual.rds"))

# ---------------------------------------------------------------------------
# 2. BFS – monthly HBA applications (high-propensity; closest to likely employers)
#    Aggregate to annual for comparison with BDS
# ---------------------------------------------------------------------------
bfs <- readRDS(file.path(ROOT, "Data", "Output", "bfs_panel.rds"))

bfs_annual <- bfs |>
  filter(naics2 == "US", !is.na(hba)) |>
  mutate(year = as.integer(format(date, "%Y"))) |>
  group_by(year) |>
  summarise(hba_annual = sum(hba, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 2000)

# ---------------------------------------------------------------------------
# 3. Index both series to 2019 = 1 (matching D&H Figure 2)
# ---------------------------------------------------------------------------
base_year <- 2019

bds_base <- bds |> filter(year == base_year) |> pull(estabs_entry_rate)
bfs_base  <- bfs_annual |> filter(year == base_year) |> pull(hba_annual)

plot_df <- bds |>
  select(year, estabs_entry_rate) |>
  mutate(bds_idx = estabs_entry_rate / bds_base) |>
  inner_join(
    bfs_annual |> mutate(bfs_idx = hba_annual / bfs_base) |> select(year, bfs_idx),
    by = "year"
  ) |>
  pivot_longer(c(bds_idx, bfs_idx), names_to = "series", values_to = "index") |>
  mutate(
    series = recode(series,
      bds_idx = "BDS establishment entry rate",
      bfs_idx = "BFS high-propensity applications"
    )
  )

# ---------------------------------------------------------------------------
# 4. BED (Business Employment Dynamics, BLS) – quarterly establishment births
# ---------------------------------------------------------------------------
BLS_KEY <- "af3787ed391040f4b3cb50bd3679db35"

bed_series <- c(
  "BDU00000000000000000085",   # Opening establishments, private, national
  "BDU00000000000000000095"    # Closing establishments, private, national
)

bed_raw <- POST(
  "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body = toJSON(list(
    seriesid        = bed_series,
    startyear       = "2000",
    endyear         = "2024",
    registrationkey = BLS_KEY
  )),
  content_type_json()
)
stop_for_status(bed_raw)

bed_json <- fromJSON(content(bed_raw, "text", encoding = "UTF-8"))

if (bed_json$status != "REQUEST_SUCCEEDED") {
  warning("BLS API returned status: ", bed_json$status,
          "\nMessages: ", paste(bed_json$message, collapse = "; "))
}

# Parse all series into a single tibble.
# Re-parse with simplifyVector=FALSE so the entire response is pure nested lists.
bed_list <- fromJSON(content(bed_raw, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)

# ---- Diagnostics: print structure so we can verify field names ----
message("BLS status : ", bed_list$status)
if (length(bed_list$message) > 0)
  message("BLS messages: ", paste(unlist(bed_list$message), collapse = "; "))

s1   <- bed_list$Results$series[[1]]
row1 <- s1$data[[1]]
message("Series[[1]] field names  : ", paste(names(s1),   collapse = ", "))
message("data[[1]]   field names  : ", paste(names(row1), collapse = ", "))
message("data[[1]] raw (str):\n",
        paste(capture.output(str(row1, max.level = 1)), collapse = "\n"))
# -------------------------------------------------------------------

bed_df <- map_dfr(bed_list$Results$series, function(s) {
  map_dfr(s$data, function(row) {
    tibble(
      seriesID = s[["seriesID"]],
      year     = row[["year"]],
      period   = row[["period"]],
      value    = row[["value"]]
    )
  })
}) |>
  mutate(
    year  = as.integer(year),
    value = as.numeric(value)
  ) |>
  filter(!period %in% c("Q05", "A01"))  # drop any annual-total rows

# Opening establishments (series …085), summed to annual
bed_opens <- bed_df |>
  filter(seriesID == "BDU00000000000000000085") |>
  group_by(year) |>
  summarise(bed_opens_annual = sum(value, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 2000)

message("BED downloaded: ", nrow(bed_opens), " annual observations (",
        min(bed_opens$year), "-", max(bed_opens$year), ")")
saveRDS(bed_opens, file.path(ROOT, "Data", "Output", "bed_annual.rds"))

# ---------------------------------------------------------------------------
# 5. Add BED to indexed plot
# ---------------------------------------------------------------------------
bed_base <- bed_opens |> filter(year == base_year) |> pull(bed_opens_annual)

plot_df <- plot_df |>
  bind_rows(
    bed_opens |>
      mutate(
        index  = bed_opens_annual / bed_base,
        series = "BED opening establishments"
      ) |>
      select(year, index, series)
  )

fig_validation <- ggplot(plot_df, aes(x = year, y = index, colour = series, linetype = series)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = 2022.2, y = Inf, label = "ChatGPT\nlaunch",
           vjust = 1.4, hjust = 0, size = 2.8, colour = "grey30") +
  scale_colour_manual(
    values = c(
      "BDS establishment entry rate"     = "#1f77b4",
      "BFS high-propensity applications" = "#d62728",
      "BED opening establishments"       = "#2ca02c"
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "BDS establishment entry rate"     = "solid",
      "BFS high-propensity applications" = "dashed",
      "BED opening establishments"       = "dotdash"
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title   = "Business Entry Measures (2019 = 1)",
    x       = NULL,
    y       = "Index (2019 = 1)",
    caption = paste(
      "Sources: Census Bureau Business Dynamics Statistics (BDS);",
      "Census Bureau Business Formation Statistics (BFS);",
      "Bureau of Labor Statistics Business Employment Dynamics (BED)."
    )
  ) +
  theme_paper +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 7, hjust = 0, colour = "grey40")
  )

ggsave(file.path(ROOT, "Draft", "fig_validation.pdf"), fig_validation,
       width = 7, height = 4, device = "pdf")
message("Saved: Draft/fig_validation.pdf (updated with BED)")

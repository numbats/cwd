library(forecast)
library(gt)
library(gtExtras)
library(dplyr)
library(tibble)
library(zoo)

df_q <- tibble(
  yq    = as.yearqtr(time(woolyrnq)),
  value = as.numeric(woolyrnq)
) |>
  mutate(
    year   = as.integer(format(yq, "%Y")),
    decade = paste0(floor(year/10) * 10, "s")
  )

# --- Collapse to one row per decade with sparkline + 5-number summary ---
decades <- df_q |>
  group_by(decade) |>
  summarise(
    spark  = list(value),                         # quarterly series for the decade
    latest = dplyr::last(value),                  # last obs in the decade
    min    = min(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE) |> round() |> formatC(big.mark = ","),
    q3 = quantile(value, 0.75, na.rm = TRUE) |> round() |> formatC(big.mark = ","),
    median = quantile(value, 0.50, na.rm = TRUE) |> round() |> formatC(big.mark = ","),
    max    = max(value, na.rm = TRUE),
    median_string = paste0(median, "<br>(", q1, ", ", q3, ")"),
    .groups = "drop"
  )

# --- Build gt table: sparkline + 5-number summary ---
decades |>
  select(decade, spark, latest, min, max, median_string) |>
  gt() |>
  gt_plt_sparkline(
    spark
  ) |>
  tab_header(
    title = md("**Australian wool yarn production — sparklines by decade**"),
    subtitle = "Sparkline: quarterly values within each decade · 5-number summary at right"
  ) |>
  cols_label(
    decade = md("**Decade**"),
    spark  = "",
    latest = md("**Last**"),
    min = md("**Min**"),
    median_string = md("**Median**"),
    max    = md("**Max**")
  ) |>
  fmt_number(c(latest, min, max), decimals = 0) |>
  cols_align(align = "left", columns = decade) |>
  fmt_markdown(columns = median_string) |>
  cols_align(align = "center", columns = median_string)


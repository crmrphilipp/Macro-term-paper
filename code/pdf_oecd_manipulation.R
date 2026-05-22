# ============================================================
# Build OECD panel regression data
# ============================================================

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(tidyverse)
library(stargazer)
library(readr)
library(WDI)
library(readxl)
library(kableExtra)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. Load raw data
# ============================================================

pdf_oecd_raw <- read.csv(
  "../data/oecd_master.csv",
  na.strings = c("", "NA", "NaN", "Invalid Number", "Invalid number")
)

# GDP/GNI/cons extracted in billions for these countries
BILLION_NC <- c("JPN", "USA", "TUR")


# ============================================================
# 2. Create PPP and real values
# ============================================================

# Extract year-2000 conversion rates per country
rates_2000 <- pdf_oecd_raw |>
  filter(year == 2000) |>
  select(
    iso,
    xr_2000 = xr_c1,
    ppp_gdp_2000 = ppp_gdp_c2,
    ppp_cons_2000 = ppp_cons_c3
  )

pdf_oecd <- pdf_oecd_raw |>
  left_join(rates_2000, by = "iso") |>
  mutate(
    scale = if_else(iso %in% BILLION_NC, 1000000000, 1000000),
    
    gdp_usd_xr   = (gdp_nc  / xr_2000) * scale,
    gdp_usd_ppp  = (gdp_nc  / ppp_gdp_2000) * scale,
    gni_usd_ppp  = (gni_nc  / ppp_gdp_2000) * scale,
    
    # I keep your original line here, but note:
    # if this is used later, it probably also needs * scale.
    cons_usd_xr  = (cons_nc / xr_2000),
    
    cons_usd_ppp = (cons_nc / ppp_cons_2000) * scale
  ) |>
  select(
    -scale,
    -xr_2000,
    -ppp_gdp_2000,
    -ppp_cons_2000,
    -gni_approx
  )


# ============================================================
# 3. Create checks
# ============================================================

pdf_oecd <- pdf_oecd |>
  mutate(
    gdp_check = 1 - (1000000000 * gdp_a3) / gdp_usd_xr,
    gdp_ppp_check = 1 - (1000000000 * gdp_b3) / gdp_usd_ppp,
    cons_check = 1 - (1000000000 * acons_a4) / cons_usd_xr,
    cons_ppp_check = 1 - (1000000000 * acons_b4) / cons_usd_ppp
  )


# ============================================================
# 4. Optional diagnostic plots
# ============================================================

p1 <- ggplot(pdf_oecd, aes(x = factor(year), y = gdp_ppp_check)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, fill = "#4E79A7", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "firebrick", linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "GDP PPP check",
    subtitle = "1 − (USD bn ref / computed), should hover around 0",
    x = NULL,
    y = "Relative difference"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(pdf_oecd, aes(x = factor(year), y = cons_ppp_check)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, fill = "#F28E2B", alpha = 0.6) +
  geom_hline(yintercept = 0.10, linetype = "dashed", colour = "firebrick", linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Consumption PPP check",
    subtitle = "1 − (USD bn ref / computed), hovers ~10% due to narrower acons definition",
    x = NULL,
    y = "Relative difference"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ============================================================
# 5. Create per-capita values
# ============================================================
# population is in thousands, so multiply by 1000

pdf_oecd <- pdf_oecd |>
  mutate(
    gdp_usd_ppp_pc  = gdp_usd_ppp  / (pop_c4 * 1000),
    gni_usd_ppp_pc  = gni_usd_ppp  / (pop_c4 * 1000),
    cons_usd_ppp_pc = cons_usd_ppp / (pop_c4 * 1000),
    
    acons_a4_pc = (acons_a4 * 1000000000) / (pop_c4 * 1000),
    acons_b4_pc = (acons_b4 * 1000000000) / (pop_c4 * 1000),
    gdp_b3_pc   = (gdp_b3   * 1000000000) / (pop_c4 * 1000)
  )


# ============================================================
# 6. Create country-level growth rates using delta log
# ============================================================

pdf_oecd <- pdf_oecd |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(
    d_log_gdp_ppp  = log(gdp_usd_ppp_pc)  - dplyr::lag(log(gdp_usd_ppp_pc)),
    d_log_gni_ppp  = log(gni_usd_ppp_pc)  - dplyr::lag(log(gni_usd_ppp_pc)),
    d_log_cons_ppp = log(cons_usd_ppp_pc) - dplyr::lag(log(cons_usd_ppp_pc)),
    
    d_log_acons_a4 = log(acons_a4_pc) - dplyr::lag(log(acons_a4_pc)),
    d_log_acons_b4 = log(acons_b4_pc) - dplyr::lag(log(acons_b4_pc)),
    d_log_gdp_b3   = log(gdp_b3_pc)   - dplyr::lag(log(gdp_b3_pc))
  ) |>
  ungroup()


# ============================================================
# 7. Create true-value variables
# ============================================================

pdf_oecd <- pdf_oecd |>
  mutate(
    gdp_b3_adj   = gdp_b3   * 1000000000,
    acons_b4_adj = acons_b4 * 1000000000,
    pop_c4_adj   = pop_c4   * 1000
  )


# ============================================================
# 8. Export data for external function
# ============================================================

pdf_oecd_reg <- pdf_oecd |>
  select(
    iso,
    country,
    year,
    gdp_b3_adj,
    acons_b4_adj,
    pop_c4_adj,
    gdp_usd_ppp,
    cons_usd_ppp,
    gni_usd_ppp
  )

write.csv(
  pdf_oecd_reg,
  "../data/pdf_oecd_reg.csv",
  row.names = FALSE
)


# ============================================================
# 9. Function for robust aggregate per-capita growth rates
# ============================================================
# Important:
# This computes aggregate growth using only countries that have
# valid values in both t and t-1.
#
# This fixes the CHE/MEX GNI problem:
# if GNI is missing in 2003 for CHE/MEX, their population is also
# excluded from the 2003 GNI aggregate and from the corresponding
# 2002 lag aggregate used for the 2002-2003 growth rate.

make_agg_growth <- function(data, value_col, pop_col, output_name) {
  
  data |>
    arrange(iso, year) |>
    group_by(iso) |>
    mutate(
      value_now = .data[[value_col]],
      pop_now   = .data[[pop_col]],
      value_lag = dplyr::lag(.data[[value_col]]),
      pop_lag   = dplyr::lag(.data[[pop_col]])
    ) |>
    ungroup() |>
    filter(
      is.finite(value_now),
      is.finite(value_lag),
      is.finite(pop_now),
      is.finite(pop_lag),
      value_now > 0,
      value_lag > 0,
      pop_now > 0,
      pop_lag > 0
    ) |>
    group_by(year) |>
    summarise(
      agg_growth =
        log(sum(value_now) / sum(pop_now)) -
        log(sum(value_lag) / sum(pop_lag)),
      n_countries = n_distinct(iso),
      .groups = "drop"
    ) |>
    rename(
      !!output_name := agg_growth,
      !!paste0("n_", output_name) := n_countries
    )
}


# ============================================================
# 10. Build aggregate growth rates
# ============================================================

agg_gdp <- make_agg_growth(
  data = pdf_oecd,
  value_col = "gdp_usd_ppp",
  pop_col = "pop_c4_adj",
  output_name = "agg_d_log_gdp_ppp"
)

agg_gni <- make_agg_growth(
  data = pdf_oecd,
  value_col = "gni_usd_ppp",
  pop_col = "pop_c4_adj",
  output_name = "agg_d_log_gni_ppp"
)

agg_cons <- make_agg_growth(
  data = pdf_oecd,
  value_col = "cons_usd_ppp",
  pop_col = "pop_c4_adj",
  output_name = "agg_d_log_cons_ppp"
)

agg_acons_b4 <- make_agg_growth(
  data = pdf_oecd,
  value_col = "acons_b4_adj",
  pop_col = "pop_c4_adj",
  output_name = "agg_d_log_acons_b4"
)

agg_gdp_b3 <- make_agg_growth(
  data = pdf_oecd,
  value_col = "gdp_b3_adj",
  pop_col = "pop_c4_adj",
  output_name = "agg_d_log_gdp_b3"
)


# ============================================================
# 11. Join all aggregate growth rates
# ============================================================

agg_levels <- agg_gdp |>
  left_join(agg_gni, by = "year") |>
  left_join(agg_cons, by = "year") |>
  left_join(agg_acons_b4, by = "year") |>
  left_join(agg_gdp_b3, by = "year") |>
  as_tibble()


# ============================================================
# 12. Safety: remove possible time-series classes
# ============================================================
# This avoids the error:
# invalid time series parameters specified (1)

pdf_oecd <- pdf_oecd |>
  as_tibble() |>
  ungroup() |>
  mutate(
    across(
      c(
        d_log_gdp_ppp,
        d_log_gni_ppp,
        d_log_cons_ppp,
        d_log_acons_b4,
        d_log_gdp_b3
      ),
      ~ as.numeric(.)
    )
  )

agg_levels <- agg_levels |>
  as_tibble() |>
  mutate(
    across(
      starts_with("agg_d_log"),
      ~ as.numeric(.)
    )
  )


# ============================================================
# 13. Join aggregate growth rates and compute deviations
# ============================================================

pdf_oecd <- pdf_oecd |>
  left_join(
    agg_levels |>
      select(year, starts_with("agg_d_log")),
    by = "year"
  ) |>
  mutate(
    dev_gdp_ppp  = as.numeric(d_log_gdp_ppp)  - as.numeric(agg_d_log_gdp_ppp),
    dev_gni_ppp  = as.numeric(d_log_gni_ppp)  - as.numeric(agg_d_log_gni_ppp),
    dev_cons_ppp = as.numeric(d_log_cons_ppp) - as.numeric(agg_d_log_cons_ppp),
    dev_acons_b4 = as.numeric(d_log_acons_b4) - as.numeric(agg_d_log_acons_b4),
    dev_gdp_b3   = as.numeric(d_log_gdp_b3)   - as.numeric(agg_d_log_gdp_b3)
  )


# ============================================================
# 14. Create panel regression export
# ============================================================

pdf_oecd_panel_reg <- pdf_oecd |>
  select(
    iso,
    country,
    year,
    
    gdp_usd_ppp,
    gni_usd_ppp,
    cons_usd_ppp,
    
    gdp_b3_adj,
    acons_b4_adj,
    pop_c4_adj,
    
    d_log_gdp_ppp,
    d_log_gni_ppp,
    d_log_cons_ppp,
    d_log_acons_b4,
    d_log_gdp_b3,
    
    agg_d_log_gdp_ppp,
    agg_d_log_gni_ppp,
    agg_d_log_cons_ppp,
    agg_d_log_acons_b4,
    agg_d_log_gdp_b3,
    
    dev_gdp_ppp,
    dev_gni_ppp,
    dev_cons_ppp,
    dev_acons_b4,
    dev_gdp_b3
  )


# ============================================================
# 15. Save output
# ============================================================

write.csv(
  pdf_oecd_panel_reg,
  "../data/pdf_oecd_panel_reg.csv",
  row.names = FALSE
)


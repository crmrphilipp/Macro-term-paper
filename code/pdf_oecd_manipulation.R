rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pdf_oecd_raw<-read.csv("../data/oecd_master.csv")

library(dplyr)
library(tidyr)
library(tidyverse)
library(stargazer)
library(readr)
library(WDI)
library(readxl)
library(kableExtra)

# gdps extracted in billions
BILLION_NC <- c("JPN", "USA", "TUR")   


# create ppp and real values
# Extract year-2000 conversion rates per country
rates_2000 <- pdf_oecd_raw |>
  filter(year == 2000) |>
  select(iso, xr_2000 = xr_c1, ppp_gdp_2000 = ppp_gdp_c2, ppp_cons_2000 = ppp_cons_c3)

pdf_oecd <- pdf_oecd_raw |>
  left_join(rates_2000, by = "iso") |>
  mutate(
    scale        = if_else(iso %in% BILLION_NC, 1000000000, 1000000),
    
    gdp_usd_xr   = (gdp_nc  / xr_2000)*scale,
    gdp_usd_ppp  = (gdp_nc  / ppp_gdp_2000)*scale,
    gni_usd_ppp  = (gni_nc  / ppp_gdp_2000)*scale,
    cons_usd_xr  = (cons_nc / xr_2000),
    cons_usd_ppp = (cons_nc / ppp_cons_2000)*scale
  ) |>
  select(-scale, -xr_2000, -ppp_gdp_2000, -ppp_cons_2000,-gni_approx)

# create checks
pdf_oecd <- pdf_oecd |>
  mutate(gdp_check = 1-(1000000000*gdp_a3)/gdp_usd_xr,
         gdp_ppp_check = 1-(1000000000*gdp_b3)/gdp_usd_ppp,
         cons_check = 1-(1000000000*acons_a4)/cons_usd_xr,
         cons_ppp_check = 1-(1000000000*acons_b4)/cons_usd_ppp
         )

library(ggplot2)
library(patchwork)

p1 <- ggplot(pdf_oecd, aes(x = factor(year), y = gdp_ppp_check)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, fill = "#4E79A7", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "firebrick", linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "GDP PPP check",
    subtitle = "1 − (USD bn ref / computed), should hover around 0",
    x = NULL, y = "Relative difference"
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
    x = NULL, y = "Relative difference"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# create pc values
# population is in 1000
pdf_oecd <- pdf_oecd |>
  mutate(
    gdp_usd_ppp_pc = gdp_usd_ppp / (pop_c4*1000),
    gni_usd_ppp_pc = gni_usd_ppp / (pop_c4*1000),
    cons_usd_ppp_pc = cons_usd_ppp / (pop_c4*1000),
    acons_a4_pc  = (acons_a4*1000000000)     / (pop_c4*1000),
    acons_b4_pc  = (acons_b4*1000000000)     / (pop_c4*1000),
    gdp_b3_pc    = (gdp_b3*1000000000) / (pop_c4*1000)        
  )

# create growth rates using delta log
pdf_oecd <- pdf_oecd |>
  group_by(iso) |>
  arrange(year) |>
  mutate(
    d_log_gdp_ppp  = log(gdp_usd_ppp_pc)  - lag(log(gdp_usd_ppp_pc)),
    d_log_gni_ppp  = log(gni_usd_ppp_pc)  - lag(log(gni_usd_ppp_pc)),
    d_log_cons_ppp = log(cons_usd_ppp_pc)  - lag(log(cons_usd_ppp_pc)),
    d_log_acons_a4 = log(acons_a4_pc)      - lag(log(acons_a4_pc)),
    d_log_acons_b4 = log(acons_b4_pc)      - lag(log(acons_b4_pc)),
    d_log_gdp_b3   = log(gdp_b3_pc)        - lag(log(gdp_b3_pc))
  ) |>
  ungroup()

# all in true values
pdf_oecd <- pdf_oecd |>
  mutate(gdp_b3_adj=gdp_b3*1000000000, acons_b4_adj=acons_b4*1000000000, pop_c4_adj=pop_c4*1000
  )


# export data for felix function
pdf_oecd_reg <- pdf_oecd |>
  select(iso, country, year, gdp_b3_adj, acons_b4_adj, pop_c4_adj, gdp_usd_ppp, cons_usd_ppp, gni_usd_ppp)

write.csv(pdf_oecd_reg, "../data/pdf_oecd_reg.csv")


# aggreagate growth rates and deviation

# Aggregate levels (sum across countries by year)
agg_levels <- pdf_oecd |>
  group_by(year) |>
  summarise(
    agg_gdp_usd_ppp  = sum(gdp_usd_ppp,  na.rm = TRUE),
    agg_gni_usd_ppp  = sum(gni_usd_ppp,  na.rm = TRUE),
    agg_cons_usd_ppp = sum(cons_usd_ppp,  na.rm = TRUE),
    agg_acons_b4     = sum(acons_b4_adj,      na.rm = TRUE),
    agg_gdp_b3       = sum(gdp_b3_adj,        na.rm = TRUE),
    agg_pop          = sum(pop_c4_adj,        na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate per capita
agg_levels <- agg_levels |>
  mutate(
    agg_gdp_usd_ppp_pc  = agg_gdp_usd_ppp  / agg_pop,
    agg_gni_usd_ppp_pc  = agg_gni_usd_ppp  / agg_pop,
    agg_cons_usd_ppp_pc = agg_cons_usd_ppp / agg_pop,
    agg_acons_b4_pc     = agg_acons_b4     / agg_pop,
    agg_gdp_b3_pc       = agg_gdp_b3       / agg_pop
  )

#Aggregate growth rates (delta log of per capita aggregates)
agg_levels <- agg_levels |>
  arrange(year) |>
  mutate(
    agg_d_log_gdp_ppp  = log(agg_gdp_usd_ppp_pc)  - lag(log(agg_gdp_usd_ppp_pc)),
    agg_d_log_gni_ppp  = log(agg_gni_usd_ppp_pc)  - lag(log(agg_gni_usd_ppp_pc)),
    agg_d_log_cons_ppp = log(agg_cons_usd_ppp_pc)  - lag(log(agg_cons_usd_ppp_pc)),
    agg_d_log_acons_b4 = log(agg_acons_b4_pc)      - lag(log(agg_acons_b4_pc)),
    agg_d_log_gdp_b3   = log(agg_gdp_b3_pc)        - lag(log(agg_gdp_b3_pc))
  )

# Join aggregate growth rates back and compute deviations
pdf_oecd <- pdf_oecd |>
  left_join(
    agg_levels |> select(year, starts_with("agg_d_log")),
    by = "year"
  ) |>
  mutate(
    dev_gdp_ppp  = d_log_gdp_ppp  - agg_d_log_gdp_ppp,
    dev_gni_ppp  = d_log_gni_ppp  - agg_d_log_gni_ppp,
    dev_cons_ppp = d_log_cons_ppp - agg_d_log_cons_ppp,
    dev_acons_b4 = d_log_acons_b4 - agg_d_log_acons_b4,
    dev_gdp_b3   = d_log_gdp_b3   - agg_d_log_gdp_b3   
  )

pdf_oecd_panel_reg <- pdf_oecd |>
  select(iso, country,year, gdp_usd_ppp, gni_usd_ppp, cons_usd_ppp, gdp_b3_adj, acons_b4_adj, pop_c4_adj, d_log_gdp_ppp, d_log_gni_ppp, d_log_cons_ppp, d_log_acons_b4, d_log_gdp_b3, agg_d_log_gdp_ppp, agg_d_log_gni_ppp, agg_d_log_cons_ppp, agg_d_log_acons_b4, agg_d_log_gdp_b3, dev_gdp_ppp, dev_gni_ppp, dev_cons_ppp, dev_acons_b4, dev_gdp_b3 )

write.csv(pdf_oecd_panel_reg, "../data/pdf_oecd_panel_reg.csv")

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
    scale        = if_else(iso %in% BILLION_NC, 1, 1000),
    
    gdp_usd_xr   = gdp_nc  / xr_2000       / scale *1000000000,
    gdp_usd_ppp  = gdp_nc  / ppp_gdp_2000  / scale *1000000000,
    gni_usd_ppp  = gni_nc  / ppp_gdp_2000  / scale *1000000000,
    cons_usd_xr  = cons_nc / xr_2000       / scale *1000000000,
    cons_usd_ppp = cons_nc / ppp_cons_2000  / scale *1000000000
  ) |>
  select(-scale, -xr_2000, -ppp_gdp_2000, -ppp_cons_2000)
# create checks
pdf_oecd <- pdf_oecd |>
  mutate(gdp_check = 1-gdp_a3/gdp_usd_xr,
         gdp_ppp_check = 1-gdp_b3/gdp_usd_ppp,
         cons_check = 1-acons_a4/cons_usd_xr,
         cons_ppp_check = 1-acons_b4/cons_usd_ppp
         )

# create pc values
pdf_oecd <- pdf_oecd |>
  mutate(
    gdp_nc_pc    = gdp_nc       / pop_c4,
    gni_nc_pc    = gni_nc       / pop_c4,
    cons_nc_pc   = cons_nc      / pop_c4,
    gdp_usd_xr_pc  = gdp_usd_xr  / pop_c4,
    gdp_usd_ppp_pc = gdp_usd_ppp / pop_c4,
    gni_usd_ppp_pc = gni_usd_ppp / pop_c4,
    cons_usd_xr_pc  = cons_usd_xr  / pop_c4,
    cons_usd_ppp_pc = cons_usd_ppp / pop_c4,
    acons_a4_pc  = acons_a4     / pop_c4,
    acons_b4_pc  = acons_b4     / pop_c4,
    gdp_b3_pc    = gdp_b3 / pop_c4        
  )

# create growth rates using delta log
pdf_oecd <- pdf_oecd |>
  group_by(iso) %>%
  arrange(year) %>%
  mutate(
    d_log_gdp_nc   = log(gdp_nc_pc    / lag(gdp_nc_pc)),
    d_log_gni_nc   = log(gni_nc_pc    / lag(gni_nc_pc)),
    d_log_cons_nc  = log(cons_nc_pc   / lag(cons_nc_pc)),
    d_log_gdp_xr   = log(gdp_usd_xr_pc  / lag(gdp_usd_xr_pc)),
    d_log_gdp_ppp  = log(gdp_usd_ppp_pc / lag(gdp_usd_ppp_pc)),
    d_log_gni_ppp  = log(gni_usd_ppp_pc / lag(gni_usd_ppp_pc)),
    d_log_cons_xr  = log(cons_usd_xr_pc  / lag(cons_usd_xr_pc)),
    d_log_cons_ppp = log(cons_usd_ppp_pc / lag(cons_usd_ppp_pc)),
    d_log_acons_a4 = log(acons_a4_pc  / lag(acons_a4_pc)),
    d_log_acons_b4 = log(acons_b4_pc  / lag(acons_b4_pc)),
    d_log_gdp_b3 = log(gdp_b3_pc / lag(gdp_b3_pc))

  ) %>%
  ungroup()


# export data for felix function
pdf_oecd_reg <- pdf_oecd |>
  select(iso, country, year, gdp_b3, acons_b4, pop_c4, gdp_usd_ppp, cons_usd_ppp, gni_usd_ppp)

write.csv(pdf_oecd_reg, "../data/pdf_oecd_reg.csv")


# aggreagate growth rates and deviation

# Aggregate levels (sum across countries by year)
agg_levels <- pdf_oecd |>
  group_by(year) |>
  summarise(
    agg_gdp_usd_ppp  = sum(gdp_usd_ppp,  na.rm = TRUE),
    agg_gni_usd_ppp  = sum(gni_usd_ppp,  na.rm = TRUE),
    agg_cons_usd_ppp = sum(cons_usd_ppp,  na.rm = TRUE),
    agg_acons_b4     = sum(acons_b4,      na.rm = TRUE),
    agg_gdp_b3       = sum(gdp_b3,        na.rm = TRUE),
    agg_pop          = sum(pop_c4,        na.rm = TRUE),
    .groups = "drop"
  )

# ── STEP 2: Aggregate per capita ─────────────────────────────────────────────
agg_levels <- agg_levels |>
  mutate(
    agg_gdp_usd_ppp_pc  = agg_gdp_usd_ppp  / agg_pop,
    agg_gni_usd_ppp_pc  = agg_gni_usd_ppp  / agg_pop,
    agg_cons_usd_ppp_pc = agg_cons_usd_ppp / agg_pop,
    agg_acons_b4_pc     = agg_acons_b4     / agg_pop,
    agg_gdp_b3_pc       = agg_gdp_b3       / agg_pop
  )

# ── STEP 3: Aggregate growth rates (delta log of per capita aggregates) ───────
agg_levels <- agg_levels |>
  arrange(year) |>
  mutate(
    agg_d_log_gdp_ppp  = log(agg_gdp_usd_ppp_pc  / lag(agg_gdp_usd_ppp_pc)),
    agg_d_log_gni_ppp  = log(agg_gni_usd_ppp_pc  / lag(agg_gni_usd_ppp_pc)),
    agg_d_log_cons_ppp = log(agg_cons_usd_ppp_pc / lag(agg_cons_usd_ppp_pc)),
    agg_d_log_acons_b4 = log(agg_acons_b4_pc     / lag(agg_acons_b4_pc)),
    agg_d_log_gdp_b3   = log(agg_gdp_b3_pc       / lag(agg_gdp_b3_pc))
  )

# ── STEP 4: Join aggregate growth rates back and compute deviations ───────────
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

# ══════════════════════════════════════════════════════════════════════════════
#  
# Home-Bias Recreation
#
# ══════════════════════════════════════════════════════════════════════════════


rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)
library(tidyverse)
library(stargazer)
library(readr)
library(WDI)
library(readxl)
library(kableExtra)

# load data

oecd_data <- read.csv("../data/oecd_small_reg.csv")

oecd_data <- oecd_data %>%
    filter(year>1992, year<2004)

ehb_data <- read.csv("../data/ehb_reg_small.csv")

ehb_crude_data <- read.csv("../data/ehb_crude_reg_small.csv")

merged_df <- list(oecd_data, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 1998)

library(plm)

panel_df <- pdata.frame(merged_df, index = c("iso", "year"))

# Regression 1: Equity Home Bias deviation
reg_ehb <- plm(
  gni_dev ~ 1+ growth_dev + I(time * growth_dev) + I(EHB_dev * growth_dev),
  data   = panel_df,
  model  = "within",   # country fixed effects
  effect = "individual"
)

# Regression 2: Crude home bias (log foreign equity / GDP)
reg_crude <- plm(
  gni_dev ~ 1+ growth_dev + I(time * growth_dev) + I(ehb_crude_dev * growth_dev),
  data   = panel_df,
  model  = "within",
  effect = "individual"
)

summary(reg_ehb)
summary(reg_crude)

library(stargazer)


stargazer(
  reg_ehb, reg_crude,
  type  = "latex",
  out   = "../output/reg_risk_sharing.tex",
  title = "International Risk Sharing and Home Bias: OECD 1993--2003",
  dep.var.labels   = "GNI Growth Deviation",
  covariate.labels = c(
    "GDP Growth Deviation",
    "GDP Deviation $\\times$ Time",
    "GDP Deviation $\\times$ EHB",
    "GDP Deviation $\\times$ Crude HB"
  ),
  add.lines = list(c("Country FE", "Yes", "Yes")),
  omit.stat = c("f", "ser"),
  notes     = "Country fixed effects estimated via within transformation.",
  label     = "tab:risk_sharing"
)


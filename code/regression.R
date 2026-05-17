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

oecd_data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

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
  gni_dev ~ 1+ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data   = panel_df,
  model  = "within",   # country fixed effects
  effect = "individual"
)

# Regression 2: Crude home bias (log foreign equity / GDP)
reg_crude <- plm(
  gni_dev ~ 1+ gdp_dev + I(time * gdp_dev) + I(ehb_crude_dev * gdp_dev),
  data   = panel_df,
  model  = "within",
  effect = "individual"
)

summary(reg_ehb)
summary(reg_crude)

# Check how many obs per country enter the regression
used_obs <- model.frame(reg_ehb)
table(attr(used_obs, "index")$iso)

used_obs_crude <- model.frame(reg_crude)
table(attr(used_obs_crude, "index")$iso)


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



########### Test (16/05/2026): Use OECD Data on GDP and consumption at current (constant) prices and run regression with these values
library(readr)

# 1. Fixed the URL to end with exactly 'format=csv'
test_oecd_sdmx  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_MULT_INDICES,2.0/A.AUS+AUT+BEL+CAN+DNK+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+MEX+NLD+NZL+NOR+PRT+ESP+SWE+CHE+TUR+GBR+USA+OECD...B1GQ_POP.....LR..?startPeriod=1993&endPeriod=2003&dimensionAtObservation=AllDimensions&format=csv"
test_oecd_data <- "../data/test_oecd_data.csv"

# 2. Download the file
if (!file.exists(test_oecd_data)) {
  download.file(test_oecd_sdmx, destfile = test_oecd_data, mode = "wb")
  message("test_oecd dataset downloaded.")
}
 
# 3. Read the CSV without skipping rows
test_oecd_raw <- read_csv(test_oecd_data)

# 4. View the first few rows to confirm it worked
head(test_oecd_raw)



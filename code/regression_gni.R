#### The main regression: GNI Risk Sharing
rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# load data
oecd_data_GNI <- read.csv("../data/data_iy_rep.csv")

# right format
df_oecd_GNI <- oecd_data_GNI %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD, Population),
    names_from = measure,
    values_from = c(OBS_VALUE, OBS_VALUE_per_capita),
    names_glue = "{measure}_{.value}"
  ) %>%
  rename(
    GNI = GNI_OBS_VALUE,
    GDP = GDP_OBS_VALUE,
    GNI_pc = GNI_OBS_VALUE_per_capita,
    GDP_pc = GDP_OBS_VALUE_per_capita,
  )

### Creating deviation variables 

### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

df_oecd_GNI <- df_oecd_GNI %>%
  filter(REF_AREA %in% reg_sample)


#### per capita growth rates: GDP and GNI
df_oecd_GNI <- df_oecd_GNI %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    GNI_log_diff = log(GNI_pc) - log(dplyr::lag(GNI_pc))
  ) %>%
  ungroup()


# aggregate per capita growth rates: GDP and GNI


# common country-year sample: GDP, GNI and Population must all be available

df_oecd_GNI_aggregate2 <- df_oecd_GNI %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_total2 = sum(
      GDP[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    GNI_total2 = sum(
      GNI[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    population_total2 = sum(
      Population[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    gdp_total_pc2 = gdp_total2 / population_total2,
    GNI_total_pc2 = GNI_total2 / population_total2
  ) %>%
  mutate(
    gdp_log_diff_total2 =
      log(gdp_total_pc2) - log(dplyr::lag(gdp_total_pc2)),
    
    GNI_log_diff_total2 =
      log(GNI_total_pc2) - log(dplyr::lag(GNI_total_pc2))
  )


# join the aggregate growth rates
df_oecd_GNI <- df_oecd_GNI %>%
  left_join(
    df_oecd_GNI_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total2, GNI_log_diff_total2),
    by = "TIME_PERIOD"
  )


# growth rate deviations
df_oecd_GNI <- df_oecd_GNI %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total2,
    GNI_dev = GNI_log_diff - GNI_log_diff_total2
  )


# keep regression period
df_oecd_GNI <- df_oecd_GNI %>%
  filter(TIME_PERIOD > 1992, TIME_PERIOD < 2004)


# load home bias data
ehb_data <- read.csv("../data/ehb_reg_small.csv")
ehb_crude_data <- read.csv("../data/ehb_crude_reg_small.csv")


# rename identifiers
df_oecd_GNI <- df_oecd_GNI %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )


# merge data
merged_df_GNI <- list(df_oecd_GNI, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df_GNI <- merged_df_GNI %>%
  mutate(time = year - 1998)


# clean estimation sample
reg_df_GNI <- merged_df_GNI %>%
  filter(
    !is.na(GNI_dev),
    !is.na(gdp_dev),
    !is.na(EHB_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)


# First step regression with country FE
panel_df_GNI <- pdata.frame(reg_df_GNI, index = c("iso", "year"))

reg_GNI_stage1 <- plm(
  GNI_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data   = panel_df_GNI,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_df_GNI$resid_stage1 <- as.numeric(residuals(reg_GNI_stage1))

sigma_by_country <- reg_df_GNI %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_df_GNI_w <- reg_df_GNI %>%
  left_join(sigma_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_df_GNI_w <- pdata.frame(reg_df_GNI_w, index = c("iso", "year"))

reg_GNI_stage2 <- plm(
  GNI_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data    = panel_df_GNI_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)


##############################
#### Creating the tables
##############################
summary(reg_GNI_stage2)

############################################################
#### Export GNI risk sharing regression
############################################################

library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  stop("Output folder not found. Check getwd() and whether ../output exists.")
}

stargazer(
  reg_GNI_stage2,
  type = "latex",
  out = file.path(output_dir, "reg_gni_risk_sharing.tex"),
  title = "Income Risk Sharing and Equity Home Bias",
  dep.var.labels = "GNI Growth Deviation",
  column.labels = c("EHB"),
  covariate.labels = c(
    "GDP Growth Deviation",
    "Time $\\times$ GDP Growth Deviation",
    "Home Bias $\\times$ GDP Growth Deviation"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  header = FALSE
)

file.exists(file.path(output_dir, "reg_gni_risk_sharing.tex"))

############################################################
#### Table 5-style regressions: GNI / Income Risk Sharing
############################################################

estimate_table5_income_gni <- function(data, income_asset_var) {
  
  # Clean estimation sample for this specific asset variable
  reg_df_income_gni_tmp <- data %>%
    mutate(income_asset_dev = .data[[income_asset_var]]) %>%
    filter(
      is.finite(GNI_dev),
      is.finite(gdp_dev),
      is.finite(income_asset_dev),
      is.finite(time)
    ) %>%
    arrange(iso, year)
  
  # First-stage country FE regression
  panel_df_income_gni_tmp <- pdata.frame(
    reg_df_income_gni_tmp,
    index = c("iso", "year")
  )
  
  reg_income_gni_stage1_tmp <- plm(
    GNI_dev ~ gdp_dev + I(time * gdp_dev) + I(income_asset_dev * gdp_dev),
    data   = panel_df_income_gni_tmp,
    model  = "within",
    effect = "individual"
  )
  
  # Country-specific residual standard deviation
  reg_df_income_gni_tmp$resid_income_gni_stage1 <-
    as.numeric(residuals(reg_income_gni_stage1_tmp))
  
  sigma_by_country_income_gni_tmp <- reg_df_income_gni_tmp %>%
    group_by(iso) %>%
    summarise(
      sigma_income_gni_i = sd(resid_income_gni_stage1, na.rm = TRUE),
      n_resid_income_gni = sum(is.finite(resid_income_gni_stage1)),
      .groups = "drop"
    )
  
  # Second-stage weights
  reg_df_income_gni_w_tmp <- reg_df_income_gni_tmp %>%
    left_join(sigma_by_country_income_gni_tmp, by = "iso") %>%
    mutate(weight_income_gni_i = 1 / sigma_income_gni_i) %>%
    filter(
      is.finite(weight_income_gni_i),
      weight_income_gni_i > 0
    )
  
  # Second-stage weighted country FE regression
  panel_df_income_gni_w_tmp <- pdata.frame(
    reg_df_income_gni_w_tmp,
    index = c("iso", "year")
  )
  
  reg_income_gni_stage2_tmp <- plm(
    GNI_dev ~ gdp_dev + I(time * gdp_dev) + I(income_asset_dev * gdp_dev),
    data    = panel_df_income_gni_w_tmp,
    model   = "within",
    effect  = "individual",
    weights = weight_income_gni_i
  )
  
  return(reg_income_gni_stage2_tmp)
}


############################################################
#### Estimate the five Table 5-style GNI regressions
############################################################

reg_income_gni_eq_assets_table5 <- estimate_table5_income_gni(
  merged_df_GNI,
  "eq_ehb_crude_dev_non_ppp"
)

reg_income_gni_debt_assets_table5 <- estimate_table5_income_gni(
  merged_df_GNI,
  "debt_ehb_crude_dev_non_ppp"
)

reg_income_gni_fdi_assets_table5 <- estimate_table5_income_gni(
  merged_df_GNI,
  "fdi_ehb_crude_dev_non_ppp"
)

reg_income_gni_eq_debt_assets_table5 <- estimate_table5_income_gni(
  merged_df_GNI,
  "eq_debt_ehb_crude_dev_non_ppp"
)

reg_income_gni_all_assets_table5 <- estimate_table5_income_gni(
  merged_df_GNI,
  "ehb_crude_dev_non_ppp"
)


############################################################
#### Export Table 5-style GNI regression table
############################################################
BITTE NOCHMAL CHECKEN, IWIE FUNKTIONIERT DIESER OUTPUT NICHT?!
library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  stop("Output folder not found. Check getwd() and whether ../output exists.")
}

stargazer(
  reg_income_gni_eq_assets_table5,
  reg_income_gni_debt_assets_table5,
  reg_income_gni_fdi_assets_table5,
  reg_income_gni_eq_debt_assets_table5,
  reg_income_gni_all_assets_table5,
  type = "latex",
  out = file.path(output_dir, "table5_income_gni_risk_sharing.tex"),
  title = "Income Risk Sharing and Foreign Asset Holdings",
  dep.var.labels = "GNI Growth Deviation",
  column.labels = c(
    "Equity",
    "Debt",
    "FDI",
    "Equity + Debt",
    "All Assets"
  ),
  covariate.labels = c(
    "GDP Growth Deviation",
    "Time $\\times$ GDP Growth Deviation",
    "Foreign Assets $\\times$ GDP Growth Deviation"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  header = FALSE
)

file.exists(file.path(output_dir, "table5_income_gni_risk_sharing.tex"))


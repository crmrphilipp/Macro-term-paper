# ============================================
#The main regression: Consumption Risk Sharing
#Table 3 in the paper
#=============================================
rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
oecd_data <- read.csv("../data/data_cy_rep.csv")

# right format
df_oecd <- oecd_data %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD, Population),
    names_from = measure,
    values_from = c(OBS_VALUE, OBS_VALUE_per_capita),
    names_glue = "{measure}_{.value}"
  ) %>%
  rename(
    consumption = consumption_OBS_VALUE,
    GDP = GDP_OBS_VALUE,
    consumption_pc = consumption_OBS_VALUE_per_capita,
    GDP_pc = GDP_OBS_VALUE_per_capita,
  )

### Creating deviation variables 

### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

df_oecd <- df_oecd %>%
  filter(REF_AREA %in% reg_sample)


#### per capita growth rates: GDP and consumption
df_oecd <- df_oecd %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    consumption_log_diff = log(consumption_pc) - log(dplyr::lag(consumption_pc))
  ) %>%
  ungroup()


# aggregate per capita growth rates: GDP and consumption


# common country-year sample: GDP, consumption and Population must all be available

df_oecd_aggregate2 <- df_oecd %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_total2 = sum(
      GDP[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    consumption_total2 = sum(
      consumption[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    population_total2 = sum(
      Population[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    gdp_total_pc2 = gdp_total2 / population_total2,
    consumption_total_pc2 = consumption_total2 / population_total2
  ) %>%
  mutate(
    gdp_log_diff_total2 =
      log(gdp_total_pc2) - log(dplyr::lag(gdp_total_pc2)),
    
    consumption_log_diff_total2 =
      log(consumption_total_pc2) - log(dplyr::lag(consumption_total_pc2))
  )


# join the aggregate growth rates
df_oecd <- df_oecd %>%
  left_join(
    df_oecd_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total2, consumption_log_diff_total2),
    by = "TIME_PERIOD"
  )


# growth rate deviations
df_oecd <- df_oecd %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total2,
    consumption_dev = consumption_log_diff - consumption_log_diff_total2
  )


# keep regression period
df_oecd <- df_oecd %>%
  filter(TIME_PERIOD > 1992, TIME_PERIOD < 2004)


# load home bias data
ehb_data <- read.csv("../data/ehb_reg_small.csv")
ehb_crude_data <- read.csv("../data/ehb_crude_reg_small.csv")


# rename identifiers
df_oecd <- df_oecd %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )


# merge data
merged_df <- list(df_oecd, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 1998)


# clean estimation sample
reg_df <- merged_df %>%
  filter(
    !is.na(consumption_dev),
    !is.na(gdp_dev),
    !is.na(EHB_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)


# First step regression with country FE
panel_df <- pdata.frame(reg_df, index = c("iso", "year"))

reg_cons_stage1 <- plm(
  consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data   = panel_df,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_df$resid_stage1 <- as.numeric(residuals(reg_cons_stage1))

sigma_by_country <- reg_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_df_w <- reg_df %>%
  left_join(sigma_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_df_w <- pdata.frame(reg_df_w, index = c("iso", "year"))

reg_cons_stage2 <- plm(
  consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data    = panel_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)


##############################
#### Creating the tables
##############################
summary(reg_cons_stage2)

library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  stop("The output folder does not exist. Please check your working directory with getwd().")
}

stargazer(
  reg_cons_stage2,
  type = "latex",
  out = file.path(output_dir, "reg_consumption_risk_sharing.tex"),
  title = "Consumption Risk Sharing and Equity Home Bias",
  dep.var.labels = "Consumption Growth Deviation",
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

file.exists(file.path(output_dir, "reg_consumption_risk_sharing.tex"))

############################################################
#### Table 5-style regressions: Foreign assets / GDP
############################################################

estimate_table5_cons <- function(data, asset_var) {
  
  # Clean estimation sample for this specific asset variable
  reg_df_tmp <- data %>%
    mutate(asset_dev = .data[[asset_var]]) %>%
    filter(
      is.finite(consumption_dev),
      is.finite(gdp_dev),
      is.finite(asset_dev),
      is.finite(time)
    ) %>%
    arrange(iso, year)
  
  # First-stage country FE regression
  panel_df_tmp <- pdata.frame(reg_df_tmp, index = c("iso", "year"))
  
  reg_stage1_tmp <- plm(
    consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(asset_dev * gdp_dev),
    data   = panel_df_tmp,
    model  = "within",
    effect = "individual"
  )
  
  # Country-specific residual standard deviation
  reg_df_tmp$resid_stage1 <- as.numeric(residuals(reg_stage1_tmp))
  
  sigma_by_country_tmp <- reg_df_tmp %>%
    group_by(iso) %>%
    summarise(
      sigma_i = sd(resid_stage1, na.rm = TRUE),
      n_resid = sum(is.finite(resid_stage1)),
      .groups = "drop"
    )
  
  # Second-stage weights
  reg_df_tmp_w <- reg_df_tmp %>%
    left_join(sigma_by_country_tmp, by = "iso") %>%
    mutate(weight_i = 1 / sigma_i) %>%
    filter(
      is.finite(weight_i),
      weight_i > 0
    )
  
  # Second-stage weighted country FE regression
  panel_df_tmp_w <- pdata.frame(reg_df_tmp_w, index = c("iso", "year"))
  
  reg_stage2_tmp <- plm(
    consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(asset_dev * gdp_dev),
    data    = panel_df_tmp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  return(reg_stage2_tmp)
}
#==========================================================
#### Estimate the five Table 5-style regressions
#Note:
# eq_ehb_crude_dev_non_ppp        <- Equity assets / GDP deviation
# debt_ehb_crude_dev_non_ppp      <- Debt assets / GDP deviation
# fdi_ehb_crude_dev_non_ppp       <- FDI assets / GDP deviation
# eq_debt_ehb_crude_dev_non_ppp   <- Equity + Debt assets / GDP deviation
# ehb_crude_dev_non_ppp           <- All assets deviation
#==========================================================

reg_cons_eq_assets <- estimate_table5_cons(
  merged_df,
  "eq_ehb_crude_dev_non_ppp"
)

reg_cons_debt_assets <- estimate_table5_cons(
  merged_df,
  "debt_ehb_crude_dev_non_ppp"
)

reg_cons_fdi_assets <- estimate_table5_cons(
  merged_df,
  "fdi_ehb_crude_dev_non_ppp"
)

reg_cons_eq_debt_assets <- estimate_table5_cons(
  merged_df,
  "eq_debt_ehb_crude_dev_non_ppp"
)

reg_cons_all_assets <- estimate_table5_cons(
  merged_df,
  "ehb_crude_dev_non_ppp"
)

summary(reg_cons_eq_assets)
summary(reg_cons_debt_assets)
summary(reg_cons_fdi_assets)
summary(reg_cons_eq_debt_assets)
summary(reg_cons_all_assets)

#=============================
#### Export regression table
#=============================

library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  stop("The output folder does not exist. Please check your working directory with getwd().")
}

stargazer(
  reg_cons_stage2,
  type = "latex",
  out = file.path(output_dir, "table_5_consumption_risk_sharing.tex"),
  title = "Risk Sharing and Equity Home Bias",
  dep.var.labels = "Consumption Growth Deviation",
  column.labels = c("EHB"),
  covariate.labels = c(
    "GDP Growth Deviation",
    "Time $\\times$ GDP Growth Deviation",
    "Home Bias $\\times$ GDP Growth Deviation"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE
)

file.exists(file.path(output_dir, "table_5_consumption_risk_sharing.tex"))

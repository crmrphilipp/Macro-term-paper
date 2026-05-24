# ============================================
#The main regression: Consumption Risk Sharing
#Table 3 & Table 5 in the paper
#=============================================

# ============================================
# 1. Cleaning data and preparing data for regression
#=============================================

rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
cons_rep_raw <- read.csv("../data/data_cy_rep.csv")


# load home bias data
ehb_data <- read.csv("../data/ehb_reg_small.csv")
ehb_crude_data <- read.csv("../data/ehb_crude_reg_small.csv")


### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)
# right format
cons_rep <- cons_rep_raw %>%
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
cons_rep <- cons_rep %>%
  filter(REF_AREA %in% reg_sample)

#### per capita growth rates: GDP and consumption
cons_rep <- cons_rep %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    consumption_log_diff = log(consumption_pc) - log(dplyr::lag(consumption_pc))
  ) %>%
  ungroup()

# common country-year sample: GDP, consumption and Population must all be available
cons_rep_aggregate2 <- cons_rep %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_total = sum(
      GDP[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    cons_total = sum(
      consumption[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    pop_total = sum(
      Population[!is.na(GDP) & !is.na(consumption) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    gdp_total_pc = gdp_total / pop_total,
    cons_total_pc = cons_total / pop_total
  ) %>%
  mutate(
    gdp_log_diff_total =
      log(gdp_total_pc) - log(dplyr::lag(gdp_total_pc)),
    
    cons_log_diff_total =
      log(cons_total_pc) - log(dplyr::lag(cons_total_pc))
  )


# join the aggregate growth rates
cons_rep <- cons_rep %>%
  left_join(
    cons_rep_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total, cons_log_diff_total),
    by = "TIME_PERIOD"
  )


# growth rate deviations
cons_rep <- cons_rep %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total,
    cons_dev = consumption_log_diff - cons_log_diff_total
  )

# keep regression period
cons_rep <- cons_rep %>%
  filter(TIME_PERIOD > 1992, TIME_PERIOD < 2004)

# rename identifiers
cons_rep <- cons_rep %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )

# merge data
merged_df <- list(cons_rep, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 1998)

# clean estimation sample
reg_cons_df <- merged_df %>%
  filter(
    !is.na(cons_dev),
    !is.na(gdp_dev),
    !is.na(EHB_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)

write.csv(
  reg_cons_df,
  "../data/reg_cons_df_original_sample.csv",
  row.names = FALSE
)
# ============================================
# 2. Run Table 3 Consumption regression
#=============================================

# First step regression with country FE
panel_cons_df <- pdata.frame(reg_cons_df, index = c("iso", "year"))

reg_cons_stage1 <- plm(
  cons_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data   = panel_cons_df,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_cons_df$resid_stage1 <- as.numeric(residuals(reg_cons_stage1))

sigma_by_country <- reg_cons_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_cons_df_w <- reg_cons_df %>%
  left_join(sigma_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_cons_df_w <- pdata.frame(reg_cons_df_w, index = c("iso", "year"))

reg_cons_stage2 <- plm(
  cons_dev ~ gdp_dev + I(time * gdp_dev) + I(EHB_dev * gdp_dev),
  data    = panel_cons_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)




# ============================================
# 3. Table 5 consumption regression: Foreign Assets over GDP
#=============================================

estimate_table5_cons <- function(data, asset_var) {
  
  # Clean estimation sample for this specific asset variable
  reg_cons_df_tmp <- data %>%
    mutate(asset_dev = .data[[asset_var]]) %>%
    filter(
      is.finite(cons_dev),
      is.finite(gdp_dev),
      is.finite(asset_dev),
      is.finite(time)
    ) %>%
    arrange(iso, year)
  
  # First-stage country FE regression
  panel_cons_df_tmp <- pdata.frame(reg_cons_df_tmp, index = c("iso", "year"))
  
  reg_stage1_tmp <- plm(
    cons_dev ~ gdp_dev + I(time * gdp_dev) + I(asset_dev * gdp_dev),
    data   = panel_cons_df_tmp,
    model  = "within",
    effect = "individual"
  )
  
  # Country-specific residual standard deviation
  reg_cons_df_tmp$resid_stage1 <- as.numeric(residuals(reg_stage1_tmp))
  
  sigma_by_country_tmp <- reg_cons_df_tmp %>%
    group_by(iso) %>%
    summarise(
      sigma_i = sd(resid_stage1, na.rm = TRUE),
      n_resid = sum(is.finite(resid_stage1)),
      .groups = "drop"
    )
  
  # Second-stage weights
  reg_cons_df_tmp_w <- reg_cons_df_tmp %>%
    left_join(sigma_by_country_tmp, by = "iso") %>%
    mutate(weight_i = 1 / sigma_i) %>%
    filter(
      is.finite(weight_i),
      weight_i > 0
    )
  
  # Second-stage weighted country FE regression
  panel_cons_df_tmp_w <- pdata.frame(reg_cons_df_tmp_w, index = c("iso", "year"))
  
  reg_stage2_tmp <- plm(
    cons_dev ~ gdp_dev + I(time * gdp_dev) + I(asset_dev * gdp_dev),
    data    = panel_cons_df_tmp_w,
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


# ============================================
# 4. Creating the tables
#=============================================

# Table 3 Regression results

library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  stop("The output folder does not exist. Please check your working directory with getwd().")
}

# Extract coefficient table from regression 2
coef_table_stage2 <- summary(reg_cons_stage2)$coefficients

# Check coefficient names
print(rownames(coef_table_stage2))

# Original coefficient names from the regression
required_terms <- c(
  "gdp_dev",
  "I(time * gdp_dev)",
  "I(EHB_dev * gdp_dev)"
)

missing_terms <- setdiff(required_terms, rownames(coef_table_stage2))

if (length(missing_terms) > 0) {
  stop(
    "These terms are missing from the regression output: ",
    paste(missing_terms, collapse = ", ")
  )
}

# Extract original coefficients
k0 <- coef_table_stage2["gdp_dev", "Estimate"]
k1 <- coef_table_stage2["I(time * gdp_dev)", "Estimate"]
k2 <- coef_table_stage2["I(EHB_dev * gdp_dev)", "Estimate"]

# Extract original standard errors
se_k0 <- coef_table_stage2["gdp_dev", "Std. Error"]
se_k1 <- coef_table_stage2["I(time * gdp_dev)", "Std. Error"]
se_k2 <- coef_table_stage2["I(EHB_dev * gdp_dev)", "Std. Error"]

# Transform coefficients as in Table 3
table3_coef <- c(
  100 * (1 - k0),
  -100 * k1,
  -100 * k2
)

# IMPORTANT:
# Names must match the original coefficient names,
# not the pretty labels.
names(table3_coef) <- required_terms

# Transform t-values
# First t-value tests k0 = 1, because average risk sharing is 100 * (1 - k0)
table3_t <- c(
  (1 - k0) / se_k0,
  (-k1) / se_k1,
  (-k2) / se_k2
)

names(table3_t) <- required_terms

# Optional: print transformed values before exporting
print(table3_coef)
print(table3_t)

# Stargazer output
stargazer(
  reg_cons_stage2,
  type = "latex",
  out = file.path(output_dir, "reg_consumption_risk_sharing.tex"),
  title = "OECD REP Consumption Risk Sharing and Equity Home Bias",
  dep.var.labels = "Consumption Risk Sharing",
  column.labels = c("Consumption"),
  model.numbers = FALSE,
  coef = list(table3_coef),
  t = list(table3_t),
  report = "vct",
  covariate.labels = c(
    "Average risk sharing",
    "Trend",
    "Equity Home Bias"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  header = FALSE
)

file.exists(file.path(output_dir, "reg_consumption_risk_sharing.tex"))

# ============================================
# Table 5 regression results
# Function: transform coefficients into Table 5 format
# ============================================

get_table5_cons_estimates <- function(model) {
  
  coef_table <- summary(model)$coefficients
  
  required_terms <- c(
    "gdp_dev",
    "I(time * gdp_dev)",
    "I(asset_dev * gdp_dev)"
  )
  
  missing_terms <- setdiff(required_terms, rownames(coef_table))
  
  if (length(missing_terms) > 0) {
    stop(
      "The following coefficient(s) are missing from the model: ",
      paste(missing_terms, collapse = ", ")
    )
  }
  
  # --------------------------------------------
  # Extract original coefficients
  # --------------------------------------------
  # The regression estimates:
  #
  # cons_dev = k0 * gdp_dev
  #          + k1 * time * gdp_dev
  #          + k2 * asset_dev * gdp_dev
  #          + country fixed effects
  #          + error
  #
  # But Table 5 reports risk-sharing coefficients:
  #
  # Average risk sharing =  100 * (1 - k0)
  # Trend                = -100 * k1
  # Asset interaction    = -100 * k2
  # --------------------------------------------
  
  k0 <- coef_table["gdp_dev", "Estimate"]
  k1 <- coef_table["I(time * gdp_dev)", "Estimate"]
  k2 <- coef_table["I(asset_dev * gdp_dev)", "Estimate"]
  
  se_k0 <- coef_table["gdp_dev", "Std. Error"]
  se_k1 <- coef_table["I(time * gdp_dev)", "Std. Error"]
  se_k2 <- coef_table["I(asset_dev * gdp_dev)", "Std. Error"]
  
  # Transformed coefficients in percent
  coef_out <- c(
    100 * (1 - k0),
    -100 * k1,
    -100 * k2
  )
  
  names(coef_out) <- required_terms
  
  # Corresponding t-values
  # For average risk sharing, the relevant null is:
  # 100 * (1 - k0) = 0, which is equivalent to k0 = 1.
  t_out <- c(
    (1 - k0) / se_k0,
    (-k1) / se_k1,
    (-k2) / se_k2
  )
  
  names(t_out) <- required_terms
  
  return(
    list(
      coef = coef_out,
      t = t_out
    )
  )
}


# ============================================
# Prepare transformed coefficients and t-values
# ============================================

table5_cons_models <- list(
  reg_cons_eq_assets,
  reg_cons_debt_assets,
  reg_cons_fdi_assets,
  reg_cons_eq_debt_assets,
  reg_cons_all_assets
)

table5_cons_output <- lapply(
  table5_cons_models,
  get_table5_cons_estimates
)

table5_cons_coef <- lapply(
  table5_cons_output,
  function(x) x$coef
)

table5_cons_t <- lapply(
  table5_cons_output,
  function(x) x$t
)


# ============================================
# Stargazer output: Table 5 consumption regression
# ============================================

stargazer(
  table5_cons_models,
  type = "latex",
  out = file.path(output_dir, "table5_consumption_foreign_assets.tex"),
  title = "OECD REP Consumption Risk Sharing and Foreign Asset Holdings Relative to GDP",
  dep.var.labels = "Consumption Risk Sharing",
  column.labels = c(
    "Equity",
    "Debt",
    "FDI",
    "Equity + Debt",
    "All Assets"
  ),
  model.numbers = FALSE,
  coef = table5_cons_coef,
  t = table5_cons_t,
  report = "vct",
  covariate.labels = c(
    "Average risk sharing",
    "Trend",
    "Asset holdings"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  header = FALSE
)


# ============================================
# Check whether output file was created
# ============================================

file.exists(
  file.path(output_dir, "table5_consumption_foreign_assets.tex")
)

#### The main regression: Consumption Risk Sharing

rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
pdf_df_raw <- read.csv("../data/pdf_oecd_panel_reg.csv")

# load home bias data
ehb_data <- read.csv('../data/ehb_reg_small.csv')
ehb_crude_data <- read.csv('../data/ehb_crude_reg_small.csv')

pdf_df <- pdf_df_raw

### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

### Keep regression sample countries
pdf_df <- pdf_df %>%
  filter(iso %in% reg_sample)

# merge data
merged_df <- list(pdf_df, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 1998)


# clean estimation sample
reg_df <- merged_df %>%
  filter(
    !is.na(dev_cons_ppp),
    !is.na(dev_gdp_ppp),
    !is.na(EHB_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)

#================================
# Total consumption regression
#================================

# First step regression with country FE
panel_df <- pdata.frame(reg_df, index = c("iso", "year"))

reg_cons_stage1 <- plm(
  dev_cons_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(EHB_dev * dev_gdp_ppp),
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
  dev_cons_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(EHB_dev * dev_gdp_ppp),
  data    = panel_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)


summary(reg_cons_stage2)
summary(reg_cons_stage1)


#========================================
# Consumption regression again, but using actual 
# individual consumption instead of total 
# consumption.
#========================================

# First step regression with country FE

reg_cons_stage1_ic <- plm(
  dev_acons_b4 ~ dev_gdp_b3 + I(time * dev_gdp_b3) + I(EHB_dev * dev_gdp_b3),
  data   = panel_df,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_df$resid_stage1_ic <- as.numeric(residuals(reg_cons_stage1_ic))

sigma_by_country_ic <- reg_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1_ic, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1_ic)),
    .groups = "drop"
  )


# Creating country weights
reg_df_w_ic <- reg_df %>%
  left_join(sigma_by_country_ic, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_df_w_ic<- pdata.frame(reg_df_w_ic, index = c("iso", "year"))

reg_cons_stage2_ic <- plm(
  dev_acons_b4 ~ dev_gdp_b3 + I(time * dev_gdp_b3) + I(EHB_dev * dev_gdp_b3),
  data    = panel_df_w_ic,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)


summary(reg_cons_stage2_ic)
summary(reg_cons_stage1_ic)

#========================================
# Regression using GNI
#========================================


# Clean GNI estimation sample
reg_df_gni <- reg_df %>%
  filter(
    is.finite(dev_gni_ppp),
    is.finite(dev_gdp_ppp),
    is.finite(EHB_dev),
    is.finite(time)
  ) %>%
  arrange(iso, year)

# First-step regression with country FE
panel_df_gni <- pdata.frame(reg_df_gni, index = c("iso", "year"))

reg_gni_stage1 <- plm(
  dev_gni_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(EHB_dev * dev_gdp_ppp),
  data   = panel_df_gni,
  model  = "within",
  effect = "individual"
)

# Country-level residual standard deviations
reg_df_gni$resid_stage1_gni <- as.numeric(residuals(reg_gni_stage1))

sigma_by_country_gni <- reg_df_gni %>%
  group_by(iso) %>%
  summarise(
    sigma_i_gni = sd(resid_stage1_gni, na.rm = TRUE),
    n_resid_gni = sum(is.finite(resid_stage1_gni)),
    .groups = "drop"
  )

# Create country weights
reg_df_w_gni <- reg_df_gni %>%
  left_join(sigma_by_country_gni, by = "iso") %>%
  mutate(weight_i_gni = 1 / sigma_i_gni) %>%
  filter(
    is.finite(weight_i_gni),
    weight_i_gni > 0
  )

# Second-step weighted FE regression
panel_df_w_gni <- pdata.frame(reg_df_w_gni, index = c("iso", "year"))

reg_gni_stage2 <- plm(
  dev_gni_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(EHB_dev * dev_gdp_ppp),
  data    = panel_df_w_gni,
  model   = "within",
  effect  = "individual",
  weights = weight_i_gni
)

summary(reg_gni_stage1)
summary(reg_gni_stage2)

#========================================
# Regression Table 5
# Foreign Assets over GDP
#========================================


estimate_table5_cons <- function(data, asset_var) {
  
  # Clean estimation sample for this specific asset variable
  reg_df_tmp <- data %>%
    mutate(asset_dev = .data[[asset_var]]) %>%
    filter(
      is.finite(dev_cons_ppp),
      is.finite(dev_gdp_ppp),
      is.finite(asset_dev),
      is.finite(time)
    ) %>%
    arrange(iso, year)
  
  # First-stage country FE regression
  panel_df_tmp <- pdata.frame(reg_df_tmp, index = c("iso", "year"))
  
  reg_stage1_tmp <- plm(
    dev_cons_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(asset_dev * dev_gdp_ppp),
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
      weight_i > 0,
      n_resid >= 2
    )
  
  # Second-stage weighted country FE regression
  panel_df_tmp_w <- pdata.frame(reg_df_tmp_w, index = c("iso", "year"))
  
  reg_stage2_tmp <- plm(
    dev_cons_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(asset_dev * dev_gdp_ppp),
    data    = panel_df_tmp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  return(reg_stage2_tmp)
}


#==========================================================
# CONSUMPTION
#### Estimate the five Table 5-style regressions
# Note:
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

#========================================
# Regression Table 5
# Foreign Assets over GDP
# GNI Risk Sharing
#========================================

estimate_table5_gni <- function(data, asset_var) {
  
  # Clean estimation sample for this specific asset variable
  reg_df_tmp <- data %>%
    mutate(asset_dev = .data[[asset_var]]) %>%
    filter(
      is.finite(dev_gni_ppp),
      is.finite(dev_gdp_ppp),
      is.finite(asset_dev),
      is.finite(time)
    ) %>%
    arrange(iso, year)
  
  # First-stage country FE regression
  panel_df_tmp <- pdata.frame(reg_df_tmp, index = c("iso", "year"))
  
  reg_stage1_tmp <- plm(
    dev_gni_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(asset_dev * dev_gdp_ppp),
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
      weight_i > 0,
      n_resid >= 2
    )
  
  # Second-stage weighted country FE regression
  panel_df_tmp_w <- pdata.frame(reg_df_tmp_w, index = c("iso", "year"))
  
  reg_stage2_tmp <- plm(
    dev_gni_ppp ~ dev_gdp_ppp + I(time * dev_gdp_ppp) + I(asset_dev * dev_gdp_ppp),
    data    = panel_df_tmp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  return(reg_stage2_tmp)
}

#==========================================================
# GNI
#### Estimate the five Table 5-style regressions
# Note:
# eq_ehb_crude_dev_non_ppp        <- Equity assets / GDP deviation
# debt_ehb_crude_dev_non_ppp      <- Debt assets / GDP deviation
# fdi_ehb_crude_dev_non_ppp       <- FDI assets / GDP deviation
# eq_debt_ehb_crude_dev_non_ppp   <- Equity + Debt assets / GDP deviation
# ehb_crude_dev_non_ppp           <- All assets deviation
#==========================================================

reg_gni_eq_assets <- estimate_table5_gni(
  merged_df,
  "eq_ehb_crude_dev_non_ppp"
)

reg_gni_debt_assets <- estimate_table5_gni(
  merged_df,
  "debt_ehb_crude_dev_non_ppp"
)

reg_gni_fdi_assets <- estimate_table5_gni(
  merged_df,
  "fdi_ehb_crude_dev_non_ppp"
)

reg_gni_eq_debt_assets <- estimate_table5_gni(
  merged_df,
  "eq_debt_ehb_crude_dev_non_ppp"
)

reg_gni_all_assets <- estimate_table5_gni(
  merged_df,
  "ehb_crude_dev_non_ppp"
)

summary(reg_gni_eq_assets)
summary(reg_gni_debt_assets)
summary(reg_gni_fdi_assets)
summary(reg_gni_eq_debt_assets)
summary(reg_gni_all_assets)


#=============================
#### Export regression table
#=============================

# ============================================================
# Stargazer table creation code
# Paper-style estimates with standard errors in parentheses
# ============================================================

library(stargazer)

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# Transform raw coefficients into paper-style estimates
# Reports:
#   100 * (1 - kappa_0)
#   -100 * kappa_1
#   -100 * kappa_2
# ------------------------------------------------------------

get_paper_coef <- function(model) {
  
  b <- coef(model)
  
  out <- c(
    100 * (1 - b[1]),
    -100 * b[2],
    -100 * b[3]
  )
  
  # Important:
  # Keep original coefficient names so stargazer can match them.
  names(out) <- names(b)[1:3]
  
  return(out)
}

# ------------------------------------------------------------
# Transform standard errors accordingly
# ------------------------------------------------------------

get_paper_se <- function(model) {
  
  b <- coef(model)
  se_raw <- sqrt(diag(vcov(model)))
  
  out <- c(
    100 * se_raw[1],
    100 * se_raw[2],
    100 * se_raw[3]
  )
  
  # Important:
  # Keep original coefficient names so stargazer can match them.
  names(out) <- names(b)[1:3]
  
  return(out)
}

# ============================================================
# Quick checks
# ============================================================

print(get_paper_coef(reg_cons_stage2))
print(get_paper_se(reg_cons_stage2))

# ============================================================
# Table 3: Consumption Risk Sharing and Equity Home Bias
# ============================================================

stargazer(
  reg_cons_stage2,
  
  coef = list(
    get_paper_coef(reg_cons_stage2)
  ),
  
  se = list(
    get_paper_se(reg_cons_stage2)
  ),
  
  type = "latex",
  out = file.path(output_dir, "pdf_table_3_consumption_estimates_se.tex"),
  
  title = "Consumption Risk Sharing and Equity Home Bias",
  dep.var.labels = "Consumption Growth Deviation",
  column.labels = c("Consumption"),
  
  covariate.labels = c(
    "Average risk sharing",
    "Time trend",
    "Equity home bias"
  ),
  
  report = "vcs",
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  notes = "Standard errors in parentheses.",
  notes.append = FALSE
)

# ============================================================
# Table 3: GNI Risk Sharing and Equity Home Bias
# ============================================================

stargazer(
  reg_gni_stage2,
  
  coef = list(
    get_paper_coef(reg_gni_stage2)
  ),
  
  se = list(
    get_paper_se(reg_gni_stage2)
  ),
  
  type = "latex",
  out = file.path(output_dir, "pdf_table_3_gni_estimates_se.tex"),
  
  title = "GNI Risk Sharing and Equity Home Bias",
  dep.var.labels = "GNI Growth Deviation",
  column.labels = c("GNI"),
  
  covariate.labels = c(
    "Average income risk sharing",
    "Time trend",
    "Equity home bias"
  ),
  
  report = "vcs",
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  notes = "Standard errors in parentheses.",
  notes.append = FALSE
)



# ============================================================
# Table 5 LaTeX tables
# Point estimates only
# No standard errors, no t-values, no CSV
# ============================================================

output_dir <- "../output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# Transform raw regression coefficients into paper-style estimates
# ------------------------------------------------------------

get_table5_estimates <- function(model) {
  
  b <- coef(model)
  
  estimates <- c(
    100 * (1 - unname(b[1])),  # Average risk sharing
    -100 * unname(b[2]),       # Time trend
    -100 * unname(b[3])        # Foreign assets/GDP effect
  )
  
  return(estimates)
}

# ------------------------------------------------------------
# Format numbers
# ------------------------------------------------------------

fmt <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits)
}

# ------------------------------------------------------------
# Write LaTeX table
# ------------------------------------------------------------

write_table5_latex <- function(models,
                               column_names,
                               row_names,
                               caption,
                               label,
                               out_file,
                               digits = 3) {
  
  estimates <- do.call(
    cbind,
    lapply(models, get_table5_estimates)
  )
  
  colnames(estimates) <- column_names
  
  align <- paste0("l", paste(rep("c", length(column_names)), collapse = ""))
  
  lines <- c(
    "\\begin{table}[!htbp] \\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline\\hline",
    paste0(" & ", paste(column_names, collapse = " & "), " \\\\"),
    "\\hline"
  )
  
  for (i in seq_along(row_names)) {
    
    row_values <- fmt(estimates[i, ], digits)
    
    lines <- c(
      lines,
      paste0(row_names[i], " & ", paste(row_values, collapse = " & "), " \\\\")
    )
  }
  
  lines <- c(
    lines,
    "\\hline\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  writeLines(lines, con = file.path(output_dir, out_file))
  
  return(file.exists(file.path(output_dir, out_file)))
}

# ============================================================
# Table 5: Consumption Risk Sharing and Foreign Assets over GDP
# ============================================================

write_table5_latex(
  models = list(
    reg_cons_eq_assets,
    reg_cons_debt_assets,
    reg_cons_fdi_assets,
    reg_cons_eq_debt_assets,
    reg_cons_all_assets
  ),
  
  column_names = c(
    "Equity",
    "Debt",
    "FDI",
    "Equity + Debt",
    "All Assets"
  ),
  
  row_names = c(
    "Average risk sharing",
    "Time trend",
    "Foreign assets/GDP"
  ),
  
  caption = "Consumption Risk Sharing and Foreign Assets over GDP",
  label = "tab:table5_consumption_assets",
  out_file = "table5_consumption_point_estimates.tex"
)

# ============================================================
# Table 5: GNI Risk Sharing and Foreign Assets over GDP
# ============================================================

write_table5_latex(
  models = list(
    reg_gni_eq_assets,
    reg_gni_debt_assets,
    reg_gni_fdi_assets,
    reg_gni_eq_debt_assets,
    reg_gni_all_assets
  ),
  
  column_names = c(
    "Equity",
    "Debt",
    "FDI",
    "Equity + Debt",
    "All Assets"
  ),
  
  row_names = c(
    "Average income risk sharing",
    "Time trend",
    "Foreign assets/GDP"
  ),
  
  caption = "GNI Risk Sharing and Foreign Assets over GDP",
  label = "tab:table5_gni_assets",
  out_file = "table5_gni_point_estimates.tex"
)

# ============================================================
# Check whether LaTeX files were created
# ============================================================

file.exists(file.path(output_dir, "table5_consumption_point_estimates.tex"))
file.exists(file.path(output_dir, "table5_gni_point_estimates.tex"))

file.exists(file.path(output_dir, "pdf_table_3_consumption_estimates_se.tex"))
file.exists(file.path(output_dir, "pdf_table_3_gni_estimates_se.tex"))


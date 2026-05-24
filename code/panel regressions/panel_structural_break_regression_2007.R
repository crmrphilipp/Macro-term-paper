# ============================================
# Structural Break Regression for
# the Global Financial Crisis 2008
# Two-stage country FE regressions
# ============================================


rm(list = ls())

library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  current_path <- rstudioapi::getActiveDocumentContext()$path
  
  if (!is.null(current_path) && nzchar(current_path)) {
    setwd(dirname(current_path))
  }
}
# load data
break_df <- read.csv(
  "../data/reg-cons_df_ext1.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN", "Invalid Number", "Invalid number")
)

# ============================================
# 2. Prepare regression variables
# ============================================

break_year <- 2008

reg_cons_gfc_df <- break_df %>%
  arrange(iso, year) %>%
  mutate(
    post_gfc = as.integer(year >= break_year),
    
    # Baseline interaction terms
    time_gdp = time * gdp_dev,
    ehb_gdp  = EHB_dev * gdp_dev,
    
    # Structural break interaction terms
    gdp_dev_gfc  = post_gfc * gdp_dev,
    time_gdp_gfc = post_gfc * time_gdp,
    ehb_gdp_gfc  = post_gfc * ehb_gdp
  ) %>%
  filter(
    !is.na(iso),
    !is.na(year),
    !is.na(cons_dev),
    !is.na(gdp_dev),
    !is.na(time_gdp),
    !is.na(ehb_gdp),
    !is.na(gdp_dev_gfc),
    !is.na(time_gdp_gfc),
    !is.na(ehb_gdp_gfc)
  )

# First step regression with country FE
panel_cons_df <- pdata.frame(reg_cons_gfc_df, index = c("iso", "year"))

reg_cons_stage1 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp,
  data   = panel_cons_df,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_cons_gfc_df$resid_stage1 <- as.numeric(residuals(reg_cons_stage1))

sigma_by_country <- reg_cons_gfc_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_cons_gfc_df_w <- reg_cons_gfc_df %>%
  left_join(sigma_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_cons_gfc_df_w <- pdata.frame(reg_cons_gfc_df_w, index = c("iso", "year"))

reg_cons_stage2 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp,
  data    = panel_cons_gfc_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)

summary(reg_cons_stage2)

#============================================
# Structural break regression
#============================================

# First step regression with country FE

reg_cons_break_stage1 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp 
  + gdp_dev_gfc + time_gdp_gfc + ehb_gdp_gfc,
  data   = panel_cons_df,
  model  = "within",
  effect = "individual"
)

# Error term SD
reg_cons_gfc_df$resid_break_stage1 <- as.numeric(residuals(reg_cons_break_stage1))

sigma_by_country_break <- reg_cons_gfc_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_break_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_break_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_cons_break_df_w <- reg_cons_gfc_df %>%
  left_join(sigma_by_country_break, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_cons_break_df_w <- pdata.frame(reg_cons_break_df_w, index = c("iso", "year"))

reg_cons_break_stage2 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp + 
  gdp_dev_gfc + time_gdp_gfc + ehb_gdp_gfc,
  data    = panel_cons_break_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)

summary(reg_cons_break_stage2)

# ============================================
# 6. Joint structural break test
# ============================================

gfc_break_test <- linearHypothesis(
  reg_cons_break_stage2,
  c(
    "gdp_dev_gfc = 0",
    "time_gdp_gfc = 0",
    "ehb_gdp_gfc = 0"
  ),
  test = "F"
)

print(gfc_break_test)

###########################################
####          Neuer Versuch        ########
###########################################

# ============================================
# Search for alternative structural break years
# around the Global Financial Crisis
# ============================================

library(ggplot2)

# Choose candidate years.
# You can adjust this range.
candidate_years <- 1995:2015

run_gfc_break_search <- function(candidate_break_year) {
  
  # 1. Prepare data for this candidate break year
  temp_df <- break_df %>%
    arrange(iso, year) %>%
    mutate(
      year     = as.numeric(trimws(as.character(year))),
      cons_dev = as.numeric(trimws(as.character(cons_dev))),
      gdp_dev  = as.numeric(trimws(as.character(gdp_dev))),
      time     = as.numeric(trimws(as.character(time))),
      EHB_dev  = as.numeric(trimws(as.character(EHB_dev))),
      
      post_break = as.integer(year >= candidate_break_year),
      
      # Baseline interaction terms
      time_gdp = time * gdp_dev,
      ehb_gdp  = EHB_dev * gdp_dev,
      
      # Candidate structural break interaction terms
      gdp_dev_break  = post_break * gdp_dev,
      time_gdp_break = post_break * time_gdp,
      ehb_gdp_break  = post_break * ehb_gdp
    ) %>%
    filter(
      !is.na(iso),
      !is.na(year),
      !is.na(cons_dev),
      !is.na(gdp_dev),
      !is.na(time_gdp),
      !is.na(ehb_gdp),
      !is.na(gdp_dev_break),
      !is.na(time_gdp_break),
      !is.na(ehb_gdp_break)
    )
  
  # 2. First-stage FE regression for this candidate break
  panel_temp <- pdata.frame(
    temp_df,
    index = c("iso", "year")
  )
  
  reg_temp_stage1 <- plm(
    cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
      gdp_dev_break + time_gdp_break + ehb_gdp_break,
    data   = panel_temp,
    model  = "within",
    effect = "individual"
  )
  
  # 3. Compute country-specific residual standard deviations
  temp_df$resid_stage1 <- as.numeric(residuals(reg_temp_stage1))
  
  sigma_temp <- temp_df %>%
    group_by(iso) %>%
    summarise(
      sigma_i = sd(resid_stage1, na.rm = TRUE),
      n_resid = sum(!is.na(resid_stage1)),
      .groups = "drop"
    )
  
  # 4. Add paper-style weights
  temp_df_w <- temp_df %>%
    left_join(sigma_temp, by = "iso") %>%
    filter(
      !is.na(sigma_i),
      sigma_i > 0,
      n_resid >= 2
    ) %>%
    mutate(
      weight_i = 1 / sigma_i
    )
  
  # 5. Second-stage weighted FE regression
  panel_temp_w <- pdata.frame(
    temp_df_w,
    index = c("iso", "year")
  )
  
  reg_temp_stage2 <- plm(
    cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
      gdp_dev_break + time_gdp_break + ehb_gdp_break,
    data    = panel_temp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  
  # 6. Joint test for this candidate break year
  break_test_temp <- linearHypothesis(
    reg_temp_stage2,
    c(
      "gdp_dev_break = 0",
      "time_gdp_break = 0",
      "ehb_gdp_break = 0"
    ),
    test = "F"
  )
  
  break_test_df <- as.data.frame(break_test_temp)
  
  # 7. Return compact result
  tibble(
    break_year  = candidate_break_year,
    F_statistic = break_test_df[2, "F"],
    p_value     = break_test_df[2, "Pr(>F)"],
    n_obs       = nrow(temp_df_w),
    n_countries = length(unique(temp_df_w$iso))
  )
}

# Run search over all candidate years
gfc_break_search_results <- bind_rows(
  lapply(candidate_years, run_gfc_break_search)
)

# Print results
print(gfc_break_search_results, n = 21)

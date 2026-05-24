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
  "../data/reg-cons_df_ext2.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN", "Invalid Number", "Invalid number")
)

# ============================================
# 2. Prepare regression variables
# ============================================

break_year <- 1999
euro_founders_1999 <- c(
 "AUT", "BEL", "FIN", "FRA", "DEU", "IRL",
  "ITA", "LUX", "NLD", "PRT", "ESP"
)

# For main Euro test: pre-GFC sample
# euro_sample_end <- 2007

reg_cons_euro_df <- break_df %>%
  arrange(iso, year) %>%
  mutate(
    year     = as.numeric(trimws(as.character(year))),
    cons_dev = as.numeric(trimws(as.character(cons_dev))),
    gdp_dev  = as.numeric(trimws(as.character(gdp_dev))),
    time     = as.numeric(trimws(as.character(time))),
    EHB_dev  = as.numeric(trimws(as.character(EHB_dev)))
  ) %>%
  filter(
    # year <= euro_sample_end,
    iso != "GRC"
  ) %>%
  mutate(
    # Treatment and post dummies
    euro_area = as.integer(iso %in% euro_founders_1999),
    post1999  = as.integer(year >= 1999),
    euro_post1999 = euro_area * post1999,
    
    # Baseline interaction terms
    time_gdp = time * gdp_dev,
    ehb_gdp  = EHB_dev * gdp_dev,
    
    # Allow Euro countries to have different pre-1999 slopes
    euro_gdp      = euro_area * gdp_dev,
    euro_time_gdp = euro_area * time_gdp,
    euro_ehb_gdp  = euro_area * ehb_gdp,
    
    # Common post-1999 slope changes for all countries
    post1999_gdp      = post1999 * gdp_dev,
    post1999_time_gdp = post1999 * time_gdp,
    post1999_ehb_gdp  = post1999 * ehb_gdp,
    
    # Additional Euro-specific post-1999 slope changes
    euro1999_gdp      = euro_post1999 * gdp_dev,
    euro1999_time_gdp = euro_post1999 * time_gdp,
    euro1999_ehb_gdp  = euro_post1999 * ehb_gdp
  ) %>%
  filter(
    !is.na(iso),
    !is.na(year),
    !is.na(cons_dev),
    !is.na(gdp_dev),
    !is.na(time_gdp),
    !is.na(ehb_gdp),
    !is.na(euro_gdp),
    !is.na(euro_time_gdp),
    !is.na(euro_ehb_gdp),
    !is.na(post1999_gdp),
    !is.na(post1999_time_gdp),
    !is.na(post1999_ehb_gdp),
    !is.na(euro1999_gdp),
    !is.na(euro1999_time_gdp),
    !is.na(euro1999_ehb_gdp)
  )


#============================================
# Euro Area Structural break regression
#============================================

# First step regression with country FE

reg_euro_1 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
  post1999 + euro_post1999 +
  euro_gdp + euro_time_gdp + euro_ehb_gdp +
  post1999_gdp + post1999_time_gdp + post1999_ehb_gdp +
  euro1999_gdp + euro1999_time_gdp + euro1999_ehb_gdp,
  data   = reg_cons_euro_df,
  model  = "within",
  effect = "individual"
)

# Error term SD
reg_cons_euro_df$resid_euro_1 <- as.numeric(residuals(reg_euro_1))

sigma_by_country <- reg_cons_euro_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_euro_1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_euro_1)),
    .groups = "drop"
  )


# Creating country weights
reg_cons_euro_df_w <- reg_cons_euro_df %>%
  left_join(sigma_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_cons_euro_df_w <- pdata.frame(reg_cons_euro_df_w, index = c("iso", "year"))

reg_euro_2 <- plm(
  cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
  post1999 + euro_post1999 +
  euro_gdp + euro_time_gdp + euro_ehb_gdp +
  post1999_gdp + post1999_time_gdp + post1999_ehb_gdp +
  euro1999_gdp + euro1999_time_gdp + euro1999_ehb_gdp,
  data    = panel_cons_euro_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)

summary(reg_euro_2)

# ============================================
# 6. Joint structural break test
# ============================================

# ============================================
# Euro-specific post-1999 structural break test
# ============================================

euro_break_test <- linearHypothesis(
  reg_euro_2,
  c(
    "euro1999_gdp = 0",
    "euro1999_time_gdp = 0",
    "euro1999_ehb_gdp = 0"
  ),
  test = "F"
)

print(euro_break_test)

post1999_break_test <- linearHypothesis(
  reg_euro_2,
  c(
    "post1999_gdp = 0",
    "post1999_time_gdp = 0",
    "post1999_ehb_gdp = 0"
  ),
  test = "F"
)

print(post1999_break_test)

###########################################
####          Neuer Versuch        ########
###########################################
library(dplyr)
library(plm)
library(car)
library(purrr)
library(ggplot2)

# ============================================
# Search for unknown common structural break
# in GDP-risk-sharing slope
# ============================================

euro_founders_1999 <- c(
  "AUT", "BEL", "FIN", "FRA", "DEU", "IRL",
  "ITA", "LUX", "NLD", "PRT", "ESP"
)

# Candidate break years
candidate_years <- 1999:2015

run_break_search <- function(break_year) {
  
  # ----------------------------
  # 1. Prepare data for candidate break year
  # ----------------------------
  
  temp_df <- break_df %>%
    arrange(iso, year) %>%
    mutate(
      year     = as.numeric(trimws(as.character(year))),
      cons_dev = as.numeric(trimws(as.character(cons_dev))),
      gdp_dev  = as.numeric(trimws(as.character(gdp_dev))),
      time     = as.numeric(trimws(as.character(time))),
      EHB_dev  = as.numeric(trimws(as.character(EHB_dev)))
    ) %>%
    filter(
      iso != "GRC"
      year <= 2007
    ) %>%
    mutate(
      # Euro treatment variables fixed at 1999
      euro_area = as.integer(iso %in% euro_founders_1999),
      post1999  = as.integer(year >= 1999),
      euro_post1999 = euro_area * post1999,
      
      # Candidate break dummy
      post_break = as.integer(year >= break_year),
      
      # Baseline interaction terms
      time_gdp = time * gdp_dev,
      ehb_gdp  = EHB_dev * gdp_dev,
      
      # Euro countries may have different pre-1999 slopes
      euro_gdp      = euro_area * gdp_dev,
      euro_time_gdp = euro_area * time_gdp,
      euro_ehb_gdp  = euro_area * ehb_gdp,
      
      # Euro-specific post-1999 slope changes
      euro1999_gdp      = euro_post1999 * gdp_dev,
      euro1999_time_gdp = euro_post1999 * time_gdp,
      euro1999_ehb_gdp  = euro_post1999 * ehb_gdp,
      
      # Candidate common post-break slope changes
      postbreak_gdp      = post_break * gdp_dev,
      postbreak_time_gdp = post_break * time_gdp,
      postbreak_ehb_gdp  = post_break * ehb_gdp
    ) %>%
    filter(
      !is.na(iso),
      !is.na(year),
      !is.na(cons_dev),
      !is.na(gdp_dev),
      !is.na(time_gdp),
      !is.na(ehb_gdp),
      !is.na(euro_gdp),
      !is.na(euro_time_gdp),
      !is.na(euro_ehb_gdp),
      !is.na(euro1999_gdp),
      !is.na(euro1999_time_gdp),
      !is.na(euro1999_ehb_gdp),
      !is.na(postbreak_gdp),
      !is.na(postbreak_time_gdp),
      !is.na(postbreak_ehb_gdp)
    )
  
  # ----------------------------
  # 2. First-stage FE regression
  # ----------------------------
  
  panel_temp <- pdata.frame(
    temp_df,
    index = c("iso", "year")
  )
  
  reg_stage1 <- plm(
    cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
      post1999 + euro_post1999 +
      euro_gdp + euro_time_gdp + euro_ehb_gdp +
      euro1999_gdp + euro1999_time_gdp + euro1999_ehb_gdp +
      postbreak_gdp + postbreak_time_gdp + postbreak_ehb_gdp,
    data   = panel_temp,
    model  = "within",
    effect = "individual"
  )
  
  # ----------------------------
  # 3. Compute country weights
  # ----------------------------
  
  temp_df$resid_stage1 <- as.numeric(residuals(reg_stage1))
  
  sigma_by_country <- temp_df %>%
    group_by(iso) %>%
    summarise(
      sigma_i = sd(resid_stage1, na.rm = TRUE),
      n_resid = sum(!is.na(resid_stage1)),
      .groups = "drop"
    )
  
  temp_df_w <- temp_df %>%
    left_join(sigma_by_country, by = "iso") %>%
    filter(
      !is.na(sigma_i),
      sigma_i > 0,
      n_resid >= 2
    ) %>%
    mutate(
      weight_i = 1 / sigma_i
    )
  
  # ----------------------------
  # 4. Second-stage weighted FE regression
  # ----------------------------
  
  panel_temp_w <- pdata.frame(
    temp_df_w,
    index = c("iso", "year")
  )
  
  reg_stage2 <- plm(
    cons_dev ~ gdp_dev + time_gdp + ehb_gdp +
      post1999 + euro_post1999 +
      euro_gdp + euro_time_gdp + euro_ehb_gdp +
      euro1999_gdp + euro1999_time_gdp + euro1999_ehb_gdp +
      postbreak_gdp + postbreak_time_gdp + postbreak_ehb_gdp,
    data    = panel_temp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  # ----------------------------
  # 5. Joint test for candidate break year
  # ----------------------------
  
  break_test <- linearHypothesis(
    reg_stage2,
    c(
      "postbreak_gdp = 0",
      "postbreak_time_gdp = 0",
      "postbreak_ehb_gdp = 0"
    ),
    test = "F"
  )
  
  break_test_df <- as.data.frame(break_test)
  
  tibble(
    break_year  = break_year,
    F_statistic = break_test_df[2, "F"],
    p_value     = break_test_df[2, "Pr(>F)"],
    n_obs       = nrow(temp_df_w),
    n_countries = length(unique(temp_df_w$iso))
  )
}

# Run search over candidate years
break_search_results <- map_dfr(
  candidate_years,
  run_break_search
)

break_search_results

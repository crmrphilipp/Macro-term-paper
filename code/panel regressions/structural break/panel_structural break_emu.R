
rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
library(lmtest)
library(sandwich)
library(car)

setwd(
  normalizePath(
    file.path(
      dirname(rstudioapi::getActiveDocumentContext()$path),
      "..", "..", "..", "data"
    )
  )
)

# load data
reg_gni_df <- read.csv("../data/reg_gni_df_ext_3.csv")
reg_cons_df <- read.csv("../data/reg_cons_df_ext_3.csv")

emu_countries <- c(
  "AUT", "BEL", "FIN", "FRA", "DEU",
  "GRC", "IRL", "ITA", "NLD", "PRT", "ESP"
)

reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

reg_gni_df <- reg_gni_df %>%
  filter(iso %in% reg_sample)

reg_cons_df <- reg_cons_df %>%
  filter(iso %in% reg_sample)

run_two_stage_ehb_emu_break_reg <- function(
    df,
    yvar,
    ehb_var = "EHB_dev",
    post_year = 1999,
    end_year = 2019,
    emu_countries = c(
      "AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "ITA", "NLD", "PRT", "GRC"
    )
) {
  
  # --------------------------------------------------
  # 1. Prepare regression data
  # --------------------------------------------------
  
  reg_df_tmp <- df %>%
    filter(year <= end_year) %>%
    mutate(
      post = ifelse(year >= post_year, 1, 0),
      emu  = ifelse(iso %in% emu_countries, 1, 0),
      emu_post = emu * post,
      
      # Baseline GDP shock term
      x = gdp_dev,
      x_post = post * gdp_dev,
      x_emu = emu * gdp_dev,
      x_emu_post = emu_post * gdp_dev,
      
      # Time trend interaction term
      x_time = time * gdp_dev,
      x_time_post = post * time * gdp_dev,
      x_time_emu = emu * time * gdp_dev,
      x_time_emu_post = emu_post * time * gdp_dev,
      
      # EHB interaction term
      x_ehb = .data[[ehb_var]] * gdp_dev,
      x_ehb_post = post * .data[[ehb_var]] * gdp_dev,
      x_ehb_emu = emu * .data[[ehb_var]] * gdp_dev,
      x_ehb_emu_post = emu_post * .data[[ehb_var]] * gdp_dev
    ) %>%
    filter(
      is.finite(.data[[yvar]]),
      is.finite(x),
      is.finite(x_post),
      is.finite(x_emu),
      is.finite(x_emu_post),
      is.finite(x_time),
      is.finite(x_time_post),
      is.finite(x_time_emu),
      is.finite(x_time_emu_post),
      is.finite(x_ehb),
      is.finite(x_ehb_post),
      is.finite(x_ehb_emu),
      is.finite(x_ehb_emu_post)
    )
  
  # --------------------------------------------------
  # 2. Regression formula
  # --------------------------------------------------
  
  reg_formula <- as.formula(
    paste0(
      yvar,
      " ~ x + x_post + x_emu + x_emu_post",
      " + x_time + x_time_post + x_time_emu + x_time_emu_post",
      " + x_ehb + x_ehb_post + x_ehb_emu + x_ehb_emu_post"
    )
  )
  
  # --------------------------------------------------
  # 3. First-stage country FE regression
  # --------------------------------------------------
  
  panel_df_tmp <- pdata.frame(
    reg_df_tmp,
    index = c("iso", "year")
  )
  
  reg_stage1_tmp <- plm(
    formula = reg_formula,
    data    = panel_df_tmp,
    model   = "within",
    effect  = "individual"
  )
  
  # Add first-stage residuals
  reg_df_tmp$resid_stage1 <- as.numeric(residuals(reg_stage1_tmp))
  
  # --------------------------------------------------
  # 4. Country-specific residual standard deviation
  # --------------------------------------------------
  
  sigma_by_country_tmp <- reg_df_tmp %>%
    group_by(iso) %>%
    summarise(
      sigma_i = sd(resid_stage1, na.rm = TRUE),
      n_resid = sum(is.finite(resid_stage1)),
      .groups = "drop"
    )
  
  # --------------------------------------------------
  # 5. Second-stage weights: inverse standard deviation
  # --------------------------------------------------
  
  reg_df_tmp_w <- reg_df_tmp %>%
    left_join(sigma_by_country_tmp, by = "iso") %>%
    mutate(
      weight_i = 1 / sigma_i
    ) %>%
    filter(
      is.finite(weight_i),
      weight_i > 0,
      n_resid >= 2
    )
  
  # --------------------------------------------------
  # 6. Second-stage weighted country FE regression
  # --------------------------------------------------
  
  panel_df_tmp_w <- pdata.frame(
    reg_df_tmp_w,
    index = c("iso", "year")
  )
  
  reg_stage2_tmp <- plm(
    formula = reg_formula,
    data    = panel_df_tmp_w,
    model   = "within",
    effect  = "individual",
    weights = weight_i
  )
  
  return(
    list(
      stage1 = reg_stage1_tmp,
      stage2 = reg_stage2_tmp,
      sigma_by_country = sigma_by_country_tmp,
      reg_data_weighted = reg_df_tmp_w
    )
  )
}

cons_two_stage_emu_break <- run_two_stage_ehb_emu_break_reg(
  df        = reg_cons_df,
  yvar      = "cons_dev",
  ehb_var   = "EHB_dev",
  post_year = 1999,
  end_year  = 2019
)

gni_two_stage_emu_break <- run_two_stage_ehb_emu_break_reg(
  df        = reg_gni_df,
  yvar      = "gni_dev",
  ehb_var   = "EHB_dev",
  post_year = 1999,
  end_year  = 2019
)

cons_two_stage_maastricht_1992 <- run_two_stage_ehb_emu_break_reg(
  df        = reg_cons_df,
  yvar      = "cons_dev",
  ehb_var   = "EHB_dev",
  post_year = 1992,
  end_year  = 2019
)


gni_two_stage_maastricht_1992 <- run_two_stage_ehb_emu_break_reg(
  df        = reg_gni_df,
  yvar      = "gni_dev",
  ehb_var   = "EHB_dev",
  post_year = 1992,
  end_year  = 2019
)

summary(gni_two_stage_emu_break$stage2)
summary(cons_two_stage_emu_break)
summary(cons_two_stage_maastricht_1992)
summary(gni_two_stage_maastricht_1992)

#####Testen ob ein structural break

cons_model <- cons_two_stage_maastricht_1992$stage2

linearHypothesis(
  cons_model,
  c(
    "x_emu_post = 0",
    "x_time_emu_post = 0",
    "x_ehb_emu_post = 0"
  ),
  test = "F"
)

gni_model <- gni_two_stage_maastricht_1992$stage2


linearHypothesis(
  gni_model,
  c(
    "x_emu_post = 0",
    "x_time_emu_post = 0",
    "x_ehb_emu_post = 0"
  ),
  test = "F"
)

# ==========================
# Consumption: 1999 EMU test
# ==========================

cons_model_1999 <- cons_two_stage_emu_break$stage2


linearHypothesis(
  cons_model_1999,
  c(
    "x_emu_post = 0",
    "x_time_emu_post = 0",
    "x_ehb_emu_post = 0"
  ),
  test  = "F"
)

gni_model_1999 <- gni_two_stage_emu_break$stage2

linearHypothesis(
  gni_model_1999,
  c(
    "x_emu_post = 0",
    "x_time_emu_post = 0",
    "x_ehb_emu_post = 0"
  ),
  test = "F"
)


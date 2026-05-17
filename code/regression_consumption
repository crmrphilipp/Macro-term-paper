#### The main regression: Consumption Risk Sharing
library(dplyr)
library(tidyr)
library(purrr)
library(plm)

# load data
oecd_data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

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
    GNI = GNI_OBS_VALUE,
    NNI = NNI_OBS_VALUE,
    consumption_pc = consumption_OBS_VALUE_per_capita,
    GDP_pc = GDP_OBS_VALUE_per_capita,
    GNI_pc = GNI_OBS_VALUE_per_capita,
    NNI_pc = NNI_OBS_VALUE_per_capita
  )

# Analysing missing data
missing_wide <- df_oecd %>%
  pivot_longer(
    cols = c(GDP, GNI, NNI, consumption,
             GDP_pc, GNI_pc, NNI_pc, consumption_pc),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(is.na(value)) %>%
  group_by(REF_AREA, variable) %>%
  summarise(
    first_missing_year = min(TIME_PERIOD),
    last_missing_year = max(TIME_PERIOD),
    n_missing = n(),
    missing_years = paste(TIME_PERIOD, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(REF_AREA, variable)


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
df_oecd_aggregate <- df_oecd %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_real_total = sum(GDP, na.rm = TRUE),
    pop_gdp_total  = sum(Population[!is.na(GDP) & !is.na(Population)], na.rm = TRUE),
    
    consumption_real_total = sum(consumption, na.rm = TRUE),
    pop_consumption_total  = sum(Population[!is.na(consumption) & !is.na(Population)], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    gdp_total_pc = gdp_real_total / pop_gdp_total,
    consumption_total_pc = consumption_real_total / pop_consumption_total
  ) %>%
  mutate(
    gdp_log_diff_total = log(gdp_total_pc) - log(dplyr::lag(gdp_total_pc)),
    consumption_log_diff_total = log(consumption_total_pc) - log(dplyr::lag(consumption_total_pc))
  )


# join the aggregate growth rates
df_oecd <- df_oecd %>%
  left_join(
    df_oecd_aggregate %>%
      select(TIME_PERIOD, gdp_log_diff_total, consumption_log_diff_total),
    by = "TIME_PERIOD"
  )


# growth rate deviations
df_oecd <- df_oecd %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total,
    consumption_dev = consumption_log_diff - consumption_log_diff_total
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


summary(reg_cons_stage2)


# Paper-style average consumption risk sharing
h0 <- coef(reg_cons_stage2)["gdp_dev"]
avg_consumption_risk_sharing <- 100 * (1 - h0)

avg_consumption_risk_sharing

write_csv(reg_df, "../data/reg_df.csv")

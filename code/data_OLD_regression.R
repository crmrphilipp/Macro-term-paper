#### The main regression: Consumption Risk Sharing
rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
oecd_data <- read.csv("../data/oecd2005_with_ppp2000.csv")


df_oecd <- oecd_data

### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

### Make sure variables have the right type
df_oecd <- df_oecd %>%
  mutate(
    year = as.numeric(year),
    gdp_const2000 = as.numeric(gdp_const2000),
    cons_const2000 = as.numeric(cons_const2000),
    pop_thousands = as.numeric(pop_thousands)
  )

  #### Bringing all countries to the same scale
df_oecd <- df_oecd %>%
  mutate(
    gdp_const2000_million = if_else(
      iso %in% c("JPN", "TUR", "USA"),
      gdp_const2000 * 1000,
      gdp_const2000
    ),
    cons_const2000_million = if_else(
      iso %in% c("JPN", "TUR", "USA"),
      cons_const2000 * 1000,
      cons_const2000
    )
  )

df_oecd <- df_oecd %>%
 mutate(
    gdp_const_usd = gdp_const2000_million/ppp_gdp,
    cons_const_usd = cons_const2000_million/ppp_actual_individual_consumption
    )

### Keep regression sample countries
df_oecd <- df_oecd %>%
  filter(iso %in% reg_sample)

#### Creating per capita values
df_oecd <- df_oecd %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(
    GDP_pc = gdp_const_usd / pop_thousands,
    consumption_pc = cons_const_usd / pop_thousands
  ) %>%
  ungroup()

#### Per capita growth rates: GDP and consumption
df_oecd <- df_oecd %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    consumption_log_diff = log(consumption_pc) - log(dplyr::lag(consumption_pc))
  ) %>%
  ungroup()

### Aggregate per capita growth rates: GDP and consumption
### Common country-year sample: GDP, consumption and population must all be available

df_oecd_aggregate2 <- df_oecd %>%
  filter(
    !is.na(gdp_const_usd),
    !is.na(cons_const_usd),
    !is.na(pop_thousands)
  ) %>%
  group_by(year) %>%
  summarise(
    gdp_total2 = sum(gdp_const_usd, na.rm = TRUE),
    consumption_total2 = sum(cons_const_usd, na.rm = TRUE),
    population_total2 = sum(pop_thousands, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(
    gdp_total_pc2 = gdp_total2 / population_total2,
    consumption_total_pc2 = consumption_total2 / population_total2,
    
    gdp_log_diff_total2 =
      log(gdp_total_pc2) - log(dplyr::lag(gdp_total_pc2)),
    
    consumption_log_diff_total2 =
      log(consumption_total_pc2) - log(dplyr::lag(consumption_total_pc2))
  )

### Join the aggregate growth rates
df_oecd <- df_oecd %>%
  left_join(
    df_oecd_aggregate2 %>%
      select(year, gdp_log_diff_total2, consumption_log_diff_total2),
    by = "year"
  )

### Growth rate deviations
df_oecd <- df_oecd %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total2,
    consumption_dev = consumption_log_diff - consumption_log_diff_total2
  )

### Keep regression period
df_oecd <- df_oecd %>%
  filter(year > 1992, year < 2004)

# load home bias data
ehb_data <- read.csv('../data/ehb_reg_small.csv')
ehb_crude_data <- read.csv('../data/ehb_crude_reg_small.csv')


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
summary(reg_cons_stage1)


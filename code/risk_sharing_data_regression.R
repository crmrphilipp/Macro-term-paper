# ══════════════════════════════════════════════════════════════════════════════
#  
# Getting GDP, GNI and Consumption data from the OECD (Data Explorer)
# The following code extracts:
# 1. Annual GDP, US Dollars, PPP converted, Chain linked volume (rebased), 2020, Millions
# 2. Annual final Consumption Expenditure (HH + Government), US Dollars, PPP converted, Chain linked volume (rebased), 2020, Millions
# 3. Annual GNI, US Dollars, PPP converted, Chain linked volume (rebased), 2020, Millions
# 4. Annual Net National Income, US Dollars, PPP converted, Chain linked volume (rebased), 2020, Millions
# 5. Population data to compute per capita values
# and merges to an export-ready dataset.

# Note: The reference paper computes PPP-adjusted per capita values by hand. It chooses 1995 as the base year. We choose the OECD default 
# computation of constant prices/ real GDP which are based on a chained-linked volume approach, rebased to 2020. Therefore, we do not 
# make any discretionary choices regarding the computation of deflated/ PPP adjusted values.
#
# ══════════════════════════════════════════════════════════════════════════════

############ 0. Preliminaries ############
# Clear workspace, set working directory
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(stargazer)
library(readr)
library(WDI)
library(readxl)
library(kableExtra)

# Define allowed countries
country_selection_replication <- c(
  "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
  "Türkiye", "United Kingdom", "United States"
)

country_selection <- country_selection_replication # SELECT COUNTRIES FOR ANALYSIS!!!

############ 1. + 2. Annual GDP and Consumption data ############
# Define relevant SDMX code
sdmx_gdp_cons  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A.WXOECD+ALB+ARG+BRA+BGR+CPV+CMR+CHN+HRV+CYP+GEO+HKG+IND+IDN+MDG+MLT+MAR+MKD+ROU+SAU+SEN+SRB+SGP+ZAF+ZMB+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA20+EU15+EU27_2020+OECD+OECD26+OECDE+AUS.S1..B1GQ+P3._Z...USD_PPP.LR.N.?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
gdp_cons <- "../data/gdp_cons.csv"

dir.create("../data", showWarnings = FALSE)

if (!file.exists(gdp_cons)) {
  download.file(sdmx_gdp_cons, destfile = gdp_cons, mode = "wb")
  message("gdp dataset downloaded.")
}

# Load the main data sheet 
gdp_cons_data <- read_csv(gdp_cons)

gdp_cons_data <- gdp_cons_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Gross domestic product" ~ "GDP",
    Transaction == "Final consumption expenditure" ~ "consumption"
  )) %>%
  # 2. Rename the Transaction column to GDP_cons
  rename(GDP_cons = Transaction) %>%
  # 3. Keep only countries for analysis and relevant variables
  filter(`Reference area` %in% country_selection) %>%
  select(REF_AREA, TIME_PERIOD, GDP_cons, OBS_VALUE)


############## 3. + 4. Annual GNI and Net National Income data ##############
# Define relevant SDMX code
sdmx_gni_nni <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,2.0/A.AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+SWE+TUR+GBR+USA+EA20+EU27_2020+BRA+BGR+CPV+CMR+CHN+HRV+GEO+HKG+KAZ+MAR+ROU+RUS+SAU+SEN+SGP+ZAF+AUS...B5G+B5N....USD_PPP.LR..?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
gni_nni <- "../data/gni_nni.csv"

if (!file.exists(gni_nni)) {
  download.file(sdmx_gni_nni, destfile = gni_nni, mode = "wb")
  message("gni and nni dataset downloaded.")
}

# Load the main data sheet
gni_nni_data <- read_csv(gni_nni)

# Select relevant variables/ observations
gni_nni_data <- gni_nni_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Balance of primary incomes, net / National income, net" ~ "NNI",
    Transaction == "Balance of primary incomes, gross / National income, gross" ~ "GNI",
  )) %>%
  # 2. Rename the column
  rename(GNI_NNI = Transaction) %>%
  # 3. Keep only the countries selected for analysis and relevant variables
  filter(`Reference area` %in% country_selection) %>%
  select(REF_AREA, TIME_PERIOD, GNI_NNI, OBS_VALUE)


############## 5. Population data ##############
# Define relevant SDMX code
sdmx_pop <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+G20+EU27+OECD+ARG+BRA+BGR+CHN+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF+W+AUS.POP.PS._T._T.?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
pop <- "../data/gdp_pop.csv"

if (!file.exists(pop)) {
  download.file(sdmx_pop, destfile = pop, mode = "wb")
  message("population dataset downloaded.")
}

# Load the main data sheet 
pop_data <- read_csv(pop)

# Keep relevant countries and values
pop_data <- pop_data %>%
  filter(`Reference area` %in% country_selection) %>%
  rename(Population = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, Population)

test <- gni_nni_data %>%
  filter(REF_AREA == "DNK")

################ 6. Merge datasets and compute per capita values ##############
# Merge datasets
combined_data <- gdp_cons_data %>%
  rename(measure = GDP_cons) %>%
  bind_rows(gni_nni_data %>% rename(measure = GNI_NNI))

# Add NAs if some values are missing, then order by country, then year
final_data <- combined_data %>%
  complete(
    REF_AREA, 
    TIME_PERIOD = 1992:2024, 
    measure
  ) %>%
  arrange(REF_AREA, TIME_PERIOD)

# Add per capita values
final_data_cap <- final_data %>%
  # 1. Join the population data matching on both country and year
  left_join(pop_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
  
  # 2. Calculate the per capita value
  mutate(OBS_VALUE_per_capita = OBS_VALUE / Population)

# Add aggregate values
aggregate_rows <- final_data_cap %>%
  group_by(TIME_PERIOD, measure) %>%
  summarise(
    # Calculate sum of population FOR WHICH THE MEASURE IS AVAILABLE in a given year
    valid_pop_sum = sum(Population[!is.na(OBS_VALUE)], na.rm = TRUE), 
    
    # Calculate sum ofgiven measure in a given year
    OBS_VALUE = sum(OBS_VALUE, na.rm = TRUE), 
    
    # Drop groups
    .groups = "drop"
  ) %>%
  mutate(REF_AREA = "Aggregate",
         Population = valid_pop_sum,
         OBS_VALUE_per_capita = OBS_VALUE / valid_pop_sum) %>% 
  select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita)

final_data_cap <- bind_rows(final_data_cap, aggregate_rows)

# Export the final dataset
write_csv(final_data_cap, "../data/gdp_gni_consumption_per_capita.csv")

#### The main regression: Consumption Risk Sharing
rm(list = ls())
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


summary(reg_cons_stage2)

# Paper-style average consumption risk sharing
h0 <- coef(reg_cons_stage2)["gdp_dev"]
avg_consumption_risk_sharing <- 100 * (1 - h0)

avg_consumption_risk_sharing

write_csv(reg_df, "../data/reg_df.csv")


#### 2nd Regression with Crude EHB measure

# clean estimation sample
reg_crude_df <- merged_df %>%
  filter(
    !is.na(consumption_dev),
    !is.na(gdp_dev),
    !is.na(ehb_crude_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)

# Mexico 2 missings
# Greece 2 missings
# Turkye 5 missings

# First step regression with country FE
panel_crude_df <- pdata.frame(reg_crude_df, index = c("iso", "year"))

reg_cons_crude_stage1 <- plm(
  consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(ehb_crude_dev * gdp_dev),
  data   = panel_crude_df,
  model  = "within",
  effect = "individual"
)


# Error term SD
reg_crude_df$resid_stage1 <- as.numeric(residuals(reg_cons_crude_stage1))

sigma_crude_by_country <- reg_crude_df %>%
  group_by(iso) %>%
  summarise(
    sigma_i = sd(resid_stage1, na.rm = TRUE),
    n_resid = sum(!is.na(resid_stage1)),
    .groups = "drop"
  )


# Creating country weights
reg_crude_df_w <- reg_crude_df %>%
  left_join(sigma_crude_by_country, by = "iso") %>%
  mutate(weight_i = 1 / sigma_i)


# Second step regression
panel_crude_df_w <- pdata.frame(reg_crude_df_w, index = c("iso", "year"))

reg_crude_cons_stage2 <- plm(
  consumption_dev ~ gdp_dev + I(time * gdp_dev) + I(ehb_crude_dev * gdp_dev),
  data    = panel_crude_df_w,
  model   = "within",
  effect  = "individual",
  weights = weight_i
)


summary(reg_crude_cons_stage2)


# Paper-style average consumption risk sharing
h0 <- coef(reg_cons_stage2)["gdp_dev"]
avg_consumption_risk_sharing <- 100 * (1 - h0)

avg_consumption_risk_sharing

write_csv(reg_df, "../data/reg_df.csv")



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
library(readxl)

# Define allowed countries
country_selection_replication <- c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece",
    "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
    "Türkiye", "United Kingdom", "United States"
)

country_selection <- country_selection_replication # SELECT COUNTRIES FOR ANALYSIS!!!

############ 1. + 2. Annual GDP and Consumption data ############
# Define relevant SDMX code
#sdmx_gdp_cons  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A.WXOECD+ALB+ARG+BRA+BGR+CPV+CMR+CHN+HRV+CYP+GEO+HKG+IND+IDN+MDG+MLT+MAR+MKD+ROU+SAU+SEN+SRB+SGP+ZAF+ZMB+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA20+EU15+EU27_2020+OECD+OECD26+OECDE+AUS.S1..B1GQ+P3._Z...USD_PPP.LR.N.?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
sdmx_gdp_cons <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A.AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA20+EU27_2020+ALB+ARG+BRA+BGR+CPV+CMR+CHN+HRV+CYP+GEO+HKG+IND+IDN+KAZ+MDG+MLT+MAR+MKD+ROU+RUS+SAU+SEN+SRB+SGP+ZAF+ZMB+AUS.S1..B1GQ+P3._Z...USD_PPP.LR.N.?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
gdp_cons <- "../data/gdp_cons.csv"

dir.create("../data", showWarnings = FALSE)

download.file(sdmx_gdp_cons, destfile = gdp_cons, mode = "wb")
message("gdp dataset downloaded.")

# Load the main data sheet 
gdp_cons_data <- read_csv(gdp_cons)

gdp_cons_data <- gdp_cons_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Gross domestic product" ~ "GDP",
    Transaction == "Final consumption expenditure" ~ "consumption"
  )) %>%
  # 2. Rename the Transaction column to GDP_cons and multiply by 1million to get correct values
  rename(GDP_cons = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Keep only countries for analysis and relevant variables
  filter(`Reference area` %in% country_selection) %>%
  select(REF_AREA, TIME_PERIOD, GDP_cons, OBS_VALUE) %>%
  arrange(REF_AREA, GDP_cons, TIME_PERIOD)


############## 3. + 4. Annual GNI and Net National Income data ##############
# Define relevant SDMX code
sdmx_gni_nni <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,2.0/A.AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+SWE+TUR+GBR+USA+EA20+EU27_2020+BRA+BGR+CPV+CMR+CHN+HRV+GEO+HKG+KAZ+MAR+ROU+RUS+SAU+SEN+SGP+ZAF+AUS...B5G+B5N....USD_PPP.LR..?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
gni_nni <- "../data/gni_nni.csv"

download.file(sdmx_gni_nni, destfile = gni_nni, mode = "wb")
message("gni and nni dataset downloaded.")

# Load the main data sheet
gni_nni_data <- read_csv(gni_nni)

# Select relevant variables/ observations
gni_nni_data <- gni_nni_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Balance of primary incomes, net / National income, net" ~ "NNI",
    Transaction == "Balance of primary incomes, gross / National income, gross" ~ "GNI",
  )) %>%
  # 2. Rename the column and multiply by 1million to get correct values
  rename(GNI_NNI = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Keep only the countries selected for analysis and relevant variables
  filter(`Reference area` %in% country_selection) %>%
  select(REF_AREA, TIME_PERIOD, GNI_NNI, OBS_VALUE) %>%
  arrange(REF_AREA, GNI_NNI, TIME_PERIOD)


############## 5. Population data ##############
# Define relevant SDMX code
sdmx_pop <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+G20+EU27+OECD+ARG+BRA+BGR+CHN+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF+W+AUS.POP.PS._T._T.?startPeriod=1992&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
pop <- "../data/gdp_pop.csv"

download.file(sdmx_pop, destfile = pop, mode = "wb")
message("population dataset downloaded.")

# Load the main data sheet 
pop_data <- read_csv(pop)

# Keep relevant countries and values
pop_data <- pop_data %>%
  filter(`Reference area` %in% country_selection) %>%
  rename(Population = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, Population) %>%
  arrange(REF_AREA, TIME_PERIOD)
  
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
    arrange(REF_AREA, measure, TIME_PERIOD)

# Add per capita values
final_data_cap <- final_data %>%
  # 1. Join the population data matching on both country and year
  left_join(pop_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
  
  # 2. Calculate the per capita value
  mutate(OBS_VALUE_per_capita = OBS_VALUE / Population)

############# Consumption / GDP dataset
final_data_cap_cy <- final_data_cap %>%
  filter(measure %in% c("consumption", "GDP")) %>%
  select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
  arrange(REF_AREA, measure, TIME_PERIOD)

# Calculate aggregate values based on complete cases only
aggregate_rows_cy <- final_data_cap_cy %>%

  # Filter on complete cases
  group_by(REF_AREA, TIME_PERIOD) %>%
  filter(
    any(measure == "GDP" & !is.na(OBS_VALUE)) &
    any(measure == "consumption" & !is.na(OBS_VALUE)) &
    all(!is.na(Population))
  ) %>%
  
  # Calculate aggregate values
  group_by(TIME_PERIOD, measure) %>%
  summarise(
    valid_pop_sum = sum(Population, na.rm = TRUE), 
    OBS_VALUE_sum = sum(OBS_VALUE, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  
  # Final formatting
  mutate(
    REF_AREA = "Aggregate",
    Population = valid_pop_sum,
    OBS_VALUE = OBS_VALUE_sum,
    OBS_VALUE_per_capita = OBS_VALUE_sum / valid_pop_sum
  ) %>% 
  select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
  arrange(REF_AREA, measure, TIME_PERIOD)

final_data_cap_cy <- bind_rows(final_data_cap_cy, aggregate_rows_cy)

# Export the final dataset
write_csv(final_data_cap_cy, "../data/data_cy.csv")

################ GNI and GDP dataset
... # to be completed


# aggregate_rows <- final_data_cap %>%
#   group_by(TIME_PERIOD, measure) %>%
#   summarise(
#     # Calculate sum of population FOR WHICH THE MEASURE IS AVAILABLE in a given year
#     valid_pop_sum = sum(Population[!is.na(OBS_VALUE)], na.rm = TRUE), 

#     # Calculate sum of given measure in a given year
#     OBS_VALUE_sum = sum(OBS_VALUE, na.rm = TRUE), 

#     # Drop groups
#     .groups = "drop"
#   ) %>%
#   mutate(REF_AREA = "Aggregate",
#         Population = valid_pop_sum,
#         OBS_VALUE = OBS_VALUE_sum,
#         OBS_VALUE_per_capita = OBS_VALUE_sum / valid_pop_sum) %>% 
#   select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
#   arrange(REF_AREA, measure, TIME_PERIOD)

# final_data_cap <- bind_rows(final_data_cap, aggregate_rows)
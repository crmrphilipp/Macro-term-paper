# ══════════════════════════════════════════════════════════════════════════════
#  
# Getting GDP, GNI and Consumption data from the OECD (Data Explorer)
# The following code extracts:
    # 1. Annual GDP, nominal, national currency, millions (two data sources)
    # 2. Annual final Consumption Expenditure (HH + Government), nominal,national currency, millions
    # 3. Annual GNI, nominal, national currency, millions
    # 4. CPI data for computation of real values (base year 2015), PPP exchange rates for base year 2015, population data to compute per capita values
    # and merges to an export-ready dataset.
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
library(ggplot2)

############ 1. + 2. Annual GDP and Consumption data (nominal, current prices) ############
# Define relevant SDMX code
sdmx_gdp_cons <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A.AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA20+EU27_2020+ALB+ARG+BRA+BGR+CPV+CMR+CHN+HRV+CYP+GEO+HKG+IND+IDN+KAZ+MDG+MLT+MAR+MKD+ROU+RUS+SAU+SEN+SRB+SGP+ZAF+ZMB+AUS.S1..B1GQ+P3._Z...XDC.V.N.?startPeriod=1969&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
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
  rename(GDP_CONS = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Select and arrange necessary variables
  select(`Reference area`, REF_AREA, TIME_PERIOD, GDP_CONS, OBS_VALUE) %>%
  arrange(REF_AREA, GDP_CONS, TIME_PERIOD)

############## 3. Annual GNI data ##############
sdmx_gdp_gni <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,/A.AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+POL+PRT+SVK+SVN+ESP+SWE+TUR+GBR+USA+EA20+EU27_2020+BRA+BGR+CPV+CMR+CHN+HRV+GEO+HKG+KAZ+MAR+ROU+RUS+SAU+SEN+SGP+ZAF+AUS...B1GQ+B5G....XDC.V..?startPeriod=1969&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
gdp_gni <- "../data/gdp_gni.csv"

download.file(sdmx_gdp_gni, destfile = gdp_gni, mode = "wb")
message("gdp_gni dataset downloaded.")

# Load the main data sheet
gdp_gni_data <- read_csv(gdp_gni)

# Select relevant variables/ observations
gdp_gni_data <- gdp_gni_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Gross domestic product" ~ "GDP",
    Transaction == "Balance of primary incomes, gross / National income, gross" ~ "GNI",
  )) %>%
  # 2. Rename the column and multiply by 1million to get correct values
  rename(GDP_GNI = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Select and arrange necessary variables
  select(`Reference area`, REF_AREA, TIME_PERIOD, GDP_GNI, OBS_VALUE) %>%
  arrange(REF_AREA, GDP_GNI, TIME_PERIOD)

################## 4. Sanity check: Compare GDP values in both datasets for replication time period ##############
# Define comparison dataframe
country_selection_replication <- c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece",
    "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
    "Türkiye", "United Kingdom", "United States"
)
time_frame_replication <- 1992:2003

gdp_comparison <- gdp_cons_data %>%
  filter(GDP_CONS == "GDP", TIME_PERIOD %in% time_frame_replication, `Reference area` %in% country_selection_replication) %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  rename(GDP_CONS = OBS_VALUE) %>%
  left_join(
    gdp_gni_data %>% filter(GDP_GNI == "GDP") %>% select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>% rename(GDP_GNI = OBS_VALUE),
    by = c("REF_AREA", "TIME_PERIOD")
  )

# Find mismatches, isolate available mismatches and plot them
mismatch_data <- gdp_comparison %>%
  filter(!is.na(GDP_CONS) & !is.na(GDP_GNI)) %>% 
  filter(!near(GDP_CONS, GDP_GNI))

# Plot them
ggplot(mismatch_data, aes(x = GDP_CONS, y = GDP_GNI)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Mismatched GDP Values",
    x = "GDP (Cons)",
    y = "GDP (GNI)"
  ) +
  theme_minimal()

# --> Conclusion: We use GDP from the respective different datasets to be consistent within each dataset.
# --> We do choose one of the GDP reports because i) there are near identical in any case and ii) we have to adjust GDP values used according to data availability anyways.

############## 5. Load CPI, PPPs (base year: 2015) and population and get export-ready dataset for GDP/consumption and GDP/GNI #####################
################## Load relevant datasets ####################
###### CPI (base year 2015)
sdmx_cpi <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/.A.N.CPI.._T.N._Z?startPeriod=1969&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
cpi <- "../data/cpi.csv"
download.file(sdmx_cpi, destfile = cpi, mode = "wb")
message("cpi dataset downloaded.")

cpi_data <- read_csv(cpi)
# Clean CPI data
cpi_data <- cpi_data %>%
  rename(CPI = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, CPI) %>%
  arrange(REF_AREA, TIME_PERIOD)

######## PPPs (base year 2015)
sdmx_ppp <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,2.0/A.EA20+AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?startPeriod=2015&endPeriod=2015&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
ppp <- "../data/ppp.csv"
download.file(sdmx_ppp, destfile = ppp, mode = "wb")
message("PPP dataset downloaded.")

ppp_data <- read_csv(ppp)
# Clean PPP data
ppp_data <- ppp_data %>%
  rename(PPP = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, PPP) %>%
  arrange(REF_AREA, TIME_PERIOD)

######### Population
sdmx_pop <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+G20+EU27+OECD+ARG+BRA+BGR+CHN+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF+W+AUS.POP.PS._T._T.?startPeriod=1969&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
pop <- "../data/pop.csv"
download.file(sdmx_pop, destfile = pop, mode = "wb")
message("population dataset downloaded.")

pop_data_temp <- read_csv(pop)
# Clean Population data
pop_data_temp <- pop_data_temp %>%
  rename(Population = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, Population) %>%
  arrange(REF_AREA, TIME_PERIOD)

pop_data <- pop_data_temp

####################### Begin writing data cleaning function here
data_cleaning_function <- function(gdp_cons_dataset, gdp_gni_dataset, time_frame,
                                    country_selection, cpi_data, ppp_data, pop_data){
########### Filter according to country selection and time period
gdp_cons_dataset <- gdp_cons_dataset %>%
  filter(`Reference area` %in% country_selection & `TIME_PERIOD` %in% time_frame) %>%
  select(-`Reference area`)

gdp_gni_dataset <- gdp_gni_dataset %>%
  filter(`Reference area` %in% country_selection & `TIME_PERIOD` %in% time_frame) %>%
  select(-`Reference area`)

############ In a loop, clean data for GDP/consumption and GNI/consumption data
raw_datasets <- list(
  cons = gdp_cons_dataset,
  gni = gdp_gni_dataset
)
final_datasets <- list()

# Loop through cons and gni
for (type in names(raw_datasets)) {

  gdp_col <- paste0("gdp_", type)
  
  # Filter for available observations across specified time frame
  filtered_data <- raw_datasets[[type]] %>%
    filter(TIME_PERIOD %in% time_frame) %>%
    group_by(REF_AREA) %>%
    filter(sum(!is.na(OBS_VALUE)) == 2 * length(time_frame)) %>%
    ungroup()
  
  # CPI Adjust
  real_data <- filtered_data %>%
    left_join(cpi_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
    mutate(real_OBS_VALUE = OBS_VALUE / (CPI/100)) %>%
    select(REF_AREA, TIME_PERIOD, all_of(toupper(gdp_col)), real_OBS_VALUE)
  
  # PPP Adjust
  final_datasets[[type]] <- real_data %>%
    left_join(ppp_data, by = "REF_AREA") %>%
    rename(TIME_PERIOD = TIME_PERIOD.x) %>%
    mutate(OBS_VALUE = real_OBS_VALUE / PPP) %>%
    select(REF_AREA, TIME_PERIOD, all_of(toupper(gdp_col)), OBS_VALUE) %>%
    arrange(REF_AREA, .data[[toupper(gdp_col)]], TIME_PERIOD)
}

# Extract the processed dataframes back to environment
final_data_cons <- final_datasets[["cons"]]
final_data_gni <- final_datasets[["gni"]]

############### 6. Final cleaning (per capita values, aggregate values) ##############
# Loop configuration list
loop_config <- list(
  cy = list(
    data = final_data_cons,
    gdp_col = "GDP_CONS",
    filter_terms = c("consumption", "GDP")
  ),
  iy = list(
    data = final_data_gni,
    gdp_col = "GDP_GNI",
    filter_terms = c("GNI", "GDP")
  )
)

final_cap_results <- list()

# Loop
for (output_name in names(loop_config)) {
  
  # Running variables
  current_data <- loop_config[[output_name]]$data
  col_name <- loop_config[[output_name]]$gdp_col
  terms <- loop_config[[output_name]]$filter_terms
  
  # Add pop data and standardize column name
  main_data <- current_data %>%
    left_join(pop_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
    mutate(OBS_VALUE_per_capita = OBS_VALUE / Population) %>%
    rename(measure = all_of(col_name)) %>%
    filter(measure %in% terms) %>%
    select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
    arrange(REF_AREA, measure, TIME_PERIOD)
  
  # Calculate aggregate values
  aggregate_rows <- main_data %>%
    group_by(TIME_PERIOD, measure) %>%
    summarise(
      valid_pop_sum = sum(Population, na.rm = TRUE), 
      OBS_VALUE_sum = sum(OBS_VALUE, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    mutate(
      REF_AREA = "Aggregate",
      Population = valid_pop_sum,
      OBS_VALUE = OBS_VALUE_sum,
      OBS_VALUE_per_capita = OBS_VALUE_sum / valid_pop_sum
    ) %>% 
    select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
    arrange(REF_AREA, measure, TIME_PERIOD)
  
  # Bind rows and store in list
  final_cap_results[[output_name]] <- bind_rows(main_data, aggregate_rows)
}

return(list(
    consumption_data = final_cap_results[["cy"]],
    gni_data = final_cap_results[["iy"]]
  ))

}

data_cleaning_sanity <- function(gdp_cons_dataset, gdp_gni_dataset, time_frame,
                                    country_selection, pop_data){
########### Filter according to country selection and time period
gdp_cons_dataset <- gdp_cons_dataset %>%
  filter(`Reference area` %in% country_selection & `TIME_PERIOD` %in% time_frame) %>%
  select(-`Reference area`)

gdp_gni_dataset <- gdp_gni_dataset %>%
  filter(`Reference area` %in% country_selection & `TIME_PERIOD` %in% time_frame) %>%
  select(-`Reference area`)

# Extract the processed dataframes back to environment
final_data_cons <- gdp_cons_dataset
final_data_gni <- gdp_gni_dataset
############### 6. Final cleaning (per capita values, aggregate values) ##############
# Loop configuration list
loop_config <- list(
  cy = list(
    data = final_data_cons,
    gdp_col = "GDP_CONS",
    filter_terms = c("consumption", "GDP")
  ),
  iy = list(
    data = final_data_gni,
    gdp_col = "GDP_GNI",
    filter_terms = c("GNI", "GDP")
  )
)

final_cap_results <- list()

# Loop
for (output_name in names(loop_config)) {
  
  # Running variables
  current_data <- loop_config[[output_name]]$data
  col_name <- loop_config[[output_name]]$gdp_col
  terms <- loop_config[[output_name]]$filter_terms
  
  # Add pop data and standardize column name
  main_data <- current_data %>%
    left_join(pop_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
    mutate(OBS_VALUE = OBS_VALUE * 1000000000) %>%
    mutate(OBS_VALUE_per_capita = (OBS_VALUE) / Population) %>%
    rename(measure = all_of(col_name)) %>%
    filter(measure %in% terms) %>%
    select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
    arrange(REF_AREA, measure, TIME_PERIOD)
  
  # Calculate aggregate values
  aggregate_rows <- main_data %>%
    group_by(TIME_PERIOD, measure) %>%
    summarise(
      valid_pop_sum = sum(Population, na.rm = TRUE), 
      OBS_VALUE_sum = sum(OBS_VALUE, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    mutate(
      REF_AREA = "Aggregate",
      Population = valid_pop_sum,
      OBS_VALUE = OBS_VALUE_sum,
      OBS_VALUE_per_capita = OBS_VALUE_sum / valid_pop_sum
    ) %>% 
    select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE, Population, OBS_VALUE_per_capita) %>%
    arrange(REF_AREA, measure, TIME_PERIOD)
  
  # Bind rows and store in list
  final_cap_results[[output_name]] <- bind_rows(main_data, aggregate_rows)
}

return(list(
    consumption_data = final_cap_results[["cy"]],
    gni_data = final_cap_results[["iy"]]
  ))

}

####################### REPLICATION
########## Current OECD data
# Define variables for function and run function
country_selection_replication <- c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece",
    "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
    "Türkiye", "United Kingdom", "United States"
)
time_frame_replication <- 1992:2003 # Note we start in 1992 because data will be lagged!
data_sets_replication <- data_cleaning_function(gdp_cons_dataset = gdp_cons_data, gdp_gni_dataset = gdp_gni_data,
                                                time_frame = time_frame_replication, country_selection = country_selection_replication,
                                                cpi_data = cpi_data, ppp_data = ppp_data, pop_data = pop_data)

# Export the final datasets
write_csv(data_sets_replication[["consumption_data"]], "../data/data_cy_rep.csv")
write_csv(data_sets_replication[["gni_data"]], "../data/data_iy_rep.csv")

########## Old OECD data (PDF extracted)
######################## SANITY CHECK 1: USE PDF-extracted data
###### GDP/ consumption (own calculation)
pdf_data <- read_csv("../data/pdf_oecd_reg.csv")
gdp_cons_sanity <- pdf_data %>%
  rename(REF_AREA = iso, `Reference area` = country, TIME_PERIOD = year) %>%
  select(`Reference area`, REF_AREA, TIME_PERIOD, gdp_usd_ppp, cons_usd_ppp) %>%
  pivot_longer(
    cols = c("gdp_usd_ppp", "cons_usd_ppp"),
    names_to = "GDP_CONS",                   
    values_to = "OBS_VALUE"                     
  ) %>%
  mutate(
    GDP_CONS = ifelse(GDP_CONS == "gdp_usd_ppp", "GDP", "consumption")
  ) %>%
  arrange(REF_AREA, GDP_CONS)

gdp_gni_sanity <- pdf_data %>%
  rename(REF_AREA = iso, `Reference area` = country, TIME_PERIOD = year) %>%
  select(`Reference area`, REF_AREA, TIME_PERIOD, gdp_usd_ppp, gni_usd_ppp) %>%
  pivot_longer(
    cols = c("gdp_usd_ppp", "gni_usd_ppp"),
    names_to = "GDP_GNI",                   
    values_to = "OBS_VALUE"                     
  ) %>%
  mutate(
    GDP_GNI = ifelse(GDP_GNI == "gdp_usd_ppp", "GDP", "GNI")
  ) %>%
  arrange(REF_AREA, GDP_GNI)

country_selection_sanity_check <- c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece",
    "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
    "Türkiye", "United Kingdom", "United States"
)
time_frame_sanity_check <- 1992:2003
data_sets_replication <- data_cleaning_sanity(gdp_cons_dataset = gdp_cons_sanity, gdp_gni_dataset = gdp_gni_sanity,
                                                time_frame = time_frame_sanity_check, country_selection = country_selection_sanity_check, pop_data = pop_data)

write_csv(data_sets_replication[["consumption_data"]], "../data/data_cy_sanity.csv")
write_csv(data_sets_replication[["gni_data"]], "../data/data_iy_sanity.csv")

####################### EXTENSION 1: 1987 to 2017, same countries
# Define variables for function and run function
country_selection_extension_1 <- c(
    "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece",
    "Iceland", "Ireland", "Italy", "Japan", "Mexico", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland",
    "Türkiye", "United Kingdom", "United States"
)
time_frame_extension_1 <- 1986:2017
data_sets_replication <- data_cleaning_function(gdp_cons_dataset = gdp_cons_data, gdp_gni_dataset = gdp_gni_data,
                                                time_frame = time_frame_extension_1, country_selection = country_selection_extension_1,
                                                cpi_data = cpi_data, ppp_data = ppp_data, pop_data = pop_data)

# Export the final datasets
write_csv(data_sets_replication[["consumption_data"]], "../data/data_cy_ext_1.csv")
write_csv(data_sets_replication[["gni_data"]], "../data/data_iy_ext_1.csv")

####################### EXTENSION 2: 1987 to 2017, Top 50 Economies + EU (- some countries, depending on data availability)
# Define variables for function and run function
country_selection_extension_2 <- c(
    "Argentina", "Australia", "Austria", "Bangladesh", "Belgium", 
    "Brazil", "Bulgaria", "Canada", "Chile", "China", 
    "Colombia", "Croatia", "Czechia", "Denmark", "Egypt", 
    "France", "Germany", "Greece", "Hong Kong", "Hungary", 
    "India", "Indonesia", "Iran", "Israel", "Italy", 
    "Japan", "Kazakhstan", "Korea", "Latvia", "Luxembourg", 
    "Malaysia", "Mexico", "Netherlands", "Nigeria", "Norway", 
    "Pakistan", "Philippines", "Poland", "Portugal", "Romania", 
    "Russia", "Saudi Arabia", "Singapore", "Slovak Republic", "Slovenia", 
    "South Africa", "Spain", "Sweden", "Switzerland", "Thailand", 
    "Türkiye", "United Arab Emirates", "United Kingdom", "United States", 
    "Vietnam"
)

# Check which countries and which time periods are in the big sample
filtered_gdp_data <- gdp_cons_data %>%
  filter(`Reference area` %in% country_selection_extension_2)

time_periods_by_country <- split(filtered_gdp_data$TIME_PERIOD, filtered_gdp_data$`Reference area`)

time_frame_extension_2 <- 1986:2017
data_sets_replication <- data_cleaning_function(gdp_cons_dataset = gdp_cons_data, gdp_gni_dataset = gdp_gni_data,
                                                time_frame = time_frame_extension_2, country_selection = country_selection_extension_2,
                                                cpi_data = cpi_data, ppp_data = ppp_data, pop_data = pop_data)

# Export the final datasets
write_csv(data_sets_replication[["consumption_data"]], "../data/data_cy_ext_2.csv")
write_csv(data_sets_replication[["gni_data"]], "../data/data_iy_ext_2.csv")

######################### EXTENSION 3: 1987 to 2017, Eurozone
### Eurozone: Choose all countries that eventually join the Euro
country_selection_extension_3 <- c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovak Republic", "Slovenia", "Spain"
)
time_frame_extension_3 <- 1986:2017
data_sets_replication <- data_cleaning_function(gdp_cons_dataset = gdp_cons_data, gdp_gni_dataset = gdp_gni_data,
                                                time_frame = time_frame_extension_3, country_selection = country_selection_extension_4a,
                                                cpi_data = cpi_data, ppp_data = ppp_data, pop_data = pop_data)

write_csv(data_sets_replication[["consumption_data"]], "../data/data_cy_ext_3.csv")
write_csv(data_sets_replication[["gni_data"]], "../data/data_iy_ext_3.csv")

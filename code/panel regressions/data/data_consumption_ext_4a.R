# ============================================
#The main regression: Consumption Risk Sharing
#Table 3 & Table 5 in the paper 
#EXTENTION 4a Only Eurozone 1987 - 2017
#=============================================
# ============================================
# 1. Cleaning data and preparing data for regression
#=============================================

rm(list = ls())
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
setwd(
  normalizePath(
    file.path(
      dirname(rstudioapi::getActiveDocumentContext()$path),
      "..", "..", "data"
    )
  )
)

# load data
cons_ext2_raw <- read.csv("../data/data_cy_ext_3.csv")

# load home bias data
ehb_data <- read.csv("../data/ehb_top60_restr_small.csv")


### Regression sample
reg_sample <- c(
  "AUT", # Austria
  "BEL", # Belgium
  "BGR", # Bulgaria
  "HRV", # Croatia
  "CYP", # Cyprus
  "EST", # Estonia
  "FIN", # Finland
  "FRA", # France
  "DEU", # Germany
  "GRC", # Greece
  "IRL", # Ireland
  "ITA", # Italy
  "LVA", # Latvia
  "LTU", # Lithuania
  "LUX", # Luxembourg
  "MLT", # Malta
  "NLD", # Netherlands
  "PRT", # Portugal
  "SVK", # Slovakia
  "SVN", # Slovenia
  "ESP"  # Spain
)
# right format
cons_ext2 <- cons_ext2_raw %>%
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
cons_ext2 <- cons_ext2 %>%
  filter(REF_AREA %in% reg_sample)

#### per capita growth rates: GDP and consumption
cons_ext2 <- cons_ext2 %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    consumption_log_diff = log(consumption_pc) - log(dplyr::lag(consumption_pc))
  ) %>%
  ungroup()

# common country-year sample: GDP, consumption and Population must all be available
cons_ext2_aggregate2 <- cons_ext2 %>%
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
cons_ext2 <- cons_ext2 %>%
  left_join(
    cons_ext2_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total, cons_log_diff_total),
    by = "TIME_PERIOD"
  )

# growth rate deviations
cons_ext2 <- cons_ext2 %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total,
    cons_dev = consumption_log_diff - cons_log_diff_total
  )

# keep regression period
cons_ext2 <- cons_ext2 %>%
  filter(TIME_PERIOD > 1986, TIME_PERIOD < 2018)

ehb_data <- ehb_data %>%
    filter(year > 1986, year < 2018)

# rename identifiers
cons_ext2 <- cons_ext2 %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )

ehb_data <- ehb_data %>%
  filter(iso %in% reg_sample)


# merge data
merged_df <- list(cons_ext2, ehb_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 2002)

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
  "../data/reg_cons_df_ext_4a.csv",
  row.names = FALSE
)

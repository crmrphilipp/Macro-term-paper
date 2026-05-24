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
cons_rep_raw <- read.csv("../data/data_cy_rep.csv")


# load home bias data
ehb_data <- read.csv("../data/ehb_reg_small.csv")
ehb_crude_data <- read.csv("../data/ehb_crude_reg_small.csv")


### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)
# right format
cons_rep <- cons_rep_raw %>%
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
cons_rep <- cons_rep %>%
  filter(REF_AREA %in% reg_sample)

#### per capita growth rates: GDP and consumption
cons_rep <- cons_rep %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    consumption_log_diff = log(consumption_pc) - log(dplyr::lag(consumption_pc))
  ) %>%
  ungroup()

# common country-year sample: GDP, consumption and Population must all be available
cons_rep_aggregate2 <- cons_rep %>%
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
cons_rep <- cons_rep %>%
  left_join(
    cons_rep_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total, cons_log_diff_total),
    by = "TIME_PERIOD"
  )


# growth rate deviations
cons_rep <- cons_rep %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total,
    cons_dev = consumption_log_diff - cons_log_diff_total
  )

# keep regression period
cons_rep <- cons_rep %>%
  filter(TIME_PERIOD > 1992, TIME_PERIOD < 2004)

# rename identifiers
cons_rep <- cons_rep %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )

# merge data
merged_df <- list(cons_rep, ehb_data, ehb_crude_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 1998)

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
  "../data/reg_cons_df_original_sample.csv",
  row.names = FALSE
)

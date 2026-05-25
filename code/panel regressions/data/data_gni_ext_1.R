
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
      "..", "..","..", "data"
    )
  )
)
# load data
gni_rep_raw <- read.csv("../data/data_iy_ext_1.csv")

merged_df <- gni_rep_raw

# load home bias data
ehb_data <- read.csv("../data/ehb_top60_restr_small.csv")



### Regression sample
reg_sample <- c(
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU",
  "GRC", "ITA", "JPN", "MEX", "NLD", "NZL", "NOR", "PRT",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)
# right format
gni_rep <- gni_rep_raw %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD, Population),
    names_from = measure,
    values_from = c(OBS_VALUE, OBS_VALUE_per_capita),
    names_glue = "{measure}_{.value}"
  ) %>%
  rename(
    GNI = GNI_OBS_VALUE,
    GDP = GDP_OBS_VALUE,
    GNI_pc = GNI_OBS_VALUE_per_capita,
    GDP_pc = GDP_OBS_VALUE_per_capita,
  )

### Creating deviation variables 
gni_rep <- gni_rep %>%
  filter(REF_AREA %in% reg_sample)

#### per capita growth rates: GDP and GNI
gni_rep <- gni_rep %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(
    gdp_log_diff = log(GDP_pc) - log(dplyr::lag(GDP_pc)),
    GNI_log_diff = log(GNI_pc) - log(dplyr::lag(GNI_pc))
  ) %>%
  ungroup()

# common country-year sample: GDP, GNI and Population must all be available
gni_rep_aggregate2 <- gni_rep %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_total = sum(
      GDP[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    gni_total = sum(
      GNI[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    pop_total = sum(
      Population[!is.na(GDP) & !is.na(GNI) & !is.na(Population)],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    gdp_total_pc = gdp_total / pop_total,
    gni_total_pc = gni_total / pop_total
  ) %>%
  mutate(
    gdp_log_diff_total =
      log(gdp_total_pc) - log(dplyr::lag(gdp_total_pc)),
    
    gni_log_diff_total =
      log(gni_total_pc) - log(dplyr::lag(gni_total_pc))
  )


# join the aggregate growth rates
gni_rep <- gni_rep %>%
  left_join(
    gni_rep_aggregate2 %>%
      select(TIME_PERIOD, gdp_log_diff_total, gni_log_diff_total),
    by = "TIME_PERIOD"
  )


# growth rate deviations
gni_rep <- gni_rep %>%
  mutate(
    gdp_dev = gdp_log_diff - gdp_log_diff_total,
    gni_dev = GNI_log_diff - gni_log_diff_total
  )

# keep regression period
gni_rep <- gni_rep %>%
  filter(TIME_PERIOD > 1986, TIME_PERIOD < 2018)

# rename identifiers
gni_rep <- gni_rep %>%
  rename(
    iso = REF_AREA,
    year = TIME_PERIOD
  )

# merge data
merged_df <- list(gni_rep, ehb_data) %>%
  reduce(left_join, by = c("iso", "year"))

merged_df <- merged_df %>%
  mutate(time = year - 2002)

# clean estimation sample
reg_gni_df <- merged_df %>%
  filter(
    !is.na(gni_dev),
    !is.na(gdp_dev),
    !is.na(EHB_dev),
    !is.na(time)
  ) %>%
  arrange(iso, year)


write.csv(
  reg_gni_df,
  "../data/reg_gni_df_ext_1.csv",
  row.names = FALSE
)





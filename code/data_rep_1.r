# ══════════════════════════════════════════════════════════════════════════════
#  
# DATA cleaning and preperation
#
# ══════════════════════════════════════════════════════════════════════════════

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)
library(tidyverse)
library(stargazer)
library(readr)
library(WDI)
library(readxl)
library(kableExtra)

# ═════ preparing countries═════════════════════════════════════════════════════

oecd_countries <- c("AUS","AUT","BEL","CAN","DNK","FIN","FRA","DEU","GRC","ISL",
                    "IRL","ITA","JPN","MEX","NLD","NZL","NOR","PRT","ESP","SWE",
                    "CHE","TUR","GBR","USA")

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 1: OECD / Annual GDP and components - expenditure approach
# Provides: PPP adjusted current US dollar values for relevant countries
# and relevant years.
# NOTE: Authors do compute the PPP adjustment themselves. We can/should do 
# this with OECD data as well. However, how much can they differ from 
# original OECD statistics? 
# ══════════════════════════════════════════════════════════════════════════════

oecd_url_gdp  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE1_EXPENDITURE,2.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+EST+CZE+DNK+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+LVA+KOR+JPN+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.S1+S1M+S13..B1GQ+P3....XDC+USD_EXC+USD_PPP.V..?startPeriod=1988&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
oecd_file_gdp <- "../data/oecd_data_gdp.csv"

dir.create("../data", showWarnings = FALSE)

if (!file.exists(oecd_file_gdp)) {
  download.file(oecd_url_gdp, destfile = oecd_file_gdp, mode = "wb")
  message("oecd_gdp dataset downloaded.")
}
 
# Load the main data sheet 
oecd_gdp_raw <- read_csv(oecd_file_gdp)

#investigate data structure
head(oecd_gdp_raw)

# Clean data to form a working dataset

oecd_gdp_raw <- oecd_gdp_raw %>%
  filter(TIME_PERIOD >= 1990)

oecd_gdp_raw <- oecd_gdp_raw %>%
  filter(REF_AREA %in% oecd_countries)

oecd_gdp_raw_wide <- oecd_gdp_raw %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD),
    names_from = Transaction,
    values_from = OBS_VALUE
  )

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 2 : OECD / Annual GDP & Net payments received from rest of the world
# Provides: Values to compute GNI
# ═══

oecd_url_gni  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,/A.AUS+AUT+BEL+CAN+CHL+CRI+COL+CZE+DNK+EST+FIN+DEU+FRA+GRC+HUN+ISL+IRL+ISR+ITA+KOR+JPN+LVA+LTU+LUX+MEX+NZL+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...B1GQ+B1GQXOTGL+IN1B....USD_PPP.V..?startPeriod=1988&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
oecd_file_gni <- "../data/oecd_data_gni.csv"

dir.create("../data", showWarnings = FALSE)

if (!file.exists(oecd_file_gni)) {
  download.file(oecd_url_gni, destfile = oecd_file_gni, mode = "wb")
  message("oecd_gni dataset downloaded.")
}
 
# Load the main data sheet 
oecd_gni_raw <- read_csv(oecd_file_gni)

#investigate data structure
head(oecd_gni_raw)

# Clean data to form a working dataset

oecd_gni_raw <- oecd_gni_raw %>%
  filter(TIME_PERIOD >= 1990)

oecd_gni_raw <- oecd_gni_raw %>%
  filter(REF_AREA %in% oecd_countries)

oecd_gni_raw_wide <- oecd_gni_raw %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD),
    names_from = Transaction,
    values_from = OBS_VALUE
  )

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 2 : OECD / Inflation adjustment
# 
# Provides: Values to compute price adjusted values
# NOTE: 
# ═════════════════════════════════════════════════════════════════════════════════


oecd_url_cpi  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/AUS+AUT+BEL+CHL+CAN+COL+CRI+CZE+DNK+FIN+EST+FRA+DEU+GRC+HUN+ISL+IRL+ISR+JPN+ITA+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+TUR+CHE+GBR+USA.A.N.CPI.PA._T..GY?startPeriod=1988&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
oecd_file_cpi <- "../data/oecd_data_cpi.csv"

dir.create("../data", showWarnings = FALSE)

if (!file.exists(oecd_file_cpi)) {
  download.file(oecd_url_cpi, destfile = oecd_file_cpi, mode = "wb")
  message("oecd_cpi dataset downloaded.")
}
 
# Load the main data sheet 
oecd_cpi_raw <- read_csv(oecd_file_cpi)

#investigate data structure
head(oecd_cpi_raw)

# Clean data to form a working dataset

oecd_cpi_raw <- oecd_cpi_raw %>%
  filter(TIME_PERIOD >= 1990)

oecd_cpi_raw <- oecd_cpi_raw %>%
  filter(REF_AREA %in% oecd_countries)

oecd_cpi_raw_wide <- oecd_cpi_raw %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD),
    names_from = MEASURE,
    values_from = OBS_VALUE
  )

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 2 : OECD / Population values 
# 
# Provides: Provides population values to compute per capita values
# NOTE: 
# ═════════════════════════════════════════════════════════════════════════════════


oecd_url_pop  <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE3_POP_EMPNC,2.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...POP.......?startPeriod=1988&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
oecd_file_pop <- "../data/oecd_data_pop.csv"

dir.create("../data", showWarnings = FALSE)

if (!file.exists(oecd_file_pop)) {
  download.file(oecd_url_pop, destfile = oecd_file_pop, mode = "wb")
  message("oecd_pop dataset downloaded.")
}
 
# Load the main data sheet 
oecd_pop_raw <- read_csv(oecd_file_pop)

#investigate data structure
head(oecd_pop_raw)

# Clean data to form a working dataset

oecd_pop_raw <- oecd_pop_raw %>%
  filter(TIME_PERIOD >= 1990)

oecd_pop_raw <- oecd_pop_raw %>%
  filter(REF_AREA %in% oecd_countries)

oecd_pop_raw_wide <- oecd_pop_raw %>%
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD),
    names_from = Transaction,
    values_from = OBS_VALUE
  )

# ══════════════════════════════════════════════════════════════════════════════
# Merge all datasets
# ═════════════════════════════════════════════════════════════════════════════════

oecd_df <- list(
  oecd_gdp_raw_wide,
  oecd_pop_raw_wide,
  oecd_gni_raw_wide,
  oecd_cpi_raw_wide
) %>%
  reduce(left_join, by = c("REF_AREA", "TIME_PERIOD"))

oecd_df <- oecd_df %>%
  mutate(
    gni = `Gross domestic income` + `Net primary income from the rest of the world`
  )

# Build US deflator anchored to 2020 = 1
us_deflator <- oecd_cpi_raw_wide %>%
  filter(REF_AREA == "USA") %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    multiplier  = 1 + CPI / 100,
    price_level = cumprod(multiplier),
    deflator    = price_level / price_level[TIME_PERIOD == 2020]
  ) %>%
  select(TIME_PERIOD, deflator)

# relabel
oecd_df <- oecd_df%>%
  rename(gdp_nominal = `Gross domestic product.y`)


oecd_df <- oecd_df%>%
  rename(pop = `Total population`)

# Join to main df and deflate
oecd_df <- oecd_df %>%
  left_join(us_deflator, by = "TIME_PERIOD") %>%
  mutate(gdp_real_2020usd = gdp_nominal / deflator)

# per capita  real gdp
oecd_df <- oecd_df%>%
  mutate(gdp_real_pc = gdp_real_2020usd/pop)

# per capita growth rates
oecd_df <- oecd_df %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(gdp_real_pc_growth = (gdp_real_pc / lag(gdp_real_pc) - 1) * 100) %>%
  ungroup()

# aggregate p.c. growht rates
oecd_aggregate <- oecd_df %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gdp_real_total = sum(gdp_real_2020usd, na.rm = TRUE),
    pop_total      = sum(pop, na.rm = TRUE)
  ) %>%
  mutate(
    gdp_real_pc_oecd        = gdp_real_total / pop_total,
    gdp_real_pc_oecd_growth = (gdp_real_pc_oecd / lag(gdp_real_pc_oecd) - 1) * 100
  )

# join the aggregate growth rates
oecd_df <- oecd_df %>%
  left_join(oecd_aggregate %>% select(TIME_PERIOD, gdp_real_pc_oecd_growth), 
            by = "TIME_PERIOD")

# growth rate deviation
oecd_df <- oecd_df %>%
  mutate(growth_dev = gdp_real_pc_growth - gdp_real_pc_oecd_growth)


# 1. Real + per capita + growth rates
oecd_df <- oecd_df %>%
  mutate(gni_real_2020usd = gni / deflator,
         gni_real_pc      = gni_real_2020usd / pop) %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  group_by(REF_AREA) %>%
  mutate(gni_real_pc_growth = (gni_real_pc / lag(gni_real_pc) - 1) * 100) %>%
  ungroup()

# 2. Aggregate OECD GNI series
oecd_aggregate_gni <- oecd_df %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    gni_real_total = sum(gni_real_2020usd, na.rm = TRUE),
    pop_total      = sum(pop, na.rm = TRUE)
  ) %>%
  mutate(
    gni_real_pc_oecd        = gni_real_total / pop_total,
    gni_real_pc_oecd_growth = (gni_real_pc_oecd / lag(gni_real_pc_oecd) - 1) * 100
  )

# 3. Join back and compute deviation
oecd_df <- oecd_df %>%
  left_join(oecd_aggregate_gni %>% select(TIME_PERIOD, gni_real_pc_oecd_growth),
            by = "TIME_PERIOD") %>%
  mutate(gni_dev = gni_real_pc_growth - gni_real_pc_oecd_growth)

oecd_df <- oecd_df %>%
  rename(gdp_dev = growth_dev)

# regression data export
oecd_small_reg <- oecd_df%>%
  select(REF_AREA, TIME_PERIOD, gdp_dev, gni_dev)

oecd_small_reg <- oecd_small_reg %>%
  rename(iso = REF_AREA, year =TIME_PERIOD)

write_csv(oecd_small_reg, "../data/oecd_small_reg.csv")



# ══════════════════════════════════════════════════════════════════════════════
# 
# EUROZONE EXTENSION: CHANNELS OF RISK SHARING 
# Getting GDP, GNI, NNI, DNI and Consumption data from the OECD (Data Explorer)
# The following code extracts:
    # all of the above variables in current prices (nominal) as well as CPI data to deflate
    # and merges to an export-ready dataset.
#
# ══════════════════════════════════════════════════════════════════════════════

# Note: Data for Cyprus and Malta are not available. Hence, we consider the EA20 without these two countries.

############ 0. Preliminaries ############
# Clear workspace, set working directory
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plm)

############ Annual GDP, GNI, NNI, DNI and Consumption data (nominal, current prices) ############
# Define relevant SDMX code
sdmx_channels_raw <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,/A.HRV+AUT+BEL+EST+FIN+FRA+GRC+IRL+LVA+LTU+LUX+NLD+PRT+SVK+SVN+ESP+ITA+DEU...B6N+B1GQ+P3+B5N+B5G....XDC.V..?startPeriod=1970&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
channels_raw <- "../data/channels_raw.csv"

download.file(sdmx_channels_raw, destfile = channels_raw, mode = "wb")
message("channels_raw dataset downloaded.")

channels_function <- function(channels_raw, time_frame){
# Load the main data sheet
channels_raw_data <- read_csv(channels_raw)

# Select relevant variables/ observations
channels_raw_data <- channels_raw_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Gross domestic product" ~ "GDP",
    Transaction == "Balance of primary incomes, gross / National income, gross" ~ "GNI",
    Transaction == "Balance of primary incomes, net / National income, net" ~ "NNI",
    Transaction == "Disposable income, net" ~ "NDI",
    Transaction == "Final consumption expenditure" ~ "consumption",
  )) %>%
  # 2. Rename the column and multiply by 1million to get correct values
  rename(measure = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Select and arrange necessary variables
  select(`Reference area`, REF_AREA, TIME_PERIOD, measure, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% time_frame) %>%
  arrange(REF_AREA, measure, TIME_PERIOD)

# Set up regression-ready data
channels_data <- channels_raw_data %>%
  select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE) %>%
  pivot_wider(names_from = measure, values_from = OBS_VALUE) %>%
  arrange(REF_AREA, TIME_PERIOD)

channels_diff <- channels_data %>%
  group_by(REF_AREA) %>%
  mutate(
    # Compute log differences for each aggregate
    dlog_GDP = log(GDP) - dplyr::lag(log(GDP)),
    dlog_GNI = log(GNI) - dplyr::lag(log(GNI)),
    dlog_NNI = log(NNI) - dplyr::lag(log(NNI)),
    dlog_NDI = log(NDI) - dplyr::lag(log(NDI)),
    dlog_C   = log(consumption) - dplyr::lag(log(consumption))
  ) %>%
  ungroup() %>%
  mutate(
    # Construct the dependent variables for the 5 channels in system (14)
    y_f   = dlog_GDP - dlog_GNI,  # Factor income flows
    y_d   = dlog_GNI - dlog_NNI,  ## Capital depreciation
    y_tau = dlog_NNI - dlog_NDI,  # Transfers (Note to Felix: Recall this includes EU/EA + international/ development transfers)
    y_s   = dlog_NDI - dlog_C,    # Saving
    y_u   = dlog_C                # Residual
  ) %>%
  # Drop NAs resulting from the lag differencing
  drop_na()

############ Perform panel data estimations (within transformation, time fixed effects) ############
pdata <- pdata.frame(channels_diff, index = c("REF_AREA", "TIME_PERIOD"))

# Factor income flows
eq_f   <- plm(y_f   ~ dlog_GDP, data = pdata, effect = "time", model = "within")
# Capital depreciation
eq_d   <- plm(y_d   ~ dlog_GDP, data = pdata, effect = "time", model = "within")
# Transfers
eq_tau <- plm(y_tau ~ dlog_GDP, data = pdata, effect = "time", model = "within")
# Saving
eq_s   <- plm(y_s   ~ dlog_GDP, data = pdata, effect = "time", model = "within")
# Residual
eq_u   <- plm(y_u   ~ dlog_GDP, data = pdata, effect = "time", model = "within")

results_table <- data.frame(
  Coefficient = c(
    coef(eq_f)["dlog_GDP"],
    coef(eq_d)["dlog_GDP"],
    coef(eq_tau)["dlog_GDP"],
    coef(eq_s)["dlog_GDP"],
    coef(eq_u)["dlog_GDP"]
  ),
  p_value = c(
    round(summary(eq_f)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
    round(summary(eq_d)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
    round(summary(eq_tau)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
    round(summary(eq_s)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
    round(summary(eq_u)$coefficients["dlog_GDP", "Pr(>|t|)"], 4))
)

rownames(results_table) <- c(
  "Factor Income", 
  "Capital Depreciation", 
  "Transfers", 
  "Saving",
  "Unsmoothed"
)

return(results_table)
}

### Time Frame: 1971 - 2025
table_71_25 <- channels_function(channels_raw = channels_raw, 
                                      time_frame = 1971:2025)

### Time Frame: 1971 - 1990
table_71_90 <- channels_function(channels_raw = channels_raw, 
                                      time_frame = 1971:1990)

### Time Frame: 1991 - 2005
table_91_05 <- channels_function(channels_raw = channels_raw, 
                                      time_frame = 1991:2005)

### Time Frame: 2006 - 2025
table_06_25 <- channels_function(channels_raw = channels_raw, 
                                      time_frame = 2006:2025)

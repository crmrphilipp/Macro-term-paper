# ══════════════════════════════════════════════════════════════════════════════
#  
# Recreating the risk sharing plot from the reference paper (p. 594/595)
#
# ══════════════════════════════════════════════════════════════════════════════

# Clear workspace, set working directory
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import data on GDP, consumption, GNI
data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

# GNI risk sharing regression and plot
...

# Consumption risk sharing regression and plot
for year in 1993:2024{
    # Create regression dataset for the given year
    reg_data <- data[data$measure %in% ("GDP","consumption") & data$TIME_PERIOD %in% c(year-1, year),]

    # Get aggregate GDP and consumption values for the current and previous year
    agg_gdp_current <- sum(reg_data[reg_data$measure=="GDP" & reg_data$TIME_PERIOD==year,]$OBS_VALUE_per_capita)
    agg_con_current <- sum(reg_data[reg_data$measure=="consumption" & reg_data$TIME_PERIOD==year,]$OBS_VALUE_per_capita)
    agg_gdp_previous <- sum(reg_data[reg_data$measure=="GDP" & reg_data$TIME_PERIOD==year-1,]$OBS_VALUE_per_capita)
    agg_con_previous <- sum(reg_data[reg_data$measure=="consumption" & reg_data$TIME_PERIOD==year-1,]$OBS_VALUE_per_capita)

    ld_agg_gdp <- log(agg_gdp_current) - log(agg_gdp_previous)
    ld_agg_con <- log(agg_con_current) - log(agg_con_previous)

    
}
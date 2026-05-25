# ══════════════════════════════════════════════════════════════════════════════
# 
# EUROZONE EXTENSION: CHANNELS OF RISK SHARING (Eurozone)
# Getting GDP, GNI, NNI, DNI and Consumption data from the OECD (Data Explorer)
# The following code extracts:
    # all of the above variables in current prices (nominal) as well as CPI data to deflate
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
library(ggplot2)
library(readr)
library(plm)

# ══════════════════════════════════════════════════════════════════════════════ #
############ 1. Eurozone Channels of Risk Sharing ############

############ Get annual i) GDP, GNI, GDI, total consumption ii) household consumption expenditure and iii) CPI and Population (for normalization to real and per capita values)
# Note : No data available for Cyprus and Malta, hence consider only EA19. Then, unbalanced panel.
############ Define relevant SDM codes and load data ############
# GDP, GNI, GDI, total consumption
sdmx_channels_raw <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,/A.AUT+BEL+FIN+EST+FRA+DEU+GRC+IRL+ITA+LVA+LTU+LUX+NLD+SVK+PRT+SVN+ESP+HRV+BGR...B1GQ+B5G+P3+B6G....XDC.V..?startPeriod=1969&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
#sdmx_channels_raw <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2,/A.AUT+BEL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ITA+LVA+LTU+LUX+NLD+POL+PRT+SVK+SVN+ESP+SWE+HRV+ROU+BGR...B1GQ+B5G+B6G+P3....XDC.V..?startPeriod=1969&endPeriod=2025&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
channels_raw <- "../data/channels_raw.csv"
download.file(sdmx_channels_raw, destfile = channels_raw, mode = "wb")
message("channels_raw dataset downloaded.")

# Household Consumption
sdmx_cons_hh <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE5_T501,1.0/A.HRV+BEL+EST+FIN+FRA+DEU+GRC+IRL+ITA+LVA+LTU+LUX+NLD+SVK+PRT+SVN+ESP+BGR+AUT.S14....._T.XDC.V..?startPeriod=1969&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
#sdmx_cons_hh <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE5_T501,1.0/A.BEL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ITA+LVA+LTU+LUX+NLD+POL+PRT+SVK+SVN+ESP+SWE+ROU+HRV+BGR+AUT.S14....._T.XDC.V..?startPeriod=1995&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
cons_hh <- "../data/cons_hh.csv"
download.file(sdmx_cons_hh, destfile = cons_hh, mode = "wb")
message("cons_hh dataset downloaded.")

cons_hh_data <- read_csv(cons_hh)
cons_hh_data <- cons_hh_data %>%
    rename(consumption_hh = OBS_VALUE) %>%
    mutate(consumption_hh = consumption_hh * 1000000) %>%
    select(REF_AREA, TIME_PERIOD, consumption_hh) %>%
    arrange(REF_AREA, TIME_PERIOD)

# CPI
sdmx_cpi <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/.A.N.CPI.._T.N._Z?startPeriod=1969&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
cpi <- "../data/cpi.csv"
download.file(sdmx_cpi, destfile = cpi, mode = "wb")
message("cpi dataset downloaded.")

cpi_data <- read_csv(cpi)
cpi_data <- cpi_data %>%
  rename(CPI = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, CPI) %>%
  arrange(REF_AREA, TIME_PERIOD)

# Population
sdmx_pop <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+G20+EU27+OECD+ARG+BRA+BGR+CHN+HRV+CYP+IND+IDN+MLT+ROU+RUS+SAU+SGP+ZAF+W+AUS.POP.PS._T._T.?startPeriod=1969&endPeriod=2024&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
pop <- "../data/pop.csv"
download.file(sdmx_pop, destfile = pop, mode = "wb")
message("population dataset downloaded.")

pop_data_temp <- read_csv(pop)
pop_data_temp <- pop_data_temp %>%
  rename(Population = OBS_VALUE) %>%
  select(REF_AREA, TIME_PERIOD, Population) %>%
  arrange(REF_AREA, TIME_PERIOD)

### Load data
channels_raw_data <- read_csv(channels_raw)

############ Clean and merge data sets to make them regression-ready ############
# Construct one joint dataset with GDP, GNI, GDI, total consumption and household consumption
joint_dataset <- function(channels_raw_data, cons_hh_data, cpi_data, pop_data_temp, time_frame){
  channels_raw_data <- channels_raw_data %>%
  # 1. Overwrite the specific values in the Transaction column
  mutate(Transaction = case_when(
    Transaction == "Gross domestic product" ~ "GDP",
    Transaction == "Balance of primary incomes, gross / National income, gross" ~ "GNI",
    Transaction == "Disposable income, gross" ~ "GDI",
    Transaction == "Final consumption expenditure" ~ "consumption",
  )) %>%
  # 2. Rename the column and multiply by 1million to get correct values
  rename(measure = Transaction) %>%
  mutate(OBS_VALUE = OBS_VALUE * 1000000) %>%
  # 3. Select and arrange necessary variables
  select(`Reference area`, REF_AREA, TIME_PERIOD, measure, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% time_frame)

# 4. Add consumption data
country_year_grid <- channels_raw_data %>%
  distinct(`Reference area`, REF_AREA, TIME_PERIOD)

cons_hh_prepared <- country_year_grid %>%
  left_join(cons_hh_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
  rename(OBS_VALUE = consumption_hh) %>%
  mutate(measure = "consumption_hh")

per_capita_data <- bind_rows(channels_raw_data, cons_hh_prepared) %>%
  arrange(REF_AREA, TIME_PERIOD, measure) %>%
  # 5. Add CPI and population data, keep only real per capita values
  left_join(cpi_data, by = c("REF_AREA", "TIME_PERIOD")) %>%
  mutate(OBS_VALUE = OBS_VALUE/(CPI/100)) %>%
  left_join(pop_data_temp, by = c("REF_AREA", "TIME_PERIOD")) %>%
  mutate(OBS_VALUE = OBS_VALUE / Population) %>%
  arrange(REF_AREA, measure, TIME_PERIOD)

# Convert to 2015 USD (not PPP!, see methodology of referenced paper) using WDI exchange rates (OECD had limited exchange rate data)
exchange_rates <- read_csv("../data/wdi_exchange_rates.csv")
exchange_rates <- exchange_rates %>%
  rename(`Reference area` = `Country Name`, "REF_AREA" = `Country Code`, "erate" = `2015 [YR2015]`) %>%
  select(`Reference area`, "REF_AREA", "erate") %>%
  drop_na()

# For all EA countries, manually set erate to 0.9013
euro_bound_iso3 <- c(
  "AUT", "BEL", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC", "IRL", 
  "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP", "BGR"
)

exchange_rates <- exchange_rates %>%
  mutate(erate = ifelse(REF_AREA %in% euro_bound_iso3, 0.9013, erate))

per_capita_data <- per_capita_data %>%
  left_join(exchange_rates %>% select(REF_AREA, erate), by = "REF_AREA") %>%
  mutate(
    OBS_VALUE = OBS_VALUE / erate
  ) %>%
  select(-erate) %>%
  arrange(REF_AREA, measure, TIME_PERIOD)

### Set up regression-ready data
channels_data <- per_capita_data %>%
  select(REF_AREA, TIME_PERIOD, measure, OBS_VALUE) %>%
  pivot_wider(names_from = measure, values_from = OBS_VALUE) %>%
  arrange(REF_AREA, TIME_PERIOD)

channels_diff <- channels_data %>%
  group_by(REF_AREA) %>%
  mutate(     
      dlog_GDP = log(GDP) - dplyr::lag(log(GDP)),
      dlog_GNI = log(GNI) - dplyr::lag(log(GNI)),
      dlog_GDI = log(GDI) - dplyr::lag(log(GDI)),
      dlog_C_hh = log(consumption_hh) - dplyr::lag(log(consumption_hh)),
      dlog_C   = log(consumption) - dplyr::lag(log(consumption))
    ) %>%
    ungroup() %>%
  mutate(
  # Construct the dependent variables
    y_f   = dlog_GDP - dlog_GNI,        # Factor income flows
    y_tau = dlog_GNI - dlog_GDI,        # Transfers (Note to Felix: Recall this includes EU/EA + international/ development transfers)
    y_s   = dlog_GDI - dlog_C,          # Saving
    y_u   = dlog_C,                     # Residual
    y_s_hh   = dlog_GDI - dlog_C_hh,    # Saving (household consumption)
    y_u_hh   = dlog_C_hh                # Residual (household consumption)
  )
}

############ Perform panel data estimations (within transformation, time fixed effects, Pooled OLS (no GLS weighting)) ############
calculate_risk_sharing <- function(data, area, var_s = "y_s", var_u = "y_u") {
  # Drop RELEVANT NAs here
  vars_to_check <- c("dlog_GDP", "y_f", "y_tau", var_s, var_u)
  data <- data %>%
    drop_na(any_of(vars_to_check))

  # Create Panel Data Frame
  pdata <- pdata.frame(data, index = c("REF_AREA", "TIME_PERIOD"))
  
  # Factor income and transfers
  eq_f   <- plm(y_f ~ dlog_GDP, data = pdata, effect = "time", model = "within")
  eq_tau <- plm(y_tau ~ dlog_GDP, data = pdata, effect = "time", model = "within")
  
  # Saving and Unsmoothed (depending on whether to use C or C+G)
  form_s <- as.formula(paste(var_s, "~ dlog_GDP"))
  eq_s   <- plm(form_s, data = pdata, effect = "time", model = "within")
  
  form_u <- as.formula(paste(var_u, "~ dlog_GDP"))
  eq_u   <- plm(form_u, data = pdata, effect = "time", model = "within")
  
  # Constructing results table
  results_table <- data.frame(
    Channel = c("Capital Market", "Transfers", "Saving", "Unsmoothed"),
    Coefficient = c(
      coef(eq_f)["dlog_GDP"],
      coef(eq_tau)["dlog_GDP"],
      coef(eq_s)["dlog_GDP"],
      coef(eq_u)["dlog_GDP"]
    ),
    p_value = c(
      round(summary(eq_f)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
      round(summary(eq_tau)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
      round(summary(eq_s)$coefficients["dlog_GDP", "Pr(>|t|)"], 4),
      round(summary(eq_u)$coefficients["dlog_GDP", "Pr(>|t|)"], 4)
    )
  )
  
  # Set rownames and factor levels to maintain specific ordering
  results_table$Channel <- factor(results_table$Channel, 
                                  levels = c("Capital Market", "Transfers", "Saving", "Unsmoothed"))
  rownames(results_table) <- results_table$Channel

  # Define significance stars function and integrate to Risk Sharing Channels
  get_significance_stars <- function(p) {
    if (p < 0.01)  return("***")
    if (p < 0.05)  return("**")
    if (p < 0.1)   return("*")
    return(" (n.s.)") # not significant
  }
  
  # Create plot-specific dataframe with combined Legend labels
  plot_data <- results_table %>%
    mutate(
      stars = sapply(p_value, get_significance_stars),
      LegendLabel = paste(Channel, stars), # Will be sth like "Capital Markets Channel ***"
      XAxis = "Total Risk Sharing"
    )
   
  # AI suggested color mapping
  my_palette <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2")
  names(my_palette) <- levels(results_table$Channel)
  final_colors <- my_palette[as.character(plot_data$Channel)]
  names(final_colors) <- plot_data$LegendLabel
  legend_order <- plot_data$LegendLabel[order(match(plot_data$Channel, levels(results_table$Channel)))]
  plot_data$LegendLabel <- factor(plot_data$LegendLabel, levels = legend_order)

  # Generate plot
  plot_title <- paste("Risk Sharing Channels in the", area)
  signif_caption <- "Significance codes:  '***' 0.01 '**' 0.05 '*' 0.1"
  
  p <- ggplot(plot_data, aes(x = XAxis, y = Coefficient, fill = LegendLabel)) +
    # Create the stacked bar
    geom_bar(stat = "identity", width = 0.5, color = "white", linewidth = 0.3, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = scales::percent(Coefficient, accuracy = 0.1)), 
              position = position_stack(vjust = 0.5, reverse = TRUE), 
              size = 4, fontface = "bold", color = "white") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    annotate("text", x = 1.35, y = 1.02, label = "Full Smoothing (100%)", 
             color = "gray40", hjust = 0, size = 3.5) +
    # Styling
    scale_fill_manual(values = final_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       breaks = seq(0, 1.25, by = 0.25),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = plot_title,
      caption = signif_caption,
      x = NULL,
      y = "Fraction of Risk Absorbed",
      fill = "Channel & Significance"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 20)),
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15), color = "gray30"),
      axis.text.x = element_text(face = "bold", size = 13),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "gray98", color = "gray90"),
      legend.margin = margin(6, 6, 6, 6)
    )
  
  # Return table and plot
  clean_table <- results_table
  clean_table$Channel <- as.character(clean_table$Channel)
  rownames(clean_table) <- clean_table$Channel
  clean_table$p_value <- round(clean_table$p_value, 4)
  return(list(Table = clean_table, Plot = p))
}

############ Perform analysis for different time frames ############
### Time Frame 1: 1970 - 2019
time_frame <- 1970:2019
joint_dataset_70_19 <- joint_dataset(channels_raw_data = channels_raw_data, cpi_data = cpi_data, cons_hh_data = cons_hh_data,
                                     pop_data_temp = pop_data_temp, time_frame = time_frame)

# Euro area using private and government consumption
ea_70_19 <- calculate_risk_sharing(data = joint_dataset_70_19, area = "Eurozone")
ea_70_19[["Plot"]]

### Time Frame 2: 1970 - 1991
time_frame <- 1970:1991
joint_dataset_70_91 <- joint_dataset(channels_raw_data = channels_raw_data, cpi_data = cpi_data, cons_hh_data = cons_hh_data,
                                     pop_data_temp = pop_data_temp, time_frame = time_frame)

# Euro area using private and government consumption
ea_70_91 <- calculate_risk_sharing(data = joint_dataset_70_91, area = "Eurozone")
ea_70_91[["Plot"]]

### Time Frame 3: 1991 - 2007
time_frame <- 1991:2007
joint_dataset_91_07 <- joint_dataset(channels_raw_data = channels_raw_data, cpi_data = cpi_data, cons_hh_data = cons_hh_data,
                                     pop_data_temp = pop_data_temp, time_frame = time_frame)

# Euro area using private and government consumption
ea_91_07 <- calculate_risk_sharing(data = joint_dataset_91_07, area = "Eurozone")
ea_91_07[["Plot"]]

### Time Frame 4: 2007 - 2019
time_frame <- 2010:2019
joint_dataset_07_19 <- joint_dataset(channels_raw_data = channels_raw_data, cpi_data = cpi_data, cons_hh_data = cons_hh_data,
                                     pop_data_temp = pop_data_temp, time_frame = time_frame)

# Euro area using private and government consumption
ea_07_19 <- calculate_risk_sharing(data = joint_dataset_07_19, area = "Eurozone")
ea_07_19[["Plot"]]

##########
# # Euro area using private consumption only
joint_dataset_09_19 <- joint_dataset(channels_raw_data = channels_raw_data, cpi_data = cpi_data, cons_hh_data = cons_hh_data,
                                     pop_data_temp = pop_data_temp, time_frame = 2009:2019)
ea_hh <- calculate_risk_sharing(data = joint_dataset_09_19, area = "Eurozone", var_s = "y_s_hh", var_u = "y_u_hh")
ea_hh[["Plot"]]

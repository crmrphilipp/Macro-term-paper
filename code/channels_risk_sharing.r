# ------------------------------------------------------------------------------------------------------- #
################## Using WDI data to apply the methodolgy of Gervais/ Hosseini (2026) #####################
#################################### Risk Sharing Channels in the Eurozone ################################
# ------------------------------------------------------------------------------------------------------- #

############ Preliminaries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plm)
library(zoo)

# 1. Define the indicator codes from your list
indicator_list <- c(
  gdp_real              = "NY.GDP.MKTP.KD",
  gni_real              = "NY.GNP.MKTP.KD",
  gni_nominal           = "NY.GNP.MKTP.CD",
  household_consumption = "NE.CON.PRVT.KD",
  net_secondary_income  = "BN.TRF.CURR.CD",
  population            = "SP.POP.TOTL"
)

# 2. Define EU countries (ISO-2 codes) excluding Estonia
eu_no_estonia <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", 
  "FI", "FR", "DE", "GR", "HU", "IE", "IT", 
  "LV", "LT", "LU", "MT", "NL", "PL", "PT", 
  "RO", "SK", "SI", "ES", "SE"
)

eurozone_21 <- c(
  "AT", "BE", "BG", "HR", "EE", "FI","CY" ,
  "FR", "DE", "GR", "IE", "IT", "LV", "LT", 
  "LU", "NL", "PT", "SK", "SI", "ES", "MT"
)

# 3. Download the data from the World Bank API
wdi_data <- WDI(
  indicator = indicator_list,
  country   = eurozone_21,
  start     = 1995,
  end       = 2023,
  extra     = FALSE
)

wdi_data_rep <- WDI(
  indicator = indicator_list,
  country   = eu_no_estonia,
  start     = 1995,
  end       = 2023,
  extra     = FALSE
)

############ Data cleaning
clean_data <- function(wdi_data, time_frame){
    wdi_data_clean <- wdi_data %>%
        rename(`Reference area` = "country", "REF_AREA" = "iso3c", "TIME_PERIOD" = "year") %>%
        mutate(gdi_real = gni_real*(1+(net_secondary_income/gni_nominal)),
            GDP = gdp_real / population, 
            GNI = gni_real / population, 
            GDI = gdi_real / population, 
            consumption_hh = household_consumption / population, 
        ) %>%
        select(REF_AREA, TIME_PERIOD, GDP, GNI, GDI, consumption_hh) %>%
        filter(TIME_PERIOD %in% time_frame) %>%
        arrange(REF_AREA, TIME_PERIOD)

    wdi_diff <- wdi_data_clean %>%
    group_by(REF_AREA) %>%
    mutate(     
        dlog_GDP = log(GDP) - dplyr::lag(log(GDP)),
        dlog_GNI = log(GNI) - dplyr::lag(log(GNI)),
        dlog_GDI = log(GDI) - dplyr::lag(log(GDI)),
        dlog_C_hh = log(consumption_hh) - dplyr::lag(log(consumption_hh)),
        ) %>%
        ungroup() %>%
    mutate(
    # Construct the dependent variables
        y_f   = dlog_GDP - dlog_GNI,        # Factor income flows
        y_tau = dlog_GNI - dlog_GDI,        # Transfers (Note to Felix: Recall this includes EU/EA + international/ development transfers)
        y_s_hh   = dlog_GDI - dlog_C_hh,    # Saving (household consumption)
        y_u_hh   = dlog_C_hh                # Residual (household consumption)
    )
}

############ Run risk sharing regressions in Panel Data Setting
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

### Replication: 2010-2019 for EU27
time_frame <- 2009:2019
data <- clean_data(wdi_data = wdi_data_rep, time_frame = time_frame)
eu_10_19 <- calculate_risk_sharing(data = data, area = "EU27", var_s = "y_s_hh", var_u = "y_u_hh")
eu_10_19[["Plot"]] # Sanity check for Table 7

### Euro Area Analysis
### Time Frame 1: 1995 - 2023
time_frame <- 1994:2023
data <- clean_data(wdi_data = wdi_data, time_frame = time_frame)
ea_95_23 <- calculate_risk_sharing(data = data, area = "Eurozone", var_s = "y_s_hh", var_u = "y_u_hh")
ggsave("../output/ea_95_23.pdf", plot = ea_95_23[["Plot"]], width = 8, height = 6)

### Time Frame 2: 1995 - 2007
time_frame <- 1994:2007
data <- clean_data(wdi_data = wdi_data, time_frame = time_frame)
ea_95_07 <- calculate_risk_sharing(data = data, area = "Eurozone", var_s = "y_s_hh", var_u = "y_u_hh")
ggsave("../output/ea_95_07.pdf", plot = ea_95_07[["Plot"]], width = 8, height = 6)

### Time Frame 3: 2010 - 2019
time_frame <- 2009:2019
data <- clean_data(wdi_data = wdi_data, time_frame = time_frame)
ea_10_19 <- calculate_risk_sharing(data = data, area = "Eurozone", var_s = "y_s_hh", var_u = "y_u_hh")
ggsave("../output/ea_10_19.pdf", plot = ea_10_19[["Plot"]], width = 8, height = 6)

### Time Frame 4: 2010 - 2023
time_frame <- 2009:2023
data <- clean_data(wdi_data = wdi_data, time_frame = time_frame)
ea_10_23 <- calculate_risk_sharing(data = data, area = "Eurozone", var_s = "y_s_hh", var_u = "y_u_hh")
ggsave("../output/ea_10_23.pdf", plot = ea_10_23[["Plot"]], width = 8, height = 6)

############ Run risk sharing regressions in Cross Section regressions
############ 4. Cross-Sectional Risk Sharing (Over Time)
time_frame <- 1995:2023
data <- clean_data(wdi_data = wdi_data, time_frame = time_frame)
# 1. Filter out missing values and ensure TIME_PERIOD is numeric
vars_to_check <- c("dlog_GDP", "y_f", "y_tau", "y_s_hh", "y_u_hh")

clean_df <- data %>%
  drop_na(any_of(vars_to_check)) %>%
  mutate(TIME_PERIOD = as.numeric(as.character(TIME_PERIOD)))

# 2. Run cross-sectional regressions year-by-year
yearly_results <- clean_df %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    # Check if we have enough observations (e.g., at least 3 countries) for a regression
    `Capital Market` = coef(lm(y_f ~ dlog_GDP))["dlog_GDP"],
    `Transfers`      = coef(lm(y_tau ~ dlog_GDP))["dlog_GDP"],
    `Saving`         = coef(lm(y_s_hh ~ dlog_GDP))["dlog_GDP"],
    `Unsmoothed`     = coef(lm(y_u_hh ~ dlog_GDP))["dlog_GDP"],
    .groups = "drop"
  ) %>%
  drop_na() %>%
  arrange(TIME_PERIOD)

# 3. Pivot the data to long format and apply the ksmooth kernel individually to each channel
plot_data <- yearly_results %>%
  pivot_longer(
    cols = c(`Capital Market`, `Transfers`, `Saving`, `Unsmoothed`),
    names_to = "Channel",
    values_to = "Coefficient"
  ) %>%
  # Group by channel so the smoother only looks at one channel's time series at a time
  group_by(Channel) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    # 10 year rolling averages
    Smoothed_Coefficient = zoo::rollapply(
      data = Coefficient, 
      width = 10, 
      FUN = mean, 
      na.rm = TRUE, 
      partial = TRUE, 
      align = "center"
    )
  ) %>%
  ungroup() %>%
  mutate(Channel = factor(Channel, levels = c("Capital Market", "Transfers", "Saving", "Unsmoothed")))

# 4. Generate the Plot
my_palette <- c("Capital Market" = "#4E79A7", 
                "Transfers"      = "#F28E2B", 
                "Saving"         = "#E15759", 
                "Unsmoothed"     = "#76B7B2")

p_cross_sec <- ggplot(plot_data, aes(x = TIME_PERIOD, color = Channel, fill = Channel)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  
  # Raw cross-sectional estimates as faded points
  geom_point(aes(y = Coefficient), alpha = 0.35, size = 1.5) +
  
  # Kernel-smoothed estimates as solid lines
  geom_line(aes(y = Smoothed_Coefficient), linewidth = 1.2) +
  
  scale_color_manual(values = my_palette) +
  scale_fill_manual(values = my_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(-0.5, 1.5, by = 0.25)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # Use coord_cartesian so it doesn't drop extreme points out of the dataset entirely, just zooms
  coord_cartesian(ylim = c(-0.25, 1.25)) + 
  labs(
    title = "Time-Varying Risk Sharing Channels in the Eurozone",
    x = "Year",
    y = "Fraction of Risk Absorbed",
    color = "Channel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "gray98", color = "gray90"),
    legend.margin = margin(6, 12, 6, 12)
  )

# Display the plot
ggsave("../output/channels_cross_section.pdf", plot = p_cross_sec, width = 8, height = 6)

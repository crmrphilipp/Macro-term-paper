# ══════════════════════════════════════════════════════════════════════════════
#  
# Recreating the risk sharing plot from the reference paper (p. 594/595)
#
# ══════════════════════════════════════════════════════════════════════════════

# DATA START 1987

### Clear workspace, set working directory and load libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

########################### Define data and plot generating functions
### Data cleaning
data_cleaning <- function(data, consumption){
  value <- ifelse(consumption, "consumption", "GNI")  

  ## Isolate consumption and GDP and reshape to wide format
  data_wide <- data %>%
    select(TIME_PERIOD, REF_AREA, measure, OBS_VALUE_per_capita) %>%
    pivot_wider(names_from = measure, values_from = OBS_VALUE_per_capita)

  ## Isolate aggregate and compute growth rates
  aggregate_data <- data_wide %>%
    filter(REF_AREA == "Aggregate") %>%
    arrange(TIME_PERIOD) %>%
    mutate(
      dlog_C_agg = log(.data[[value]]) - log(dplyr::lag(.data[[value]])),
      dlog_GDP_agg = log(GDP) - log(dplyr::lag(GDP))
    ) %>%
    select(TIME_PERIOD, dlog_C_agg, dlog_GDP_agg)

  ## Isolate individual countries and merge with aggregate growth rates
  country_data <- data_wide %>%
    filter(REF_AREA != "Aggregate") %>%
    group_by(REF_AREA) %>%
    arrange(TIME_PERIOD) %>%
    mutate(
      dlog_C_it = log(.data[[value]]) - log(dplyr::lag(.data[[value]])),
      dlog_GDP_it = log(GDP) - log(dplyr::lag(GDP))
    ) %>%
    ungroup() %>%
    arrange(REF_AREA, TIME_PERIOD) %>%
    # Join the aggregate growth rates back in by year
    inner_join(aggregate_data, by = "TIME_PERIOD")

  final_data <- country_data %>%
    mutate(
      y = dlog_C_it - dlog_C_agg,
      x = dlog_GDP_it - dlog_GDP_agg
    ) %>%
    # Drop the NAs that are introduced by the lag() function in the first year
    filter(!is.na(y) & !is.na(x))

  ## Run cross section regression for each year
  years <- unique(final_data$TIME_PERIOD)
  estimates <- data.frame(TIME_PERIOD = years, beta = NA)

  for (yr in years) {
    # Subset the data for this specific year
    yearly_data <- final_data %>%
      filter(TIME_PERIOD == yr)

    # Run the model and store coefficient
    model <- lm(y ~ x, data = yearly_data)
    estimates$beta[estimates$TIME_PERIOD == yr] <- coef(model)["x"]
  }

  ## Plot the estimates and smooth using a Normal Kernel
  # Smooth to a normal kernel. Note: The reference paper smoothes "the time-variation using a Normal kernel with bandwidth (standard deviation) 2." (p. 595)
  df <- estimates %>%
    mutate(risk_shared=100*(1-beta))

  smoothed_risk <- ksmooth(
    x = df$TIME_PERIOD, 
    y = df$risk_shared, 
    kernel = "normal", 
    bandwidth = 2 * (qnorm(0.75) * 4),
    x.points = df$TIME_PERIOD
  )

  df$smoothed_risk <- smoothed_risk$y
  return(df)
}

### Merged
plot_generator <- function(df, limits, left_sec = 10, seq = 1, 
                           home_bias_include = FALSE, home_bias_df = NULL, 
                           sec_limits = c(-0.8, 0.4), sec_seq = 0.2) {

  # 1. Base Plot: Risk Sharing (Left Axis - Consumption or GNI)
  p1 <- ggplot(df, aes(x = TIME_PERIOD)) +
    geom_line(aes(y = risk_shared, color = "Original Estimates"), linewidth = 0.5, alpha = 0.6) +
    geom_point(aes(y = risk_shared, color = "Original Estimates"), size = 2) +
    geom_line(aes(y = smoothed_risk, color = "Smoothed Estimates"), linewidth = 0.8) +
    geom_point(aes(y = smoothed_risk, color = "Smoothed Estimates"), shape = 15, size = 3)
  
  # 2. Optional Addition: Foreign Assets over GDP (Right Axis)
  if (home_bias_include) {
    if (is.null(home_bias_df)) stop("You must provide the Home Bias dataframe when home_bias_include is TRUE.")
    
    # Filter right-axis data to match the years present in the main df
    valid_years <- unique(df$TIME_PERIOD)
    ehb_filtered <- home_bias_df %>% filter(year %in% valid_years)
    
    # Calculate mapping parameters for the dual axis
    prim_min <- limits[1]
    prim_max <- limits[2]
    prim_range <- prim_max - prim_min
    
    sec_min <- sec_limits[1] 
    sec_max <- sec_limits[2] 
    sec_range <- sec_max - sec_min 
    
    scale_factor <- prim_range / sec_range
    
    # Add mapped Right Axis lines and points
    p1 <- p1 +
      geom_line(data = ehb_filtered, 
                aes(x = year, y = (value - sec_min) * scale_factor + prim_min, color = "Foreign Assets over GDP"), 
                linewidth = 0.8) +
      geom_point(data = ehb_filtered, 
                 aes(x = year, y = (value - sec_min) * scale_factor + prim_min, color = "Foreign Assets over GDP"), 
                 size = 2.5, shape = 17)
    
    # Configure dual y-axes
    p1 <- p1 + 
      scale_y_continuous(
        labels = function(x) sprintf("%.1f%%", x),
        limits = c(prim_min, prim_max),
        breaks = seq(prim_min, prim_max, by = left_sec),
        expand = c(0, 0),
        sec.axis = sec_axis(
          # Inverse transformation for the axis labels
          transform = ~ (. - prim_min) / scale_factor + sec_min, 
          name = "Natural Log. of (assets/GDP)",
          breaks = seq(sec_limits[1], sec_limits[2], by = sec_seq),
          labels = function(x) sprintf("%.1f", x)
        )
      ) +
      scale_color_manual(
        name = "Estimate Type",
        values = c("Original Estimates" = "#4c6aff", 
                   "Smoothed Estimates" = "#080885",
                   "Foreign Assets over GDP" = "#7df101")
      )
      
  } else {
    # 3. Standard Configuration (No Right Axis)
    p1 <- p1 + 
      geom_vline(xintercept = 1999, linetype = "dotted", color = "grey50", linewidth = 1.2) +
      geom_vline(xintercept = 2010, linetype = "dotted", color = "grey50", linewidth = 1.2) +
      scale_y_continuous(
        labels = function(x) sprintf("%.1f%%", x),
        limits = c(limits[1], limits[2]),
        breaks = seq(limits[1], limits[2], by = left_sec),
        expand = c(0, 0)
      ) +
      scale_color_manual(
        name = "Estimate Type",
        values = c("Original Estimates" = "#4c6aff", 
                   "Smoothed Estimates" = "#080885")
      )
  }
  
  # 4. Common Styling and Axes
  p1 <- p1 + 
    scale_x_continuous(
      breaks = seq(min(df$TIME_PERIOD), max(df$TIME_PERIOD), by = seq),
      expand = c(0.02, 0.02)
    ) +
    labs(y = "Percent of Risk Shared", x = "Year") +
    theme_classic() +
    theme(
      text = element_text(family = "serif"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.text = element_text(color = "black", size = 12),
      axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
      axis.title.y.right = element_text(face = "bold", size = 14, margin = margin(l = 10), angle = 270), 
      axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
      axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(margin = margin(t = 8)), 
      axis.text.y = element_text(margin = margin(r = 8)),
      axis.text.y.right = element_text(margin = margin(l = 8)),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  return(p1)
}

############################# Generate plots
########## REPLICATION
home_bias_df <- read_csv("../data/fig_2_asset_gdp.csv")
home_bias_df <- home_bias_df[,-1]
### REPLICATION 1: Current OECD data
# Consumption risk sharing
data <- read.csv("../data/data_cy_rep.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(0,70),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_rep_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_rep.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-20,35), sec_seq = 0.2,
                       home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_rep_i.pdf", plot = plot, width = 8, height = 6)

### REPLICATION 2: OLG OECD (PDF) data
# Consumption risk sharing
data <- read.csv("../data/data_cy_sanity.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-20,70),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_sanity_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_sanity.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-20,35), sec_limits = c(-1.5, 0.5), sec_seq = 0.2,
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_sanity_i.pdf", plot = plot, width = 8, height = 6)

### VOLATILITY CHECK: USING CHAINED-Linked-volumes
home_bias_df <- read_csv("../data/fig_2_asset_gdp.csv")
home_bias_df <- home_bias_df[,-1]
# Consumption risk sharing
data <- read.csv("../data/data_cy_rebased.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)
data_clean <- data_clean %>% filter(TIME_PERIOD %in% 1993:2003)

plot <- plot_generator(df = data_clean, limits = c(0,100), sec_limits = c(-0.8, 0.4),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_rebased.pdf", plot = plot, width = 8, height = 6)

##########################################################################
### EXTENSION 1: 1987 to 2017, same countries
home_bias_df <- read_csv("../data/fig_2_asset_gdp_data_long.csv")
home_bias_df <- home_bias_df[,-1]
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_1.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-10,100), seq = 5, sec_limits = c(-1.2,1),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_ext_1_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_1.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-20,50), seq = 5, sec_limits = c(-1.2, 1),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_ext_1_i.pdf", plot = plot, width = 8, height = 6)

### EXTENSION 2: 1987 to 2017, Top 50 Economies + EU (- some countries, depending on data availability)
home_bias_df <- read_csv("../data/fig_2_asset_gdp_data_long_broad.csv")
home_bias_df <- home_bias_df[,-1]
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_2.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-20,100), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_ext_2_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_2.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-70,110), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = TRUE, home_bias_df = home_bias_df)
ggsave("../output/rs_cs_ext_2_i.pdf", plot = plot, width = 8, height = 6)

#-----------------------------------------------------------------#
##### EXTENSION 3: 1987 to 2017, Eurozone (within-group aggregate)
### Will be used for Appendix
## Eurozone
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_3.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-40,100), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_3_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_3.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-70,110), seq = 5, left_sec = 30, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_3_i.pdf", plot = plot, width = 8, height = 6)

## Non-Eurozone
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_3b.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-40,100), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_3b_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_3b.csv")
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-70,110), seq = 5, left_sec = 30, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_3b_i.pdf", plot = plot, width = 8, height = 6)
#-----------------------------------------------------------------#

#-----------------------------------------------------------------#
##### EXTENSION 4: 1987 to 2017, Eurozone (same aggregate across EA and non-EA)
## Eurozone
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_2.csv")
countries_ea <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC","Aggregate",
    "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)
data <- data  %>% filter(REF_AREA %in% countries_ea)
data_clean <- data_cleaning(data = data, consumption = TRUE)

plot <- plot_generator(df = data_clean, limits = c(-40,110), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_4_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_2.csv")
countries_ea <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC","Aggregate",
    "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)
data <- data  %>% filter(REF_AREA %in% countries_ea)
data_clean <- data_cleaning(data = data, consumption = FALSE)

plot <- plot_generator(df = data_clean, limits = c(-220,190), seq = 5, left_sec = 30, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_4_i.pdf", plot = plot, width = 8, height = 6)

## Non-Eurozone
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_2.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)
countries_ea <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
    "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)
data <- data  %>% filter(!REF_AREA %in% countries_ea)

plot <- plot_generator(df = data_clean, limits = c(-40,110), seq = 5, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_4b_c.pdf", plot = plot, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_cy_ext_2.csv")
data_clean <- data_cleaning(data = data, consumption = TRUE)
countries_ea <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
    "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP"
)
data <- data  %>% filter(!REF_AREA %in% countries_ea)

plot <- plot_generator(df = data_clean, limits = c(-200,190), seq = 5, left_sec = 30, sec_limits = c(-2.5, 0.5),
                                               home_bias_include = FALSE)
ggsave("../output/rs_cs_ext_4b_i.pdf", plot = plot, width = 8, height = 6)
#-----------------------------------------------------------------#
# ══════════════════════════════════════════════════════════════════════════════
#  
# Recreating the risk sharing plot from the reference paper (p. 594/595)
#
# ══════════════════════════════════════════════════════════════════════════════

### Clear workspace, set working directory and load libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyr)

### Import data on GDP, consumption, GNI
data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

# First look at the data (can be skipped)
measure_plot <- "GDP"
unit <- c("Aggregate", "USA", "DEU", "ITA", "ESP", "FRA", "NLD", "BEL", "AUT", "DNK", "SWE", "FIN")
per_capita <- TRUE

y_column <- if (per_capita) "OBS_VALUE_per_capita" else "OBS_VALUE"

plot_data <- data %>%
  filter(REF_AREA %in% unit, measure == measure_plot)

ggplot(plot_data, aes(x = TIME_PERIOD, y = .data[[y_column]], color = REF_AREA, group = REF_AREA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Time") +
  theme_minimal()

### Consumption risk sharing regression and plot
## Isolate consumption and GDP and reshape to wide format
data_wide <- data %>%
  filter(measure %in% c("consumption", "GDP")) %>%
  select(TIME_PERIOD, REF_AREA, measure, OBS_VALUE_per_capita) %>%
  pivot_wider(names_from = measure, values_from = OBS_VALUE_per_capita)

## Isolate aggregate and compute growth rates
aggregate_data <- data_wide %>%
  filter(REF_AREA == "Aggregate") %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    dlog_C_agg = log(consumption) - log(lag(consumption)),
    dlog_GDP_agg = log(GDP) - log(lag(GDP))
  ) %>%
  select(TIME_PERIOD, dlog_C_agg, dlog_GDP_agg)

## Isolate individual countries and merge with aggregate growth rates
country_data <- data_wide %>%
  filter(REF_AREA != "Aggregate") %>%
  group_by(REF_AREA) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    dlog_C_it = log(consumption) - log(lag(consumption)),
    dlog_GDP_it = log(GDP) - log(lag(GDP))
  ) %>%
  ungroup() %>%
  arrange(REF_AREA, TIME_PERIOD) %>%
  # Join the aggregate growth rates back in by year
  inner_join(aggregate_data, by = "TIME_PERIOD")

final_data <- country_data %>%
  mutate(
    y = dlog_C_agg - dlog_C_it,
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
  bandwidth = 2,
  x.points = df$TIME_PERIOD
)

df$smoothed_risk <- smoothed_risk$y

# First, focus on years 1993-2003
df <- df %>%
  filter(TIME_PERIOD >= 1993 & TIME_PERIOD <= 2003)

ggplot(df, aes(x = TIME_PERIOD)) +
  # Original estimates
  #geom_line(aes(y = risk_shared), color = "lightpink", linewidth = 0.8, alpha = 0.7) +
  #geom_point(aes(y = risk_shared), color = "lightpink", size = 2) +
  
  # Smoothed line
  geom_line(aes(y = smoothed_risk), color = "magenta", linewidth = 0.8) +
  geom_point(aes(y = smoothed_risk), color = "magenta", shape = 15, size = 3) +
  
  # Axes and Labels
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x),
                     limits = c(0, 60),
                     breaks = seq(0, 60, by = 10),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(min(df$TIME_PERIOD), max(df$TIME_PERIOD), by = 1),
                     expand = c(0.02, 0.02)) +
  labs(y = "Percent of Risk Shared", x = "Year") +
  
  # Styling
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
    axis.ticks.length = unit(-0.15, "cm"), # Inward pointing ticks
    axis.text.x = element_text(margin = margin(t = 8)), 
    axis.text.y = element_text(margin = margin(r = 8))  
  )

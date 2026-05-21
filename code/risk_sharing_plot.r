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

############### Consumption risk sharing regression and plot
data <- read.csv("../data/data_cy.csv")

### First look at the data
# Setup static variables
measures_to_plot <- c("GDP", "consumption")
unit <- c("Aggregate", "USA", "DEU", "ITA", "ESP")
per_capita <- FALSE

# Determine column and text suffixes based on the per_capita flag
y_column <- if (per_capita) "OBS_VALUE_per_capita" else "OBS_VALUE"
label_suffix <- if (per_capita) " per capita" else ""
file_suffix <- if (per_capita) "_per_capita" else ""

# Loop through each measure
for (measure_plot in measures_to_plot) {
  
  # Filter data, rename axis label and file
  plot_data <- data %>%
    filter(REF_AREA %in% unit, measure == measure_plot)
  current_y_label <- paste0(measure_plot, label_suffix)
  current_filename <- file.path("../output", paste0(tolower(measure_plot), file_suffix, ".pdf"))
  
  # Plot generation and saving
  p <- ggplot(plot_data, aes(x = TIME_PERIOD, y = .data[[y_column]], color = REF_AREA, group = REF_AREA)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(x = "Time", y = current_y_label) +
    theme_minimal()
  ggsave(current_filename, plot = p, width = 8, height = 6)
}

## Isolate consumption and GDP and reshape to wide format
data_wide <- data %>%
  select(TIME_PERIOD, REF_AREA, measure, OBS_VALUE_per_capita) %>%
  pivot_wider(names_from = measure, values_from = OBS_VALUE_per_capita)

## Isolate aggregate and compute growth rates
aggregate_data <- data_wide %>%
  filter(REF_AREA == "Aggregate") %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    dlog_C_agg = log(consumption) - log(dplyr::lag(consumption)),
    dlog_GDP_agg = log(GDP) - log(dplyr::lag(GDP))
  ) %>%
  select(TIME_PERIOD, dlog_C_agg, dlog_GDP_agg)

## Isolate individual countries and merge with aggregate growth rates
country_data <- data_wide %>%
  filter(REF_AREA != "Aggregate") %>%
  group_by(REF_AREA) %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    dlog_C_it = log(consumption) - log(dplyr::lag(consumption)),
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
  bandwidth = 2,
  x.points = df$TIME_PERIOD
)

df$smoothed_risk <- smoothed_risk$y

#################### Plot 1: Years 1993 to 2003
df_restricted <- df %>%
  filter(TIME_PERIOD >= 1993 & TIME_PERIOD <= 2003)

p1 <- ggplot(df_restricted, aes(x = TIME_PERIOD)) +
  # Original estimates
  geom_line(aes(y = risk_shared, color="Original Estimates"), linewidth = 0.5, alpha = 0.6) +
  geom_point(aes(y = risk_shared, color = "Original Estimates"), size = 2) +
  
  # Smoothed line
  geom_line(aes(y = smoothed_risk, color = "Smoothed Estimates"), linewidth = 0.8) +
  geom_point(aes(y = smoothed_risk, color = "Smoothed Estimates"), shape = 15, size = 3) +
  
  # Axes and Labels
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x),
                     limits = c(0, 60),
                     breaks = seq(0, 60, by = 10),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(min(df$TIME_PERIOD), max(df$TIME_PERIOD), by = 1),
                     expand = c(0.02, 0.02)) +
  labs(y = "Percent of Risk Shared", x = "Year") +

  scale_color_manual(
    name = "Estimate Type",
    values = c("Original Estimates" = "lightblue", "Smoothed Estimates" = "blue")
  ) +
  
  # Styling
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = margin(t = 8)), 
    axis.text.y = element_text(margin = margin(r = 8))  
  )

ggsave("../output/risk_sharing_plot_rep.pdf", plot = p1, width = 8, height = 6)

#################### Plot 1: Years 1993 to 2024
p2 <- ggplot(df, aes(x = TIME_PERIOD)) +
  # Original estimates
  geom_line(aes(y = risk_shared, color="Original Estimates"), linewidth = 0.5, alpha = 0.6) +
  geom_point(aes(y = risk_shared, color = "Original Estimates"), size = 2) +
  
  # Smoothed line
  geom_line(aes(y = smoothed_risk, color = "Smoothed Estimates"), linewidth = 0.8) +
  geom_point(aes(y = smoothed_risk, color = "Smoothed Estimates"), shape = 15, size = 3) +
  
  # Axes and Labels
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x),
                     limits = c(0, 100),
                     breaks = seq(0, 60, by = 10),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(min(df$TIME_PERIOD), max(df$TIME_PERIOD), by = 5),
                     expand = c(0.02, 0.02)) +
  labs(y = "Percent of Risk Shared", x = "Year") +

  scale_color_manual(
    name = "Estimate Type",
    values = c("Original Estimates" = "lightblue", "Smoothed Estimates" = "blue")
  ) +
  
  # Styling
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = margin(t = 8)), 
    axis.text.y = element_text(margin = margin(r = 8))  
  )

ggsave("../output/risk_sharing_plot_extended_93_24.pdf", plot = p2, width = 8, height = 6)

############ Income risk sharing regression and plot
# to be completed


########
# Plot GDP, deviations, ...
# Replicate other paper
# Look at working paper version
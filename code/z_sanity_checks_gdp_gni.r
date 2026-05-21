###################################################################################################
################################ Sanity checks for GDP/ GNI data ##################################
###################################################################################################

### Clear workspace, set working directory and load libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(tidyr)

############### Consumption and GDP plot
data <- read.csv("../data/data_cy.csv")
# Setup static variables
measures_to_plot <- c("GDP", "consumption")
unit <- c("Aggregate", "USA", "DEU", "ITA", "ESP")
per_capita <- TRUE

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

############### GNI plot
data <- read.csv("../data/data_iy.csv")
measure_plot <- "GNI"
unit <- c("Aggregate", "USA", "DEU", "ITA", "ESP")
per_capita <- TRUE

y_column <- if (per_capita) "OBS_VALUE_per_capita" else "OBS_VALUE"
label_suffix <- if (per_capita) " per capita" else ""
file_suffix <- if (per_capita) "_per_capita" else ""

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

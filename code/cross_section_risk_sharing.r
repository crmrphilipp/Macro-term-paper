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

########################### Define plot generating function
plot_function <- function(data, consumption, limits, seq=1){

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

#################### Plot
p1 <- ggplot(df, aes(x = TIME_PERIOD)) +
  # Original estimates
  geom_line(aes(y = risk_shared, color="Original Estimates"), linewidth = 0.5, alpha = 0.6) +
  geom_point(aes(y = risk_shared, color = "Original Estimates"), size = 2) +
  
  # Smoothed line
  geom_line(aes(y = smoothed_risk, color = "Smoothed Estimates"), linewidth = 0.8) +
  geom_point(aes(y = smoothed_risk, color = "Smoothed Estimates"), shape = 15, size = 3) +
  
  # Axes and Labels
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x),
                     limits = c(limits[1], limits[2]),
                     breaks = seq(limits[1], limits[2], by = 10),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(min(df$TIME_PERIOD), max(df$TIME_PERIOD), by = seq),
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

return(p1)

}

############################# Generate plots
### REPLICATION
# Consumption risk sharing
data <- read.csv("../data/data_cy_rep.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(0,70))
ggsave("../output/rs_cs_rep_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_rep.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-20,70))
ggsave("../output/rs_cs_rep_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### EXTENSION 1: Same countries, larger time sample (1993 - 2019)
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_1.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(0,100), seq = 5)
ggsave("../output/rs_cs_ext_1_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_1.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-20,70), seq = 5)
ggsave("../output/rs_cs_ext_1_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### EXTENSION 2: All OECD countries, larger time sample (1993 - 2019)
# Consumption risk sharing
data <- read.csv("../data/data_cy_ext_2.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(0,100), seq = 5)
ggsave("../output/rs_cs_ext_2_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_ext_2.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-40,70), seq = 5)
ggsave("../output/rs_cs_ext_2_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

# ### EXTENSION 3: All OECD countries, largest time sample (1970 - 2019)
# # Consumption risk sharing
# data <- read.csv("../data/data_cy_ext_3.csv")
# plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(0,100), seq = 5)
# ggsave("../output/rs_cs_ext_3_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# # Income risk sharing
# data <- read.csv("../data/data_iy_ext_3.csv")
# plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-80,100), seq = 5)
# ggsave("../output/rs_cs_ext_3_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### EXTENSION 4: Largest time sample, Eurozone vs. non-Eurozone OECD countries
# Consumption risk sharing Eurozone
data <- read.csv("../data/data_cy_ext_4a.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-40,100), seq = 5)
ggsave("../output/rs_cs_ext_4a_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing Eurozone
data <- read.csv("../data/data_iy_ext_4a.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-130,190), seq = 5)
ggsave("../output/rs_cs_ext_4a_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

# Consumption risk sharing Non-Eurozone, but OECD (with EU)
data <- read.csv("../data/data_cy_ext_4b.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-40,100), seq = 5)
ggsave("../output/rs_cs_ext_4b_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)
 
# Income risk sharing Non-Eurozone, but OECD (with EU)
data <- read.csv("../data/data_iy_ext_4b.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-130,190), seq = 5)
ggsave("../output/rs_cs_ext_4b_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

# Consumption risk sharing Non-Eurozone, entire available sample
data <- read.csv("../data/data_cy_ext_4c.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-40,100), seq = 5)
ggsave("../output/rs_cs_ext_4c_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)
 
# Income risk sharing Non-Eurozone, entire available sample
data <- read.csv("../data/data_iy_ext_4c.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-130,190), seq = 5)
ggsave("../output/rs_cs_ext_4c_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### EXTENSION 5: Eurozone as own observation
# Consumption risk sharing Eurozone
data <- read.csv("../data/data_cy_ext_5a.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-10,100), seq = 5)
ggsave("../output/rs_cs_ext_5a_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing Eurozone
data <- read.csv("../data/data_iy_ext_5a.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-30,70), seq = 5)
ggsave("../output/rs_cs_ext_5a_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

# Consumption risk sharing Eurozone
data <- read.csv("../data/data_cy_ext_5b.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-10,100), seq = 5)
ggsave("../output/rs_cs_ext_5b_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing Eurozone
data <- read.csv("../data/data_iy_ext_5b.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-30,70), seq = 5)
ggsave("../output/rs_cs_ext_5b_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### SANITY CHECK
# Consumption risk sharing
data <- read.csv("../data/data_cy_sanity.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-20,70))
ggsave("../output/rs_cs_sanity_c.pdf", plot = plot_replication_consumption, width = 8, height = 6)

# Income risk sharing
data <- read.csv("../data/data_iy_sanity.csv")
plot_replication_gni <- plot_function(data = data, consumption = FALSE, limits = c(-20,70))
ggsave("../output/rs_cs_sanity_i.pdf", plot = plot_replication_gni, width = 8, height = 6)

### SANITY CHECK 2
# Consumption risk sharing
data <- read.csv("../data/data_cy_sanity_2.csv")
plot_replication_consumption <- plot_function(data = data, consumption = TRUE, limits = c(-20,70))
ggsave("../output/rs_cs_sanity_c_2.pdf", plot = plot_replication_consumption, width = 8, height = 6)

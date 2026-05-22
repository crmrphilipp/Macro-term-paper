
# ══════════════════════════════════════════════════════════════════════════════
#  
# Home-Bias Recreation
#
# ══════════════════════════════════════════════════════════════════════════════


#-------------------------#
###### Part I - Data ######
#-------------------------#

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



# ── Country setup ─────────────────────────────────────────────────────────────
# The 24 OECD countries from the paper (iso2c codes for WDI, iso3c for EWN/BIS)
countries_iso2 <- c("AU","AT","BE","CA","DK","FI","FR","DE","GR","IS",
                    "IE","IT","JP","MX","NL","NZ","NO","PT","ES","SE",
                    "CH","TR","GB","US")

countries_iso3 <- c("AUS","AUT","BEL","CAN","DNK","FIN","FRA","DEU","GRC","ISL",
                    "IRL","ITA","JPN","MEX","NLD","NZL","NOR","PRT","ESP","SWE",
                    "CHE","TUR","GBR","USA")

# IFS country codes for our 24 OECD countries
ifs_codes <- c(193, 122, 124, 156, 128, 172, 132, 134, 174, 176,
               178, 136, 158, 273, 138, 196, 142, 182, 184, 144,
               146, 186, 112, 111)
# Order: AUS, AUT, BEL, CAN, DNK, FIN, FRA, DEU, GRC, ISL,
#        IRL, ITA, JPN, MEX, NLD, NZL, NOR, PRT, ESP, SWE,
#        CHE, TUR, GBR, USA

# Lookup table to attach iso3c to IFS codes
ifs_to_iso3 <- tibble(
  IFS_Code = ifs_codes,
  iso      = countries_iso3
)


target_years <- c(1993, 2003)

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 1: Lane & Milesi-Ferretti / Brookings EWN Dataset
# Provides: portfolio equity assets, portfolio equity liabilities,
#           portfolio debt assets, FDI assets & liabilities, GDP
# ══════════════════════════════════════════════════════════════════════════════

ewn_url  <- "https://www.brookings.edu/wp-content/uploads/2026/02/EWN-dataset-year-end-2024_2.27.26.xlsx"
ewn_file <- "../data/EWN_dataset.xlsx"

#dir.create("../data", showWarnings = FALSE)

if (!file.exists(ewn_file)) {
  download.file(ewn_url, destfile = ewn_file, mode = "wb")
  message("EWN dataset downloaded.")
}

# Inspect sheet names
ewn_sheets <- excel_sheets(ewn_file)
print(ewn_sheets)  

# Load the main data sheet 
ewn_raw <- read_excel(ewn_file, sheet = "Dataset")

# Investigate data structure
head(ewn_raw)

# Filter countries, years and variables as needed for the table recreation
ewn <- ewn_raw |>
  filter(IFS_Code %in% ifs_codes,
         Year %in% target_years) |>
  left_join(ifs_to_iso3, by = "IFS_Code") |>
  select(
    iso,
    year        = Year,
    eq_assets   = `Portfolio equity assets (stock)`,
    eq_liab     = `Portfolio equity liabilities (stock)`,
    debt_assets = `Debt assets (stock)`,
    debt_liab   = `Portfolio debt liabilities`,
    fdi_assets  = `FDI assets (stock)`,
    fdi_liab    = `FDI liabilities (stock)`,
    gdp         = `GDP (US$)`
  )

# Filter countries, years and variables as needed for the extension of time covered
ewn_full <- ewn_raw |>
  filter(IFS_Code %in% ifs_codes) |>
  left_join(ifs_to_iso3, by = "IFS_Code") |>
  select(
    iso,
    year        = Year,
    eq_assets   = `Portfolio equity assets (stock)`,
    eq_liab     = `Portfolio equity liabilities (stock)`,
    debt_assets = `Debt assets (stock)`,
    debt_liab   = `Portfolio debt liabilities`,
    fdi_assets  = `FDI assets (stock)`,
    fdi_liab    = `FDI liabilities (stock)`,
    gdp         = `GDP (US$)`
  )

ewn_full <- ewn_full %>% filter(year>1992)

# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 2: World Bank WDI — Stock Market Capitalisation
# Provides: country market cap (current USD) → used to compute A = share of
#           world market cap, and total equity portfolio denominator
# ══════════════════════════════════════════════════════════════════════════════

# Load data via R package for OECD countries
wdi_raw <- WDI(
  indicator = c(mktcap = "CM.MKT.LCAP.CD"),
  country   = countries_iso2,
  start     = 1993,
  end       = 2024,
  extra     = FALSE
)

# Investigate data structure
head(wdi_raw)

# Fiter years and variables as needed for table recreation
wdi <- wdi_raw |>
  filter(year %in% target_years) |>
  select(iso2c, year, mktcap) |>
  mutate(iso = countrycode::countrycode(iso2c, "iso2c", "iso3c",
                                        warn = FALSE)) |>
  filter(iso %in% countries_iso3) |>
  select(iso, year, mktcap)

# Filter countries, years and variables as needed for the extension of time covered
wdi_full <- wdi_raw |>
  select(iso2c, year, mktcap) |>
  mutate(iso = countrycode::countrycode(iso2c, "iso2c", "iso3c",
                                        warn = FALSE)) |>
  filter(iso %in% countries_iso3) |>
  select(iso, year, mktcap)


# Load Data for World total market cap from WDI
wdi_world <- WDI(
  indicator = c(mktcap = "CM.MKT.LCAP.CD"),
  country   = "all",           
  start     = 1993,
  end       = 2024,
  extra     = TRUE             
) 

# Aggregate and filter accordingly for table
wdi_world_agg <- wdi_world %>%
  filter(year %in% target_years,
         region != "Aggregates") |>   # exclude pre-summed regional aggregates
  group_by(year) |>
  summarise(world_mktcap = sum(mktcap, na.rm = TRUE), .groups = "drop")

# Aggregate and filter accordingly for extension
wdi_world_agg_full <- wdi_world %>%
  filter(region != "Aggregates") |>   # exclude pre-summed regional aggregates
  group_by(year) |>
  summarise(world_mktcap = sum(mktcap, na.rm = TRUE), .groups = "drop")



# ══════════════════════════════════════════════════════════════════════════════
# Merge the two Sources and Compute EHB
# ══════════════════════════════════════════════════════════════════════════════


# Merge for table recreation
# WDI data is in absolute dollars, EWN in millions, WDI is thus converted
df <- ewn |>
  left_join(wdi,       by = c("iso", "year")) |>
  left_join(wdi_world_agg, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  )

# Merge for extension
# WDI data is in absolute dollars, EWN in millions, WDI is thus converted
df_full <- ewn_full |>
  left_join(wdi_full,       by = c("iso", "year")) |>
  left_join(wdi_world_agg_full, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  )

# ── Compute Equity Home Bias ──────────────────────────────────────────────────

df <-df |>
  mutate(
    # Total equity portfolio = domestic market cap + foreign equity held - equity held by foreigners
    total_eq_portfolio = mktcap + eq_assets - eq_liab,
    
    # Share of foreign equity in total portfolio (column 1 in Table 2, as %)
    share_foreign_eq   = eq_assets / total_eq_portfolio,
    
    # Country share of world market cap
    A                  = mktcap / world_mktcap,
    
    # Equity Home Bias (column 2 in Table 2)
    EHB                = 1 - share_foreign_eq / (1 - A)
  )

df_full <-df_full |>
  mutate(
    # Total equity portfolio = domestic market cap + foreign equity held - equity held by foreigners
    total_eq_portfolio = mktcap + eq_assets - eq_liab,
    
    # Share of foreign equity in total portfolio (column 1 in Table 2, as %)
    share_foreign_eq   = eq_assets / total_eq_portfolio,
    
    # Country share of world market cap
    A                  = mktcap / world_mktcap,
    
    # Equity Home Bias (column 2 in Table 2)
    EHB                = 1 - share_foreign_eq / (1 - A)
  )
  
# ══════════════════════════════════════════════════════════════════════════════
# Check and Clean EHB data
# ══════════════════════════════════════════════════════════════════════════════

# Get data for 1993 to 2003 only
ehb_raw <- df_full |>
  filter(year>1992, year<2004)

# Exclude negative total equity portfolio values
# see log book for further discussion
sum(ehb_raw$total_eq_portfolio<0, na.rm = TRUE)

sum(ehb_raw$EHB>1, na.rm=TRUE)
  # counting negative total equity protfolio and counting larger 1 EHB gives same number


ehb_reg <- ehb_raw |>
  filter(is.na(total_eq_portfolio) | !total_eq_portfolio<0)
  # 11 observations were dropped
  # 4 of those are Ireland which we will completely discard
  # 1 is italy 1998 coming from a huge drop in market capitalisation for that year (see plots below) while all other variables are continuous
    # this most likely comes from a change in the stock exchange structure in Italy between 1997 and 1998 (https://en.wikipedia.org/wiki/Borsa_Italiana)
  # 1 of it is France 1998 which also saw a strong drop in market capitalisation in that year (less strong than Italy)
    # again all other variables are continuous as can be seen below
    # there is no direct historic explanation for this
  # 5 are Finland 1999-2003

# Check for EHB values larger 1
sum(ehb_reg$EHB>1)

  # gives 0 counts

# we excluded all EHB values > 1 with all of them coming from total equity values lower 0

# look for negative EHB values
sum(ehb_raw$EHB<0, na.rm = TRUE)
  # 4 negative values
    # 3 are from Ireland which will be discarded
    # 1 is from Austria 1998
      # also austria exhibits a very pronounced decline in market capitalisation in 1998 with all other variables continuous
      # this again comes most likely from a restructuring in the Vienna Stock Exchange in December 1997 (https://www.wienerborse.at/en/about-us/vienna-stock-exchange/250-years-wiener-boerse/history/)


ehb_reg <- ehb_reg |>
  filter(is.na(EHB) | !EHB<0)
# 4 observations dropped

# unweighted EHB mean
ehb_reg <- ehb_reg |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_reg <- ehb_reg |>
  mutate(EHB_dev = EHB-EHB_mean)

# plot mean trend
ehb_mean_ts <- ehb_reg |>
  distinct(year, EHB_mean)

ggplot(ehb_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean EHB over Time",
    x = "Year", y = "Mean EHB"
  ) +
  theme_minimal()

# looks similar to the plot from Soresen but not identical
# starts on roughly the same level but declines slightly more

# plot in paper style
p <- ggplot(ehb_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line(color = "#f472c7", linewidth = 0.8) +
  geom_point(color = "#f472c7", shape = 15, size = 2.5) +  # shape 15 = filled square
  scale_x_continuous(
    breaks = 1993:2003,
    limits = c(1993, 2003)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    caption = "Note: Cross-sectional mean for 22 OECD countries."
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 0, size = 9),
    axis.text.y  = element_text(size = 9),
    axis.title   = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
p

ggsave(
  filename = "../output/equity_home_bias.png",
  plot     = p,
  width    = 5,      # inches — adjust to taste
  height   = 3.5,
  dpi      = 300
)


# plot mean deviation
ggplot(ehb_reg, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "EHB - Year Mean",
    color = "Country"
  ) +
  theme_minimal()+
  guides(color = guide_legend(ncol = 2))

  # austria is still weird
  # drops from 0.65 in 1997 to 0.4 in 1999 (98 discarded)
  # goes to 0.2 in 2000 and slowly increases back to 0.4 in 2003 (where the 2003 value is the same as in the paper)
  # we should do one version without Austria

# plot in paper style

library(ggrepel)

# label data: last observation per country
ehb_labels <- ehb_reg |>
  group_by(iso) |>
  filter(year == max(year)) |>
  ungroup()

p2 <- ggplot(ehb_reg, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.5, show.legend = FALSE) +
  geom_point(shape = 15, size = 1.8, show.legend = FALSE) +
  geom_text_repel(
    data          = ehb_labels,
    aes(label     = iso),
    size          = 2.5,
    nudge_x       = 0.4,          # push labels to the right of 2003
    direction     = "y",          # only repel vertically
    hjust         = 0,
    segment.size  = 0.3,
    segment.color = "grey70",
    show.legend   = FALSE
  ) +
  scale_x_continuous(
    breaks = 1993:2003,
    limits = c(1993, 2006)        # extra room on the right for labels
  ) +
  labs(
    x       = "Year",
    y       = "EHB Deviation from Yearly Mean",
    caption = "Note: Deviation of country-level equity home bias from the unweighted cross-country mean."
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 9),
    axis.title   = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = "../output/ehb_deviation_by_country.png",
  plot     = p2,
  width    = 6,
  height   = 4,
  dpi      = 300
)


# export data
ehb_reg_small <- ehb_reg |>
  select(iso,year,EHB_dev)

write_csv(ehb_reg_small, "../data/ehb_reg_small.csv")

#---------------------------------------
# as an additional version of the EHB data we exclude Austria to make sure that the outlier values dont drive the estimation results
#----------------------------------------

# filter data
ehb_reg_no_aut <- ehb_reg |>
  filter(iso!="AUT")

# unweighted EHB mean
ehb_reg_no_aut <- ehb_reg_no_aut |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_reg_no_aut <- ehb_reg_no_aut |>
  mutate(EHB_dev = EHB-EHB_mean)

# plot mean trend
ehb_mean_no_aut_ts <- ehb_reg_no_aut |>
  distinct(year, EHB_mean)

ggplot(ehb_mean_no_aut_ts, aes(x = year, y = EHB_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean EHB over Time",
    x = "Year", y = "Mean EHB"
  ) +
  theme_minimal()

# plot mean deviation
ggplot(ehb_reg_no_aut, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "EHB - Year Mean",
    color = "Country"
  ) +
  theme_minimal()+
  guides(color = guide_legend(ncol = 2))

# save
ehb_reg_no_aut_small <- ehb_reg_no_aut |>
  select(iso,year,EHB_dev)

write_csv(ehb_reg_no_aut_small, "../data/ehb_reg_no_aut_small.csv")

# ══════════════════════════════════════════════════════════════════════════════
# Crude EHB measure
# ══════════════════════════════════════════════════════════════════════════════

#oecd_data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

#oecd_wide <- oecd_data |>
#  pivot_wider(
#    id_cols     = c(REF_AREA, TIME_PERIOD, Population),
#    names_from  = measure,
#    values_from = c(OBS_VALUE, OBS_VALUE_per_capita)
  )

#oecd_wide <-oecd_wide |>
#  rename(iso=REF_AREA, year=TIME_PERIOD, pop=Population, cons=OBS_VALUE_consumption, gdp=OBS_VALUE_GDP, gni=OBS_VALUE_GNI, nni=OBS_VALUE_NNI,
#  cons_pc = OBS_VALUE_per_capita_consumption, gdp_pc=OBS_VALUE_per_capita_GDP, gni_pc=OBS_VALUE_per_capita_GNI, nni_pc=OBS_VALUE_per_capita_NNI)

#oecd_merge <- oecd_wide %>%
#    filter(year>1992, year<2004)%>%
#    select(iso, year, gdp, gdp_pc)

# prepare second ehb measure for regression
ehb_crude_raw <- df_full|>
  filter(year>1992, year<2004)

#ehb_crude_raw<-ehb_crude_raw |>
#  left_join(oecd_merge, by=c("iso", "year"))

# create the "crude" EHB measure using foreign equity over GDP
#ehb_crude_reg <- ehb_crude_raw |>
#  mutate(ehb_crude = log((eq_assets+debt_assets+fdi_assets)/gdp.y))

# create crude EHB with non ppp adjusted gdp from EWN
ehb_crude_reg <- ehb_crude_raw |>
  mutate(ehb_crude_non_ppp = log((eq_assets+debt_assets+fdi_assets)/gdp))

ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_non_ppp_tryout = log((eq_assets+debt_assets+fdi_assets)/(gdp/10)))

# unweighted mean
#ehb_crude_reg <- ehb_crude_reg |>
#  group_by(year) |>
#  mutate(
#    ehb_crude_mean = mean(ehb_crude, na.rm = TRUE),
#    n = sum(!is.na(ehb_crude))
  )

# unweighted mean for no ppp
ehb_crude_reg <- ehb_crude_reg |>
  group_by(year) |>
  mutate(
    ehb_crude_mean_non_ppp = mean(ehb_crude_non_ppp, na.rm = TRUE),
    n = sum(!is.na(ehb_crude_non_ppp))
  )

# deviation from the mean
#ehb_crude_reg <- ehb_crude_reg |>
#  mutate(ehb_crude_dev = ehb_crude-ehb_crude_mean)

# deviation from the mean for non ppp
ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_dev_non_ppp = ehb_crude_non_ppp-ehb_crude_mean_non_ppp)


# plot mean time trend
ehb_crude_mean_ts <- ehb_crude_reg |>
  distinct(year, ehb_crude_mean_non_ppp) |>
  pivot_longer(
    cols      = c(ehb_crude_mean_non_ppp),
    names_to  = "measure",
    values_to = "value"
  ) |>
  mutate(measure = recode(measure,
    "ehb_crude_mean_non_ppp" = "Non-PPP GDP"
  ))

ggplot(ehb_crude_mean_ts, aes(x = year, y = value, color = measure)) +
  geom_line() +
  geom_point() +
  labs(
    title  = "Unweighted Cross-Country Mean of Crude EHB over Time",
    x      = "Year",
    y      = "Mean log(Foreign Assets / GDP)",
    color  = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# using the non ppp adjusted data we obtain a very similar shape as in paper
# different level probably due to different GDP data?
# ppp adjusted gdp leads to strong deviation in the shape after 1999

p3 <- ggplot(ehb_crude_mean_ts, aes(x = year, y = value)) +
  geom_line(linewidth = 0.8, color = "#7df101") +
  geom_point(size = 2.5, shape = 17L, color = "#7df101") +
  scale_y_continuous(
    breaks = seq(-0.8, 0.4, by = 0.2),
    limits = c(-0.8, 0.4)
  ) +
  scale_x_continuous(
    breaks = 1993:2003,
    limits = c(1993, 2003)
  ) +
  labs(
    x       = "Year",
    y       = "Mean of log(Foreign Assets / GDP)",
    caption = paste0(
      "Note: Cross-sectional mean of log(foreign equity + debt + FDI / GDP) for 24 OECD countries.\n",
      "Replicates the right-axis series in Figure 2 of Sørensen et al. (2006) using non-PPP adjusted GDP."
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 9),
    axis.title   = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = "../output/crude_ehb_mean.png",
  plot     = p3,
  width    = 6,
  height   = 4,
  dpi      = 300
)


# plot mean deviation
#ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude_dev, group = iso, color = iso)) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#  geom_line() +
#  geom_point() +
#  labs(
#    title = "Crude EHB Deviation from Yearly Mean by Country",
#    x = "Year", y = "ehb_crude - Year Mean",
#    color = "Country"
#  ) +
#  theme_minimal()

# plot mean deviation for non ppp adjusted
ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude_dev_non_ppp, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "Crude EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "ehb_crude - Year Mean",
    color = "Country"
  ) +
  theme_minimal()

# paper stlye
ehb_crude_labels <- ehb_crude_reg |>
  group_by(iso) |>
  filter(year == max(year)) |>
  ungroup()

p4 <- ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude_dev_non_ppp, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.5, show.legend = FALSE) +
  geom_point(shape = 15, size = 1.8, show.legend = FALSE) +
  geom_text_repel(
    data         = ehb_crude_labels,
    aes(label    = iso),
    size         = 2.5,
    nudge_x      = 0.4,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.3,
    segment.color = "grey70",
    show.legend  = FALSE
  ) +
  scale_x_continuous(
    breaks = 1993:2003,
    limits = c(1993, 2006)
  ) +
  labs(
    x       = "Year",
    y       = "Crude EHB Deviation from Yearly Mean",
    caption = "Note: Deviation of country-level crude equity home bias from the unweighted cross-country mean. Non-PPP adjusted GDP."
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 9),
    axis.title   = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  filename = "../output/crude_ehb_deviation_by_country.png",
  plot     = p4,
  width    = 6,
  height   = 4,
  dpi      = 300
)


# crude EHB in different compositions
ehb_crude_reg <- ehb_crude_reg |>
  mutate(
    eq_ehb_crude_non_ppp = log((eq_assets)/gdp),
    debt_ehb_crude_non_ppp = log((debt_assets)/gdp),
    fdi_ehb_crude_non_ppp = log((fdi_assets)/gdp),
    eq_debt_ehb_crude_non_ppp = log((eq_assets+debt_assets)/gdp)
    )


# unweighted mean for no ppp
ehb_crude_reg <- ehb_crude_reg |>
  group_by(year) |>
  mutate(
    eq_ehb_crude_mean_non_ppp = mean(eq_ehb_crude_non_ppp, na.rm = TRUE),
    debt_ehb_crude_mean_non_ppp = mean(debt_ehb_crude_non_ppp, na.rm = TRUE),
    fdi_ehb_crude_mean_non_ppp = mean(fdi_ehb_crude_non_ppp, na.rm = TRUE),
    eq_debt_ehb_crude_mean_non_ppp = mean(eq_debt_ehb_crude_non_ppp, na.rm = TRUE)
  )

# deviation from the mean for non ppp
ehb_crude_reg <- ehb_crude_reg |>
  mutate(
    eq_ehb_crude_dev_non_ppp = eq_ehb_crude_non_ppp - eq_ehb_crude_mean_non_ppp,
    debt_ehb_crude_dev_non_ppp = debt_ehb_crude_non_ppp - debt_ehb_crude_mean_non_ppp,
    fdi_ehb_crude_dev_non_ppp = fdi_ehb_crude_non_ppp - fdi_ehb_crude_mean_non_ppp,
    eq_debt_ehb_crude_dev_non_ppp = eq_debt_ehb_crude_non_ppp - eq_debt_ehb_crude_mean_non_ppp
    )


ehb_crude_reg_small <- ehb_crude_reg |>
  select(iso,year,ehb_crude_dev_non_ppp, eq_ehb_crude_dev_non_ppp, debt_ehb_crude_dev_non_ppp, fdi_ehb_crude_dev_non_ppp, eq_debt_ehb_crude_dev_non_ppp)

write_csv(ehb_crude_reg_small, "../data/ehb_crude_reg_small.csv")



#=====================================#
##### Part II - Graphs and Checks #####
#=====================================#


# ══════════════════════════════════════════════════════════════════════════════
# Check and Visualize Data
# ══════════════════════════════════════════════════════════════════════════════

library(ggplot2)

### 1a. EHB (one line per country) ###
ggplot(ehb_reg, aes(x = year, y = EHB, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Home Bias (EHB) by Country",
    x = "Year", y = "EHB",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

### 1b. EHB without Austria (one line per country) ###
ggplot(ehb_reg_no_aut, aes(x = year, y = EHB, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Home Bias (EHB) by Country (Austria excluded)",
    x = "Year", y = "EHB",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

### 2a. EHB_dev (deviation from yearly mean, one line per country) ###
ggplot(ehb_reg, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "EHB - Year Mean",
    color = "Country"
  ) +
  theme_minimal()+
  guides(color = guide_legend(ncol = 2))

### 2b. EHB_dev (deviation from yearly mean, one line per country) ###
ggplot(ehb_reg_no_aut, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "EHB Deviation from Yearly Mean by Country (Austria excluded)",
    x = "Year", y = "EHB - Year Mean",
    color = "Country"
  ) +
  theme_minimal()+
  guides(color = guide_legend(ncol = 2))


### 3a. EHB_mean ###

# one observation per year — collapse first to avoid overplotting
ehb_mean_ts <- ehb_reg |>
  distinct(year, EHB_mean)

ggplot(ehb_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean EHB over Time",
    x = "Year", y = "Mean EHB"
  ) +
  theme_minimal()

### 3b. EHB_mean and EHB_mean without Austria ###

ehb_mean_ts_no_aut <- ehb_reg_no_aut |>
  distinct(year, EHB_mean)

ehb_mean_ts_no_aut <- ehb_mean_ts_no_aut|>
  rename(no_aut_EHB_mean=EHB_mean)

ehb_mean_ts <- ehb_mean_ts|>
  left_join(ehb_mean_ts_no_aut, by="year")

ehb_mean_ts_long <- ehb_mean_ts |>
  pivot_longer(
    cols      = c(EHB_mean, no_aut_EHB_mean),
    names_to  = "series",
    values_to = "value"
  ) |>
  mutate(series = recode(series,
    "EHB_mean"        = "All countries",
    "no_aut_EHB_mean" = "Excl. Austria"
  ))

ggplot(ehb_mean_ts_long, aes(x = year, y = value, color = series, linetype = series)) +
  geom_line() +
  geom_point() +
  labs(
    title  = "Unweighted Cross-Country Mean EHB over Time",
    x      = "Year",
    y      = "Mean EHB",
    color  = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


### 4. ehb_crude (one line per country) ###
ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Crude EHB [log(Foreign Equity / GDP)] by Country",
    x = "Year", y = "log(eq_assets / GDP)",
    color = "Country"
  ) +
  theme_minimal()

### 5. ehb_crude_dev (one line per country) ###
ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  labs(
    title = "Crude EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "ehb_crude - Year Mean",
    color = "Country"
  ) +
  theme_minimal()

### 6. ehb_crude_mean (one observation per year) ###
ehb_crude_mean_ts <- ehb_crude_reg |>
  distinct(year, ehb_crude_mean)

ggplot(ehb_crude_mean_ts, aes(x = year, y = ehb_crude_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean of Crude EHB over Time",
    x = "Year", y = "Mean log(eq_assets / GDP)"
  ) +
  theme_minimal()


# ══════════════════════════════════════════════════════════════════════════════
# Investigate Data/Country Problems that occurred
# ══════════════════════════════════════════════════════════════════════════════

# furhter investigation of the variables on which EHB is built to spot outliers or discontinuities

# get replication time period
df_replication_years <- df_full %>%
  filter(year>1992, year<2004)

## 1a. eq_assets
ggplot(df_replication_years, aes(x = year, y = eq_assets, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Assets by Country",
    x = "Year", y = "Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## 1b. logged eq_assets
ggplot(df_replication_years, aes(x = year, y = log(eq_assets), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Assets by Country",
    x = "Year", y = "Log Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## 2a. eq_liab
ggplot(df_replication_years, aes(x = year, y = eq_liab, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Liabilities by Country",
    x = "Year", y = "Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## 2b. logged eq_liab
ggplot(df_replication_years, aes(x = year, y = log(eq_liab), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Liabilities by Country",
    x = "Year", y = "Log Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## 3a. mktcap
ggplot(df_replication_years, aes(x = year, y = mktcap, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Market Capitalisation by Country",
    x = "Year", y = "Market Capitalisation",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## 3b. logged mktcap
ggplot(df_replication_years, aes(x = year, y = log(mktcap), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Market Capitalisation by Country",
    x = "Year", y = "Log Market Capitalisation",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))


## 4. aggregate mktcap trend
market_cap_world_ts <- df_replication_years |>
  distinct(year, world_mktcap)

ggplot(market_cap_world_ts, aes(x = year, y = world_mktcap)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Total Market Capitalisation",
    x = "Year", y = "Sum of Market Capitalisation"
  ) +
  theme_minimal()

## invstigation on Austria specifically

df_replication_years_aut <- df_replication_years %>%
  filter(iso=="AUT")

ggplot(df_replication_years_aut, aes(x = year, y = eq_assets, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Assets by Country",
    x = "Year", y = "Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_aut, aes(x = year, y = log(eq_assets), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Assets by Country",
    x = "Year", y = "Log Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_aut, aes(x = year, y = eq_liab, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Liabilities by Country",
    x = "Year", y = "Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_aut, aes(x = year, y = log(eq_liab), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Liabilities by Country",
    x = "Year", y = "Log Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_aut, aes(x = year, y = mktcap, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Market Capitalisation by Country",
    x = "Year", y = "Market Capitalisation",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## invstigation on Italy specifically
df_replication_years_ita <- df_replication_years %>%
  filter(iso=="ITA")

ggplot(df_replication_years_ita, aes(x = year, y = eq_assets, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Assets by Country",
    x = "Year", y = "Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_ita, aes(x = year, y = log(eq_assets), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Assets by Country",
    x = "Year", y = "Log Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_ita, aes(x = year, y = eq_liab, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Liabilities by Country",
    x = "Year", y = "Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_ita, aes(x = year, y = log(eq_liab), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Liabilities by Country",
    x = "Year", y = "Log Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_ita, aes(x = year, y = mktcap, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Market Capitalisation by Country",
    x = "Year", y = "Market Capitalisation",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

## invstigation on France specifically
df_replication_years_fra <- df_replication_years %>%
  filter(iso=="FRA")

ggplot(df_replication_years_fra, aes(x = year, y = eq_assets, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Assets by Country",
    x = "Year", y = "Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_fra, aes(x = year, y = log(eq_assets), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Assets by Country",
    x = "Year", y = "Log Equity Assets",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_fra, aes(x = year, y = eq_liab, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Equity Liabilities by Country",
    x = "Year", y = "Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_fra, aes(x = year, y = log(eq_liab), group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Log Equity Liabilities by Country",
    x = "Year", y = "Log Equity Liabilties",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))

ggplot(df_replication_years_fra, aes(x = year, y = mktcap, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Market Capitalisation by Country",
    x = "Year", y = "Market Capitalisation",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+
  guides(color = guide_legend(ncol = 2))




#==========================================#
##### Part III - Tables from the Paper #####
#==========================================#


# ══════════════════════════════════════════════════════════════════════════════
# Paper Table Replication: Table 1, Home Bias and Table 2
# ══════════════════════════════════════════════════════════════════════════════


# ── Formulas for Computation ──────────────────────────────────────────────────

# Formula following the descriptions of table 2
# total_eq_portfolio = mktcap + eq_assets - eq_liab
# share_foreign_eq   = eq_assets / total_eq_portfolio
# A                  = country_mktcap / world_mktcap
# EHB                = 1 - share_foreign_eq / (1 - A)




# ── Table 1 Check ─────────────────────────────────────────────────────────────

table1 <- ewn |>
  filter(year %in% target_years) |>
  mutate(
    eq_assets_gdp  = round(eq_assets  / gdp, 2),
    eq_liab_gdp    = round(eq_liab    / gdp, 2),
    debt_assets_gdp = round(debt_assets / gdp, 2),
    debt_liab_gdp  = round(debt_liab  / gdp, 2),
    fdi_assets_gdp = round(fdi_assets / gdp, 2),
    fdi_liab_gdp   = round(fdi_liab   / gdp, 2)
  ) |>
  select(iso, year, eq_assets_gdp, debt_assets_gdp, fdi_assets_gdp,
         eq_liab_gdp, debt_liab_gdp, fdi_liab_gdp) |>
  arrange(iso, year)

print(table1)

# ── Table 1 Layout Recreation ─────────────────────────────────────────────────

table1_wide <- ewn |>
  filter(year %in% target_years) |>
  mutate(
    eq_assets_gdp   = round(eq_assets   / gdp, 2),
    eq_liab_gdp     = round(eq_liab     / gdp, 2),
    debt_assets_gdp = round(debt_assets / gdp, 2),
    debt_liab_gdp   = round(debt_liab   / gdp, 2),
    fdi_assets_gdp  = round(fdi_assets  / gdp, 2),
    fdi_liab_gdp    = round(fdi_liab    / gdp, 2)
  ) |>
  select(iso, year,
         eq_assets_gdp, debt_assets_gdp, fdi_assets_gdp,
         eq_liab_gdp,   debt_liab_gdp,   fdi_liab_gdp) |>
  pivot_wider(
    names_from  = year,
    values_from = c(eq_assets_gdp, debt_assets_gdp, fdi_assets_gdp,
                    eq_liab_gdp,   debt_liab_gdp,   fdi_liab_gdp),
    names_glue  = "{.value}_{year}"
  ) |>
  # Add country names for display
  mutate(country = countrycode::countrycode(iso, "iso3c", "country.name")) |>
  arrange(country) |>
  select(country,
         eq_assets_gdp_1993,  eq_assets_gdp_2003,
         debt_assets_gdp_1993, debt_assets_gdp_2003,
         fdi_assets_gdp_1993,  fdi_assets_gdp_2003,
         eq_liab_gdp_1993,    eq_liab_gdp_2003,
         debt_liab_gdp_1993,  debt_liab_gdp_2003,
         fdi_liab_gdp_1993,   fdi_liab_gdp_2003)

table1_wide |>
  kbl(
    format  = "latex",
    booktabs = TRUE,
    caption = "County-level foreign asset and liability holdings of equity, debt, and foreign direct investment relative to GDP",
    col.names = c("Country",
                  "1993", "2003",
                  "1993", "2003",
                  "1993", "2003",
                  "1993", "2003",
                  "1993", "2003",
                  "1993", "2003"),
    align = c("l", rep("r", 12))
  ) |>
  add_header_above(c(" "       = 1,
                     "Equity"  = 2,
                     "Debt"    = 2,
                     "FDI"     = 2,
                     "Equity"  = 2,
                     "Debt"    = 2,
                     "FDI"     = 2)) |>
  add_header_above(c(" "         = 1,
                     "Assets"    = 6,
                     "Liabilities" = 6)) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("../output/table1.tex")



# ── Table 2 columns 1 and 2 ───────────────────────────────────────────────────

table2_equity <- df |>
  filter(year %in% target_years) |>
  select(iso, year, share_foreign_eq, EHB) |>
  mutate(
    share_foreign_eq = round(share_foreign_eq * 100, 2),  # as % like in paper
    EHB              = round(EHB, 2)
  ) |>
  pivot_wider(
    names_from  = year,
    values_from = c(share_foreign_eq, EHB),
    names_glue  = "{.value}_{year}"
  ) |>
  arrange(iso)

print(table2_equity)

# ── Table 2 Layout Recreation ─────────────────────────────────────────────────

table2_equity_wide <- df |>
  filter(year %in% target_years) |>
  mutate(
    total_eq_portfolio = mktcap + eq_assets - eq_liab,
    share_foreign_eq   = eq_assets / total_eq_portfolio,
    A                  = mktcap / world_mktcap,
    EHB                = 1 - share_foreign_eq / (1 - A)
  ) |>
  mutate(
    share_foreign_eq = round(share_foreign_eq * 100, 2),
    EHB              = round(EHB, 2)
  ) |>
  select(iso, year, share_foreign_eq, EHB) |>
  pivot_wider(
    names_from  = year,
    values_from = c(share_foreign_eq, EHB),
    names_glue  = "{.value}_{year}"
  ) |>
  mutate(country = countrycode::countrycode(iso, "iso3c", "country.name")) |>
  arrange(country) |>
  select(country,
         share_foreign_eq_1993, EHB_1993,
         share_foreign_eq_2003, EHB_2003)

table2_equity_wide |>
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Equity Home Bias 1993 and 2003",
    col.names = c("Country",
                  "\\shortstack{Foreign equity \\\\ in portfolio (\\%)}", "Equity Home Bias",
                  "\\shortstack{Foreign equity \\\\ in portfolio (\\%)}", "Equity Home Bias"),
    align   = c("l", "r", "r", "r", "r"),
    escape  = FALSE
  ) |>
  add_header_above(c(" " = 1, "1993" = 2, "2003" = 2)) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("../output/table2.tex")






# ══════════════════════════════════════════════════════════════════════════════
# Data Availability Diagnostics
# Check WDI market cap coverage for the 24 OECD countries, 1993–2024
# ══════════════════════════════════════════════════════════════════════════════

wdi_full <- wdi_full |>
  mutate(available = !is.na(mktcap)
  )

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1: Summary statistics — coverage per country
# ══════════════════════════════════════════════════════════════════════════════

coverage_summary <- wdi_full |>
  group_by(iso) |>
  summarise(
    n_years       = n(),
    n_available   = sum(available),
    n_missing     = sum(!available),
    pct_available = round(100 * mean(available), 1),
    # First and last year with data
    first_year    = ifelse(any(available), min(year[available]), NA),
    last_year     = ifelse(any(available), max(year[available]), NA),
    # List missing years compactly
    missing_years = {
      yrs <- year[!available]
      if (length(yrs) == 0) "none"
      else if (length(yrs) <= 6) paste(yrs, collapse = ", ")
      else paste0(paste(yrs[1:5], collapse = ", "), ", ... (", length(yrs), " total)")
    },
    .groups = "drop"
  ) |>
  arrange(pct_available, iso)


coverage_summary |>
  select(Country = iso,
         `Available` = n_available,
         `Missing` = n_missing,
         `Coverage (%)` = pct_available,
         `First obs.` = first_year,
         `Last obs.` = last_year,
         `Missing years` = missing_years) |>
  kbl(format   = "latex",
      booktabs = TRUE,
      caption  = "WDI stock market capitalisation: data availability by country, 1993–2024",
      align    = c("l","r","r","r","r","r","l")) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  footnote(general = "Coverage computed over 32 years (1993–2024). Missing years listed explicitly when 6 or fewer, otherwise summarised by count.",
           general_title = "Note.", footnote_as_chunk = TRUE)|>
           save_kable("../output/table_coverage.tex")


# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2: Year-level summary — how many countries are covered each year?
# ══════════════════════════════════════════════════════════════════════════════

year_coverage <- wdi_full |>
  group_by(year) |>
  summarise(
    n_available = sum(available),
    n_missing   = sum(!available),
    pct         = round(100 * mean(available), 1),
    .groups     = "drop"
  )

year_coverage |>
  rename(Year = year,
         `Countries with data` = n_available,
         `Countries missing`   = n_missing,
         `Coverage (%)`        = pct) |>
  kbl(format   = "latex",
      booktabs = TRUE,
      caption  = "WDI market capitalisation: number of countries with available data by year",
      align    = c("r","r","r","r")) |>
  kable_styling(latex_options = c("hold_position")) |>
  footnote(general = "Sample is the 24 OECD countries. Coverage is out of 24 countries.",
           general_title = "Note.", footnote_as_chunk = TRUE)|>
           save_kable("../output/year_coverage.tex")


# ══════════════════════════════════════════════════════════════════════════════
# Table 3: Missing data post-2003
# ══════════════════════════════════════════════════════════════════════════════

extension_gaps <- wdi_full |>
  filter(year > 2003, !available) |>
  select(iso, year) |>
  group_by(iso) |>
  summarise(missing_years = paste(year, collapse = ", "),
            n_missing     = n(),
            .groups       = "drop") |>
  arrange(desc(n_missing))

extension_gaps |>
  arrange(desc(n_missing)) |>
  rename(Country = iso,
         `Missing Years` = missing_years,
         `N Missing` = n_missing) |>
  kbl(format    = "latex",
      booktabs  = TRUE,
      caption   = "Missing WDI market capitalisation data post-2003",
      align     = c("l", "l", "r")) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  footnote(general = "Only countries with at least one missing observation after 2003 are shown.",
           general_title = "Note.", footnote_as_chunk = TRUE) |>
  save_kable("../output/table_extension_gaps.tex")







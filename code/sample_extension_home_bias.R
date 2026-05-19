# ══════════════════════════════════════════════════════════════════════════════
#  
# Home-Bias Sample Extension
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
library(countrycode)



# ── Country setup ─────────────────────────────────────────────────────────────
# The 24 OECD countries from the paper (iso2c codes for WDI, iso3c for EWN/BIS)

# Full IFS ↔ ISO lookup from the countrycode codelist
ifs_to_iso3 <- codelist |>
  filter(!is.na(imf), !is.na(iso3c)) |>
  select(
    IFS_Code     = imf,
    iso2         = iso2c,
    iso          = iso3c,
    country_name = country.name.en
  ) |>
  arrange(IFS_Code)


# Manual patches for countries missing from countrycode's IMF coverage
ifs_patches <- tribble(
  ~IFS_Code, ~iso2, ~iso,  ~country_name,
  113L,      "GG",  "GGY", "Guernsey",
  117L,      "JE",  "JEY", "Jersey",
  118L,      "IM",  "IMN", "Isle of Man",
  147L,      "LI",  "LIE", "Liechtenstein",
  171L,      "AD",  "AND", "Andorra",
  353L,      "AN",  "ANT", "Netherlands Antilles",
  371L,      "VG",  "VGB", "British Virgin Islands",
  381L,      "TC",  "TCA", "Turks and Caicos Islands",
  869L,      "TV",  "TUV", "Tuvalu",
  967L,      "XK",  "XKX", "Kosovo",
  # Regional aggregates — no ISO code
  163L,      NA,    NA,    "Euro Area",
  309L,      NA,    NA,    "Eastern Caribbean Currency Union"
)

ifs_to_iso3 <- codelist |>
  filter(!is.na(imf), !is.na(iso3c)) |>
  select(
    IFS_Code     = imf,
    iso2         = iso2c,
    iso          = iso3c,
    country_name = country.name.en
  ) |>
  arrange(IFS_Code) |>
  bind_rows(ifs_patches)

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
  left_join(ifs_to_iso3, by = "IFS_Code") |>
  select(
    iso,
    ifs         = IFS_Code,
    country     = Country,
    year        = Year,
    eq_assets   = `Portfolio equity assets (stock)`,
    eq_liab     = `Portfolio equity liabilities (stock)`,
    debt_assets = `Portfolio debt assets`,
    debt_liab   = `Portfolio debt liabilities`,
    fdi_assets  = `FDI assets (stock)`,
    fdi_liab    = `FDI liabilities (stock)`,
    gdp         = `GDP (US$)`
  )


# ══════════════════════════════════════════════════════════════════════════════
# SOURCE 2: World Bank WDI — Stock Market Capitalisation
# Provides: country market cap (current USD) → used to compute A = share of
#           world market cap, and total equity portfolio denominator
# ══════════════════════════════════════════════════════════════════════════════

# Load Data for World total market cap from WDI
wdi_world <- WDI(
  indicator = c(mktcap = "CM.MKT.LCAP.CD"),
  country   = "all",           
  start     = 1970,
  end       = 2024,
  extra     = TRUE             
) 

wdi_world <-wdi_world |>
    select(
        country,
        iso = iso3c,
        year,
        mktcap
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
# Analysis of Data Availability
# largest 50 Economies and EU countries (61 countries)
# ══════════════════════════════════════════════════════════════════════════════

eu_iso3 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
              "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
              "POL","PRT","ROU","SVK","SVN","ESP","SWE")

# Rank countries by average GDP across all years
top_n_iso <- ewn |>
  filter(!is.na(gdp), !is.na(iso)) |>
  group_by(iso, country) |>
  summarise(avg_gdp = mean(gdp, na.rm = TRUE), .groups = "drop") |>
  slice_max(avg_gdp, n = 50) |>
  pull(iso)

sample_iso <- union(top_n_iso, eu_iso3)

# variables of interest
vars_ewn <- c("eq_assets", "eq_liab", "debt_assets", "fdi_assets")
vars_wdi <- c("mktcap")

# EWN coverage
cov_ewn <- ewn |>
  filter(iso %in% sample_iso) |>
  group_by(iso, country) |>
  summarise(
    across(all_of(vars_ewn),
           ~ mean(!is.na(.)) * 100,
           .names = "{.col}_pct"),
    years_total = n(),
    year_min    = min(year),
    year_max    = max(year),
    .groups = "drop"
  )

# WDI coverage
cov_wdi <- wdi_world |>
  filter(iso %in% sample_iso) |>
  group_by(iso) |>
  summarise(
    mktcap_pct  = mean(!is.na(mktcap)) * 100,
    years_total = n(),
    year_min    = min(year),
    year_max    = max(year),
    .groups = "drop"
  )


# Graphic analysis

# Combined
cov_combined <- cov_ewn |>
  left_join(cov_wdi |> select(iso, mktcap_pct), by = "iso") |>
  arrange(desc(eq_assets_pct + mktcap_pct))


cov_long <- cov_combined |>
  select(iso, country, ends_with("_pct")) |>
  pivot_longer(ends_with("_pct"),
               names_to  = "variable",
               values_to = "pct_available") |>
  mutate(
    variable = str_remove(variable, "_pct"),
    country  = fct_reorder(country, pct_available, mean)
  )

ggplot(cov_long, aes(x = variable, y = country, fill = pct_available)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(pct_available)), size = 2.5, color = "white") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, limits = c(0, 100),
                       name = "% years\nwith data") +
  labs(title = "Data availability by country and variable",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# analysis for shorter time span (1993-2018)

cov_ewn_short <- ewn |>
  filter(iso %in% sample_iso, year>1992, year<2018) |>
  group_by(iso, country) |>
  summarise(
    across(all_of(vars_ewn),
           ~ mean(!is.na(.)) * 100,
           .names = "{.col}_pct"),
    years_total = n(),
    year_min    = min(year),
    year_max    = max(year),
    .groups = "drop"
  )

cov_wdi_short <- wdi_world |>
  filter(iso %in% sample_iso, year>1992, year<2018) |>
  group_by(iso) |>
  summarise(
    mktcap_pct  = mean(!is.na(mktcap)) * 100,
    years_total = n(),
    year_min    = min(year),
    year_max    = max(year),
    .groups = "drop"
  )

cov_combined_short <- cov_ewn_short |>
  left_join(cov_wdi_short |> select(iso, mktcap_pct), by = "iso") |>
  arrange(desc(eq_assets_pct + mktcap_pct))


cov_long_short <- cov_combined_short |>
  select(iso, country, ends_with("_pct")) |>
  pivot_longer(ends_with("_pct"),
               names_to  = "variable",
               values_to = "pct_available") |>
  mutate(
    variable = str_remove(variable, "_pct"),
    country  = fct_reorder(country, pct_available, mean)
  )

ggplot(cov_long_short, aes(x = variable, y = country, fill = pct_available)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(pct_available)), size = 2.5, color = "white") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, limits = c(0, 100),
                       name = "% years\nwith data") +
  labs(title = "Data availability by country and variable",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



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

oecd_data <- read.csv("../data/gdp_gni_consumption_per_capita.csv")

oecd_wide <- oecd_data |>
  pivot_wider(
    id_cols     = c(REF_AREA, TIME_PERIOD, Population),
    names_from  = measure,
    values_from = c(OBS_VALUE, OBS_VALUE_per_capita)
  )

oecd_wide <-oecd_wide |>
  rename(iso=REF_AREA, year=TIME_PERIOD, pop=Population, cons=OBS_VALUE_consumption, gdp=OBS_VALUE_GDP, gni=OBS_VALUE_GNI, nni=OBS_VALUE_NNI,
  cons_pc = OBS_VALUE_per_capita_consumption, gdp_pc=OBS_VALUE_per_capita_GDP, gni_pc=OBS_VALUE_per_capita_GNI, nni_pc=OBS_VALUE_per_capita_NNI)

oecd_merge <- oecd_wide %>%
    filter(year>1992, year<2004)%>%
    select(iso, year, gdp, gdp_pc)

# prepare second ehb measure for regression
ehb_crude_raw <- df_full|>
  filter(year>1992, year<2004)

ehb_crude_raw<-ehb_crude_raw |>
  left_join(oecd_merge, by=c("iso", "year"))

# create the "crude" EHB measure using foreign equity over GDP
ehb_crude_reg <- ehb_crude_raw |>
  mutate(ehb_crude = log((eq_assets+debt_assets+fdi_assets)/gdp.y))

# create crude EHB with non ppp adjusted gdp from EWN
ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_non_ppp = log((eq_assets+debt_assets+fdi_assets)/gdp.x))

# unweighted mean
ehb_crude_reg <- ehb_crude_reg |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm = TRUE),
    n = sum(!is.na(ehb_crude))
  )

# unweighted mean for no ppp
ehb_crude_reg <- ehb_crude_reg |>
  group_by(year) |>
  mutate(
    ehb_crude_mean_non_ppp = mean(ehb_crude_non_ppp, na.rm = TRUE),
    n = sum(!is.na(ehb_crude_non_ppp))
  )

# deviation from the mean
ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_dev = ehb_crude-ehb_crude_mean)

# deviation from the mean for non ppp
ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_dev_non_ppp = ehb_crude_non_ppp-ehb_crude_mean_non_ppp)


# plot mean time trend
ehb_crude_mean_ts <- ehb_crude_reg |>
  distinct(year, ehb_crude_mean, ehb_crude_mean_non_ppp) |>
  pivot_longer(
    cols      = c(ehb_crude_mean, ehb_crude_mean_non_ppp),
    names_to  = "measure",
    values_to = "value"
  ) |>
  mutate(measure = recode(measure,
    "ehb_crude_mean"         = "PPP-adjusted GDP",
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

# plot mean deviation
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


ehb_crude_reg_small <- ehb_crude_reg |>
  select(iso,year,ehb_crude_dev_non_ppp)

write_csv(ehb_crude_reg_small, "../data/ehb_crude_reg_small.csv")





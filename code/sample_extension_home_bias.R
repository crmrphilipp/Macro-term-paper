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

# Aggregate market cap
wdi_world_agg <- wdi_world %>%
  filter(iso != "Aggregates") |>   # exclude pre-summed regional aggregates
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
ewn_top60 <- ewn|>
    filter(iso %in% sample_iso)

wdi_top60 <- wdi_world|>
    filter(iso %in% sample_iso)


# Merge for table recreation
# WDI data is in absolute dollars, EWN in millions, WDI is thus converted
df_top60 <- ewn_top60 |>
  left_join(wdi_top60,       by = c("iso", "year")) |>
  left_join(wdi_world_agg, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  ) 


# ── Compute Equity Home Bias ──────────────────────────────────────────────────

df_top60 <-df_top60 |>
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

ehb_top60 <- df_top60

# Exclude negative total equity portfolio values
# see log book for further discussion
sum(ehb_top60$total_eq_portfolio<0, na.rm = TRUE)
    # 72 observations

sum(ehb_top60$EHB>1, na.rm=TRUE)
    # 68 observations

negative_portfolio <- ehb_top60 |>
    filter(!is.na(total_eq_portfolio), !is.na(EHB)) |>
    filter(total_eq_portfolio < 0 | EHB > 1)

    # the difference from 72 to 68 comes from 4 observations with EHB=1 where eq_assets=0 (Bulgaria 1996, Romania 1998-2000)
    # Luxemburg is negative from 1990 to 2024 (35 obs)
    # Ireland is negative from 2000 to 2018 (19 obs)
    # Finland from 1999 to 2004 (6 obs)
    # Bulgaria 2001
    # Cyprus 2018
    # Italy 1998
    # France 1998
    # Mexico 1978-1981 (4 obs)

    # mainly driven by Luxembourg and Ireland - Tax Havens
        # reasonable irregularities

ehb_top60 <- ehb_top60 |>
  filter(is.na(total_eq_portfolio) | !total_eq_portfolio<0)
  

# look for negative EHB values
sum(ehb_top60$EHB<0, na.rm = TRUE)
    # 34 

negative_ehb<-ehb_top60|>
    filter(!is.na(EHB)) |>
    filter(EHB < 0)
    # Austria 1998 (known) #1
    # Ireland 1997-1999 (known) #3
    # Cyprus 2011-2017, 2019-2023 #12
    # Malta 2008-2014, 2017-2024 #15
    # Netherlands 2015-2017 #3

library(ggplot2)

ehb_top60 |>
  filter(iso == "NLD") |>
  select(year, eq_assets, eq_liab, mktcap) |>
  pivot_longer(cols = c(eq_assets, eq_liab, mktcap),
               names_to = "variable",
               values_to = "value") |>
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  labs(title = "Netherlands: Equity Assets, Liabilities & Market Cap",
       x = "Year", y = "Value", color = "Variable") +
  theme_minimal()
# plot for the netherlands shows that in the relevant years mktcap drops below eq_assets and eq_liab which are basically identical
# this is probably coming from different measurement for the different sources and total_eq_portfolio should be roughly 0


ehb_top60 <- ehb_top60 |>
  filter(is.na(EHB) | !EHB<0)

# check coverage
cov_ehb <- ehb_top60 |>
  group_by(iso) |>
  summarise(
    EHB_pct     = mean(!is.na(EHB)) * 100,
    years_total = n(),
    year_min    = min(year),
    year_max    = max(year),
    .groups = "drop"
  ) |>
  arrange(desc(EHB_pct))


cov_ehb |>
  mutate(iso = fct_reorder(iso, EHB_pct)) |>
  ggplot(aes(x = "EHB", y = iso, fill = EHB_pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(EHB_pct)), size = 2.5, color = "white") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, limits = c(0, 100),
                       name = "% years\nwith data") +
  labs(title = "EHB data availability by country",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# short coverage
cov_ehb_short <- ehb_top60 |>
    filter(year>1992,year<2018)|>
    group_by(iso) |>
    summarise(
        EHB_pct     = mean(!is.na(EHB)) * 100,
        years_total = n(),
        year_min    = min(year),
        year_max    = max(year),
        .groups = "drop"
    ) |>
  arrange(desc(EHB_pct))


cov_ehb_short |>
  mutate(iso = fct_reorder(iso, EHB_pct)) |>
  ggplot(aes(x = "EHB", y = iso, fill = EHB_pct)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(EHB_pct)), size = 2.5, color = "white") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, limits = c(0, 100),
                       name = "% years\nwith data") +
  labs(title = "EHB data availability by country",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ehb_top60 |>
  mutate(
    has_data = !is.na(EHB),
    iso  = fct_reorder(iso, has_data, mean)  # order by coverage
  ) |>
  ggplot(aes(x = year, y = iso, fill = has_data)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027"),
                    labels = c("TRUE" = "Available", "FALSE" = "Missing"),
                    name = NULL) +
  labs(title = "EHB data availability by country and year",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_text(size = 7))

p4 <- ehb_top60 |>
  filter(year>1993) |>
  mutate(
    has_data = !is.na(EHB),
    iso  = fct_reorder(iso, gdp, mean)  # order by coverage
  ) |>
  ggplot(aes(x = year, y = iso, fill = has_data)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027"),
                    labels = c("TRUE" = "Available", "FALSE" = "Missing"),
                    name = NULL) +
  labs(title = "EHB data availability by country and year",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_text(size = 7))

ggsave(
  filename = "../output/ehb_coverage.png",
  plot     = p4,
  width    = 6,
  height   = 12,
  dpi      = 300
)

##### Extended Time and Sample #####

ehb_top60_restr <-ehb_top60 |>
    filter(iso!="IRL", iso!="FIN", iso!="SWE", iso!="TWN", iso!="CYP", iso!="MLT", iso!="LTU", iso!="LTA", iso!="EST")

# unweighted EHB mean
ehb_top60_restr <- ehb_top60_restr |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_top60_restr <- ehb_top60_restr |>
  mutate(EHB_dev = EHB-EHB_mean)

# plot mean trend
ehb_top60_mean_ts <- ehb_top60_restr |>
  distinct(year, EHB_mean)

ggplot(ehb_top60_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean EHB over Time",
    x = "Year", y = "Mean EHB"
  ) +
  theme_minimal()

# clearly declinign pattern

p5 <- ggplot(ehb_top60_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line(color = "#E07080", linewidth = 0.8) +
  geom_point(color = "#E07080", shape = 15, size = 2.5) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    caption = "Note: Unweighted cross-country mean equity home bias index."
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
  filename = "../output/ehb_mean_extended_1975_2024.png",
  plot     = p5,
  width    = 6,
  height   = 4,
  dpi      = 300
)

# plot mean deviation
ggplot(ehb_top60_restr, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
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


library(ggrepel)

# get last point per country for labels
last_points <- ehb_top60_restr |>
  group_by(iso) |>
  filter(year == max(year))

ggplot(ehb_top60_restr, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data = last_points,
    aes(label = iso),
    nudge_x     = 0.5,
    direction   = "y",
    hjust       = 0,
    size        = 2.5,
    segment.size = 0.3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # make room on the right
  labs(
    title = "EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "EHB - Year Mean"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# crude EHB in different compositions
ehb_top60_restr <- ehb_top60_restr |>
  mutate(
    ehb_crude         = log((eq_assets + debt_assets + fdi_assets) / gdp),
    eq_ehb_crude      = log((eq_assets) / gdp),
    debt_ehb_crude    = log((debt_assets) / gdp),
    fdi_ehb_crude     = log((fdi_assets) / gdp),
    eq_debt_ehb_crude = log((eq_assets + debt_assets) / gdp)
  ) |>
  mutate(across(c(ehb_crude, eq_ehb_crude, debt_ehb_crude, 
                  fdi_ehb_crude, eq_debt_ehb_crude),
                ~ ifelse(is.infinite(.), NA, .)))


# unweighted mean
ehb_top60_restr <- ehb_top60_restr |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm=TRUE),
    eq_ehb_crude_mean = mean(eq_ehb_crude, na.rm = TRUE),
    debt_ehb_crude_mean = mean(debt_ehb_crude, na.rm = TRUE),
    fdi_ehb_crude_mean = mean(fdi_ehb_crude, na.rm = TRUE),
    eq_debt_ehb_crude_mean = mean(eq_debt_ehb_crude, na.rm = TRUE)
  )

# deviation from the mean for non ppp
ehb_top60_restr <- ehb_top60_restr |>
  mutate(
    ehb_crude_dev = ehb_crude - ehb_crude_mean,
    eq_ehb_crude_dev = eq_ehb_crude - eq_ehb_crude_mean,
    debt_ehb_crude_dev = debt_ehb_crude - debt_ehb_crude_mean,
    fdi_ehb_crude_dev = fdi_ehb_crude - fdi_ehb_crude_mean,
    eq_debt_ehb_crude_dev = eq_debt_ehb_crude - eq_debt_ehb_crude_mean
    )


countries_ext_panel <- unique(ehb_top60_restr$iso)

countries_ext_panel


ehb_top60_restr_small <- ehb_top60_restr |>
  select(iso,year,EHB_dev, ehb_crude_dev, eq_ehb_crude_dev,debt_ehb_crude_dev,fdi_ehb_crude_dev,eq_debt_ehb_crude_dev)

write_csv(ehb_top60_restr_small, "../data/ehb_top60_restr_small.csv")


# --- Plot 1: all means as time series ---

means_long <- ehb_top60_restr |>
  ungroup() |>
  distinct(year, ehb_crude_mean, eq_ehb_crude_mean, debt_ehb_crude_mean,
           fdi_ehb_crude_mean, eq_debt_ehb_crude_mean) |>
  pivot_longer(-year, names_to = "variable", values_to = "value") |>
  mutate(variable = str_remove(variable, "_mean"))

ggplot(means_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Means of Crude EHB Measures",
       x = "Year", y = "Mean (log)", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# paper style
# variable names after str_remove: "ehb_crude", "eq_ehb_crude",
# "debt_ehb_crude", "fdi_ehb_crude", "eq_debt_ehb_crude"
# confirm with: unique(means_long$variable)

var_labels <- c(
  "ehb_crude"          = "Total (Eq. + Debt + FDI)",
  "eq_ehb_crude"       = "Equity",
  "debt_ehb_crude"     = "Debt",
  "fdi_ehb_crude"      = "FDI",
  "eq_debt_ehb_crude"  = "Equity + Debt"
)

var_colors <- c(
  "ehb_crude"          = "#6A9E6A",   # green — mirrors Figure 2 total assets line
  "eq_ehb_crude"       = "#E07080",   # pink — consistent with Figure 1 equity color
  "debt_ehb_crude"     = "#6A8DBE",   # blue
  "fdi_ehb_crude"      = "#C8963E",   # orange
  "eq_debt_ehb_crude"  = "#7B6BA8"    # purple
)

var_shapes <- c(
  "ehb_crude"          = 15L,   # filled square
  "eq_ehb_crude"       = 17L,   # filled triangle
  "debt_ehb_crude"     = 16L,   # filled circle
  "fdi_ehb_crude"      = 18L,   # filled diamond
  "eq_debt_ehb_crude"  = 8L     # asterisk
)

p6 <- ggplot(means_long, aes(x = year, y = value, color = variable, shape = variable)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  scale_color_manual(values = var_colors, labels = var_labels) +
  scale_shape_manual(values = var_shapes, labels = var_labels) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  labs(
    x       = "Year",
    y       = "Mean of log(Foreign Assets / GDP)",
    color   = NULL,
    shape   = NULL,
    caption = paste0(
      "Note: Unweighted cross-country means of crude equity home bias measures.\n",
      "Sample restricted to top 60 countries. Non-PPP adjusted GDP."
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    legend.key.width = unit(1.5, "cm"),    # wider legend keys so line style is visible
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(
    color = guide_legend(nrow = 2),   # wrap legend to 2 rows if needed
    shape = guide_legend(nrow = 2)
  )

ggsave(
  filename = "../output/crude_ehb_means_extended_1975_2024.png",
  plot     = p6,
  width    = 7,
  height   = 4.5,
  dpi      = 300
)

# --- Plot 2: ehb_crude_dev with country labels ---

last_points <- ehb_top60_restr |>
  group_by(iso) |>
  filter(year == max(year[!is.na(ehb_crude_dev)]))

ggplot(ehb_top60_restr, aes(x = year, y = ehb_crude_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data        = last_points,
    aes(label   = iso),
    nudge_x     = 0.5,
    direction   = "y",
    hjust       = 0,
    size        = 2.5,
    segment.size = 0.3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "EHB Crude Deviation from Yearly Mean by Country",
       x = "Year", y = "ehb_crude - Year Mean") +
  theme_minimal() +
  theme(legend.position = "none")


##### Extended Time Original Sample #####

countries_iso3 <- c("AUS","AUT","BEL","CAN","DNK","FIN","FRA","DEU","GRC","ISL",
                    "IRL","ITA","JPN","MEX","NLD","NZL","NOR","PRT","ESP","SWE",
                    "CHE","TUR","GBR","USA")


ehb_long_orig <-ehb_top60 |>
    filter(iso %in% countries_iso3)

# unweighted EHB mean
ehb_long_orig <- ehb_long_orig |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_long_orig <- ehb_long_orig |>
  mutate(EHB_dev = EHB-EHB_mean)

# plot mean trend
ehb_long_orig_mean_ts <- ehb_long_orig |>
  distinct(year, EHB_mean)

ggplot(ehb_long_orig_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Unweighted Cross-Country Mean EHB over Time",
    x = "Year", y = "Mean EHB"
  ) +
  theme_minimal()

# clearly declinign pattern

p7 <- ggplot(ehb_long_orig_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line(color = "#E07080", linewidth = 0.8) +
  geom_point(color = "#E07080", shape = 15, size = 2.5) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    caption = "Note: Unweighted cross-country mean equity home bias index."
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
  filename = "../output/ehb_orig_sample_1975_2024.png",
  plot     = p5,
  width    = 6,
  height   = 4,
  dpi      = 300
)

# plot mean deviation
ggplot(ehb_long_orig, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
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


# get last point per country for labels
last_points_orig <- ehb_long_orig |>
  group_by(iso) |>
  filter(year == max(year))

ggplot(ehb_long_orig, aes(x = year, y = EHB_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data = last_points,
    aes(label = iso),
    nudge_x     = 0.5,
    direction   = "y",
    hjust       = 0,
    size        = 2.5,
    segment.size = 0.3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # make room on the right
  labs(
    title = "EHB Deviation from Yearly Mean by Country",
    x = "Year", y = "EHB - Year Mean"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# crude EHB in different compositions
ehb_long_orig <- ehb_long_orig |>
  mutate(
    ehb_crude         = log((eq_assets + debt_assets + fdi_assets) / gdp),
    eq_ehb_crude      = log((eq_assets) / gdp),
    debt_ehb_crude    = log((debt_assets) / gdp),
    fdi_ehb_crude     = log((fdi_assets) / gdp),
    eq_debt_ehb_crude = log((eq_assets + debt_assets) / gdp)
  ) |>
  mutate(across(c(ehb_crude, eq_ehb_crude, debt_ehb_crude, 
                  fdi_ehb_crude, eq_debt_ehb_crude),
                ~ ifelse(is.infinite(.), NA, .)))


# unweighted mean
ehb_long_orig <- ehb_long_orig |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm=TRUE),
    eq_ehb_crude_mean = mean(eq_ehb_crude, na.rm = TRUE),
    debt_ehb_crude_mean = mean(debt_ehb_crude, na.rm = TRUE),
    fdi_ehb_crude_mean = mean(fdi_ehb_crude, na.rm = TRUE),
    eq_debt_ehb_crude_mean = mean(eq_debt_ehb_crude, na.rm = TRUE)
  )

# deviation from the mean
ehb_long_orig <- ehb_long_orig |>
  mutate(
    ehb_crude_dev = ehb_crude - ehb_crude_mean,
    eq_ehb_crude_dev = eq_ehb_crude - eq_ehb_crude_mean,
    debt_ehb_crude_dev = debt_ehb_crude - debt_ehb_crude_mean,
    fdi_ehb_crude_dev = fdi_ehb_crude - fdi_ehb_crude_mean,
    eq_debt_ehb_crude_dev = eq_debt_ehb_crude - eq_debt_ehb_crude_mean
    )


ehb_long_orig_small <- ehb_long_orig |>
  select(iso,year,EHB_dev, ehb_crude_dev, eq_ehb_crude_dev,debt_ehb_crude_dev,fdi_ehb_crude_dev,eq_debt_ehb_crude_dev)

write_csv(ehb_long_orig_small, "../data/ehb_long_orig_small.csv")


# --- Plot 1: all means as time series ---

means_long_orig <- ehb_long_orig |>
  ungroup() |>
  distinct(year, ehb_crude_mean, eq_ehb_crude_mean, debt_ehb_crude_mean,
           fdi_ehb_crude_mean, eq_debt_ehb_crude_mean) |>
  pivot_longer(-year, names_to = "variable", values_to = "value") |>
  mutate(variable = str_remove(variable, "_mean"))

ggplot(means_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Means of Crude EHB Measures",
       x = "Year", y = "Mean (log)", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# paper style
# variable names after str_remove: "ehb_crude", "eq_ehb_crude",
# "debt_ehb_crude", "fdi_ehb_crude", "eq_debt_ehb_crude"
# confirm with: unique(means_long$variable)

var_labels <- c(
  "ehb_crude"          = "Total (Eq. + Debt + FDI)",
  "eq_ehb_crude"       = "Equity",
  "debt_ehb_crude"     = "Debt",
  "fdi_ehb_crude"      = "FDI",
  "eq_debt_ehb_crude"  = "Equity + Debt"
)

var_colors <- c(
  "ehb_crude"          = "#6A9E6A",   # green — mirrors Figure 2 total assets line
  "eq_ehb_crude"       = "#E07080",   # pink — consistent with Figure 1 equity color
  "debt_ehb_crude"     = "#6A8DBE",   # blue
  "fdi_ehb_crude"      = "#C8963E",   # orange
  "eq_debt_ehb_crude"  = "#7B6BA8"    # purple
)

var_shapes <- c(
  "ehb_crude"          = 15L,   # filled square
  "eq_ehb_crude"       = 17L,   # filled triangle
  "debt_ehb_crude"     = 16L,   # filled circle
  "fdi_ehb_crude"      = 18L,   # filled diamond
  "eq_debt_ehb_crude"  = 8L     # asterisk
)

p8 <- ggplot(means_long_orig, aes(x = year, y = value, color = variable, shape = variable)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  scale_color_manual(values = var_colors, labels = var_labels) +
  scale_shape_manual(values = var_shapes, labels = var_labels) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  labs(
    x       = "Year",
    y       = "Mean of log(Foreign Assets / GDP)",
    color   = NULL,
    shape   = NULL,
    caption = paste0(
      "Note: Unweighted cross-country means of crude equity home bias measures.\n",
      "Sample restricted to original sample countries. Non-PPP adjusted GDP."
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    legend.key.width = unit(1.5, "cm"),    # wider legend keys so line style is visible
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(
    color = guide_legend(nrow = 2),   # wrap legend to 2 rows if needed
    shape = guide_legend(nrow = 2)
  )

ggsave(
  filename = "../output/crude_ehb_means_orig_long_1975_2024.png",
  plot     = p6,
  width    = 7,
  height   = 4.5,
  dpi      = 300
)

# --- Plot 2: ehb_crude_dev with country labels ---

last_points_orig <- ehb_long_orig |>
  group_by(iso) |>
  filter(year == max(year[!is.na(ehb_crude_dev)]))

ggplot(ehb_long_orig, aes(x = year, y = ehb_crude_dev, group = iso, color = iso)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data        = last_points,
    aes(label   = iso),
    nudge_x     = 0.5,
    direction   = "y",
    hjust       = 0,
    size        = 2.5,
    segment.size = 0.3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "EHB Crude Deviation from Yearly Mean by Country",
       x = "Year", y = "ehb_crude - Year Mean") +
  theme_minimal() +
  theme(legend.position = "none")


###### Euro Area ######

# Euro Area Countries
ea_iso3 <- c("AUT","BEL","CYP","EST","FIN","FRA","DEU","GRC","IRL","ITA",
             "LVA","LTU","LUX","MLT","NLD","PRT","SVK","SVN","ESP")

# ── Build two iso lists ───────────────────────────────────────────────────

# Sample 1: top 50 + EU, drop EA members
top_n_iso <- ewn |>
  filter(!is.na(gdp), !is.na(iso)) |>
  group_by(iso, country) |>
  summarise(avg_gdp = mean(gdp, na.rm = TRUE), .groups = "drop") |>
  slice_max(avg_gdp, n = 50) |>
  pull(iso)

sample_iso_ea <- union(top_n_iso, eu_iso3) |>
  setdiff(ea_iso3)

# Sample 2: countries_iso3, drop EA members
sample_iso_ea_orig <- setdiff(countries_iso3, ea_iso3)

# ── ewn ───────────────────────────────────────────────────────────────────────

ewn_ea <- ewn |>
  filter(iso %in% sample_iso_ea   | country == "Euro Area", year>1997)

ewn_ea_orig <- ewn |>
  filter(iso %in% sample_iso_ea_orig  | country == "Euro Area", year>1997)

# ── wdi_world ─────────────────────────────────────────────────────────────────

wdi_ea <- wdi_world |>
  filter(iso %in% sample_iso_ea   | country == "Euro area", year>1997)

wdi_ea_orig <- wdi_world |>
  filter(iso %in% sample_iso_ea_orig  | country == "Euro area", year>1997)


# give new ISO code EUR 
ewn_ea <- ewn_ea |>
  mutate(iso = if_else(country == "Euro Area", "EUR", iso))

ewn_ea_orig <- ewn_ea_orig |>
  mutate(iso = if_else(country == "Euro Area", "EUR", iso))

wdi_ea <- wdi_ea |>
  mutate(iso = if_else(country == "Euro area", "EUR", iso))

wdi_ea_orig <- wdi_ea_orig |>
  mutate(iso = if_else(country == "Euro area", "EUR", iso))

# Merge
# WDI data is in absolute dollars, EWN in millions, WDI is thus converted

# full sample
df_ea <- ewn_ea |>
  left_join(wdi_ea,       by = c("iso", "year")) |>
  left_join(wdi_world_agg, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  ) 

# original sample
df_ea_orig <- ewn_ea_orig |>
  left_join(wdi_ea_orig,       by = c("iso", "year")) |>
  left_join(wdi_world_agg, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  ) 


# ── Compute Equity Home Bias ──────────────────────────────────────────────────

# full sample
df_ea <-df_ea |>
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

# original sample
df_ea_orig <-df_ea_orig |>
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

### full sample ###

ehb_ea <- df_ea

# Exclude negative total equity portfolio values
# see log book for further discussion
sum(ehb_ea$total_eq_portfolio<0, na.rm = TRUE)
    # 4 observations

sum(ehb_ea$EHB>1, na.rm=TRUE)
    # 1 observations

negative_portfolio_ea <- ehb_ea |>
    filter(!is.na(total_eq_portfolio), !is.na(EHB)) |>
    filter(total_eq_portfolio < 0 | EHB > 1)

    # the difference from 4 to 1 comes from 4 observations with EHB=1 where eq_assets=0 (Bulgaria 1996, Romania 1998-2000)
    # Bulgaria 2001

ehb_ea <- ehb_ea |>
  filter(is.na(total_eq_portfolio) | !total_eq_portfolio<0)
  

# look for negative EHB values
sum(ehb_ea$EHB<0, na.rm = TRUE)
    # 4

negative_ehb_ea <-ehb_ea|>
    filter(!is.na(EHB)) |>
    filter(EHB < 0)
    # Euro Area has negative EHB for 2015-2018
    # many large Euro countries drop out of mktcap in 2015


ehb_ea |>
  filter(iso == "EUR") |>
  select(year, eq_assets, eq_liab, mktcap) |>
  pivot_longer(cols = c(eq_assets, eq_liab, mktcap),
               names_to = "variable",
               values_to = "value") |>
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  labs(title = "Netherlands: Equity Assets, Liabilities & Market Cap",
       x = "Year", y = "Value", color = "Variable") +
  theme_minimal()


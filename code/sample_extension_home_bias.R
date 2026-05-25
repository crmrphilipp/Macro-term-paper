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
library(ggplot2)



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
    debt_assets = `Debt assets (stock)`,
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


# Merge 
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

ehb_coverage <- ehb_top60 |>
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
  plot     = ehb_coverage,
  width    = 6,
  height   = 12,
  dpi      = 300
)

##### Extended Time and Sample #####

ehb_top60_restr <-ehb_top60 |>
    filter(iso!="IRL", iso!="FIN", iso!="TWN", iso!="CYP", iso!="MLT", iso!="LTU", iso!="LTA", iso!="EST")

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

broad_sample_ehb_mean <- ggplot(ehb_top60_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line(color = "#E07080", linewidth = 0.8) +
  geom_point(color = "#E07080", shape = 15, size = 2.5) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.5, 1)
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
  plot     = broad_sample_ehb_mean,
  width    = 6,
  height   = 4,
  dpi      = 300
)


#### euro area vs non euro ###

# Euro area ISO3 codes
euro_area_iso <- c(
  "AUT", "BEL", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
  "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT",
  "SVK", "SVN", "ESP"
)

# Tag each country
ehb_top60_restr <- ehb_top60_restr |>
  mutate(group_bin = if_else(iso %in% euro_area_iso, "Euro Area", "Non-Euro Area"))

# Unweighted group means by year
#ehb_top60_restr <- ehb_top60_restr |>
#  group_by(year, group) |>
#  mutate(
#    EHB_mean = mean(EHB, na.rm = TRUE),
#    n        = sum(!is.na(EHB))
#  ) |>
#  ungroup()

# weighted group means by year
ehb_top60_restr <- ehb_top60_restr |>
  group_by(year, group_bin) |>
  mutate(
    EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE),
    n        = sum(!is.na(EHB))
  ) |>
  ungroup()


# One row per year × group for plotting
ehb_top60_group_mean_ts <- ehb_top60_restr |>
  distinct(year, group_bin, EHB_mean)

# Palette
group_colors <- c("Euro Area" = "#E07080", "Non-Euro Area" = "#5B8DB8")

broad_ehb_euro_vs <- ggplot(ehb_top60_group_mean_ts, aes(x = year, y = EHB_mean,
                                            color = group_bin, shape = group_bin)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = c("Euro Area" = 15, "Non-Euro Area" = 16)) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.3, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    shape   = NULL,
    caption = "Note: Total Equity Protfolio weighted cross-country mean equity home bias index by country group."
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position  = c(0.85, 0.85),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm")
  )


ggsave(
  filename = "../output_final/ehb_mean_extended_1975_2024_2groups.png",
  plot     = broad_ehb_euro_vs,
  width    = 6,
  height   = 4,
  dpi      = 300
)


# Group definitions
euro_core_iso <- c(
  "AUT", "BEL", "FIN", "FRA", "DEU",
  "IRL", "ITA", "LUX", "NLD", "PRT", "ESP"   # founding 1999 members
)

euro_periphery_iso <- c(
  "GRC",                                        # joined 2001
  "SVN", "CYP", "MLT", "SVK",                  # joined 2007–2009
  "CRO",
  "EST", "LVA", "LTU"                           # joined 2011–2015
)

# Tag each country into one of four groups
ehb_top60_restr <- ehb_top60_restr |>
  mutate(group_4 = case_when(
    iso %in% euro_core_iso       ~ "Eurozone Core",
    iso %in% euro_periphery_iso  ~ "Eurozone Periphery",
    iso == "USA"                 ~ "United States",
    TRUE                         ~ "Rest of Sample"
  ))

# Unweighted group means by year
ehb_top60_restr <- ehb_top60_restr |>
  group_by(year, group_4) |>
  mutate(
    EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE),
    n        = sum(!is.na(EHB))
  ) |>
  ungroup()


# One row per year × group for plotting
ehb_top60_group_mean_ts <- ehb_top60_restr |>
  distinct(year, group_4, EHB_mean)

# Palette and shape mapping
group_colors_4 <- c(
  "Eurozone Core"      = "#E07080",
  "Eurozone Periphery" = "#F0A830",
  "United States"      = "#5B8DB8",
  "Rest of Sample"     = "#6DBF8A"
)

group_shapes_4 <- c(
  "Eurozone Core"      = 15,   # filled square
  "Eurozone Periphery" = 17,   # filled triangle
  "United States"      = 18,   # filled diamond
  "Rest of Sample"     = 16    # filled circle
)

# Fix legend order
ehb_top60_group_mean_ts <- ehb_top60_group_mean_ts |>
  mutate(group_4 = factor(group_4, levels = c(
    "Eurozone Core", "Eurozone Periphery", "United States", "Rest of Sample"
  )))

broad_ehb_euro_groups <- ggplot(ehb_top60_group_mean_ts, aes(x = year, y = EHB_mean,
                                            color = group_4, shape = group_4)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = group_colors_4) +
  scale_shape_manual(values = group_shapes_4) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.2, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    shape   = NULL,
    caption = paste(
      "Note: Total Equity Protfolio weighted cross-country mean equity home bias index by country group.",
      "Eurozone Core = founding 1999 members; Eurozone Periphery = countries that joined 2001–2015."
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position  = c(0.20, 0.2),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm")
  )

ggsave(
  filename = "../output_final/ehb_mean_extended_1975_2024_4groups.png",
  plot     = broad_ehb_euro_groups,
  width    = 7,
  height   = 4.5,
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
      "Note: Unweighted cross-country means of equity assets over GDP.\n",
      "Sample restricted to top 60 countries."
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
  filename = "../output_final/crude_ehb_means_extended_1975_2024.png",
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

ehb_orig_sample_1975_2024 <- ggplot(ehb_long_orig_mean_ts, aes(x = year, y = EHB_mean)) +
  geom_line(color = "#E07080", linewidth = 0.8) +
  geom_point(color = "#E07080", shape = 15, size = 2.5) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.5, 1)
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
  filename = "../output_final/ehb_orig_sample_1975_2024.png",
  plot     = ehb_orig_sample_1975_2024,
  width    = 6,
  height   = 4,
  dpi      = 300
)

##### euro are vs non euro area ####

# Euro area ISO3 codes
euro_area_iso <- c(
  "AUT", "BEL", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
  "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT",
  "SVK", "SVN", "ESP"
)

# Tag each country
ehb_long_orig <- ehb_long_orig |>
  mutate(group = if_else(iso %in% euro_area_iso, "Euro Area", "Non-Euro Area"))

# Unweighted group means by year
ehb_long_orig <- ehb_long_orig |>
  group_by(year, group) |>
  mutate(
    EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE),
    n        = sum(!is.na(EHB))
  ) |>
  ungroup()

# Deviation from group mean
ehb_long_orig <- ehb_long_orig |>
  mutate(EHB_dev = EHB - EHB_mean)

# One row per year × group for plotting
ehb_group_mean_ts <- ehb_long_orig |>
  distinct(year, group, EHB_mean)

# Palette
group_colors <- c("Euro Area" = "#E07080", "Non-Euro Area" = "#5B8DB8")

long_orig_euro_vs <- ggplot(ehb_group_mean_ts, aes(x = year, y = EHB_mean,
                                     color = group, shape = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = c("Euro Area" = 15, "Non-Euro Area" = 16)) +
  scale_x_continuous(
    breaks = seq(1975, 2024, by = 5),
    limits = c(1975, 2024)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.3, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    shape   = NULL,
    caption = "Note: Total Equity Protfolio weighted cross-country mean equity home bias index by country group."
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position  = c(0.85, 0.85),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm")
  )

ggsave(
  filename = "../output_final/ehb_orig_sample_1975_2024_groups.png",
  plot     = long_orig_euro_vs,
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


#ehb_crude_orig <- ehb_long_orig |>
#  mutate(ehb_crude_try = log((eq_assets+debt_assets+fdi_assets)/gdp))|>
#  select(iso,country.x,year, ehb_crude_try)

#ehb_crude_orig <- ehb_crude_orig |>
#  group_by(year) |>
#  mutate(
#    ehb_crude_mean = mean(ehb_crude_try, na.rm = TRUE),
#    n = sum(!is.na(ehb_crude_try))
#  )

#ehb_crude_mean_ts <- ehb_crude_orig |>
#  distinct(year, ehb_crude_mean) |>
#  pivot_longer(
#    cols      = c(ehb_crude_mean),
#    names_to  = "measure",
#    values_to = "value"
#  ) |>
#  mutate(measure = recode(measure,
#    "ehb_crude_mean" = "GDP"
#  ))


fig_2_asset_gdp_data_long <- means_long_orig |>
  filter(variable=="ehb_crude")

write.csv(fig_2_asset_gdp_data_long, "../data/fig_2_asset_gdp_data_long.csv")


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
      "Note: Unweighted cross-country means of equity assets over GDP.\n",
      "Sample restricted to original sample countries."
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
  filename = "../output_final/crude_ehb_means_orig_long_1975_2024.png",
  plot     = p8,
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


#=================================================================================================#

                                      ###### Euro Area ######

#=================================================================================================#

ewn <- ewn |>
    mutate(iso = if_else(country == "Euro Area", "EUR", iso))

#### check difference of Euro area and aggregate over all euro members ####

# --- Define Euro area member ISO codes (adjust to what's in your data) ---
ea_isos <- c("AUT", "BEL", "DEU", "ESP", "FIN", "FRA", "GRC",
             "IRL", "ITA", "LUX", "NLD", "PRT")

# --- Build euro_aggregate by summing EA member countries per year ---
euro_aggregate <- ewn %>%
  filter(iso %in% ea_isos) %>%
  group_by(year) %>%
  summarise(
    eq_assets = sum(eq_assets, na.rm = TRUE),
    eq_liab   = sum(eq_liab,   na.rm = TRUE)
  ) %>%
  mutate(iso = "euro_aggregate")

# --- Pull EUR observation ---
eur_obs <- ewn %>%
  filter(iso == "EUR") %>%
  select(iso, year, eq_assets, eq_liab)

# --- Comparison dataset ---
ewn_comparison <- bind_rows(eur_obs, euro_aggregate) %>%
  arrange(year, iso)

# --- Difference: EUR minus euro_aggregate ---
ewn_diff <- left_join(
  eur_obs        %>% rename(eq_assets_EUR  = eq_assets, eq_liab_EUR  = eq_liab),
  euro_aggregate %>% rename(eq_assets_agg  = eq_assets, eq_liab_agg  = eq_liab),
  by = "year"
) %>%
  mutate(
    diff_eq_assets = eq_assets_EUR - eq_assets_agg,
    diff_eq_liab   = eq_liab_EUR   - eq_liab_agg
  ) %>%
  select(year, starts_with("diff_"))

# difference goes from 600.000 in 1998 to 6.500.000 in 2024





# Euro Area Countries
ea_iso3 <- c("AUT","BEL","CYP","EST","FIN","FRA","DEU","GRC","IRL","ITA",
             "LVA","LTU","LUX","MLT","NLD","PRT","SVK","SVN","ESP")

# old sample countries

countries_iso3 <- c("AUS","AUT","BEL","CAN","DNK","FIN","FRA","DEU","GRC","ISL",
                    "IRL","ITA","JPN","MEX","NLD","NZL","NOR","PRT","ESP","SWE",
                    "CHE","TUR","GBR","USA")


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
    # Italy drops out of mktcap in 2015
    # the CAPM based theoretical market representation goes down significntly
    # the portfolio does not change bacause EWN includes Italy still
    # negative Home bias means over-internationalised


ehb_ea |>
  filter(iso == "EUR") |>
  select(year, eq_assets, eq_liab, mktcap) |>
  pivot_longer(cols = c(eq_assets, eq_liab, mktcap),
               names_to = "variable",
               values_to = "value") |>
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  labs(title = "Euro Area: Equity Assets, Liabilities & Market Cap",
       x = "Year", y = "Value", color = "Variable") +
  theme_minimal()

# preliminary solution - cap data at 2015

ehb_ea_cap <- ehb_ea |>
  filter(year<2015)

# unweighted EHB mean
ehb_ea_cap <- ehb_ea_cap |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_ea_cap <- ehb_ea_cap |>
  mutate(EHB_dev = EHB-EHB_mean)


# crude EHB in different compositions
ehb_ea_cap <- ehb_ea_cap |>
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
ehb_ea_cap <- ehb_ea_cap |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm=TRUE),
    eq_ehb_crude_mean = mean(eq_ehb_crude, na.rm = TRUE),
    debt_ehb_crude_mean = mean(debt_ehb_crude, na.rm = TRUE),
    fdi_ehb_crude_mean = mean(fdi_ehb_crude, na.rm = TRUE),
    eq_debt_ehb_crude_mean = mean(eq_debt_ehb_crude, na.rm = TRUE)
  )

# deviation from the mean
ehb_ea_cap <- ehb_ea_cap |>
  mutate(
    ehb_crude_dev = ehb_crude - ehb_crude_mean,
    eq_ehb_crude_dev = eq_ehb_crude - eq_ehb_crude_mean,
    debt_ehb_crude_dev = debt_ehb_crude - debt_ehb_crude_mean,
    fdi_ehb_crude_dev = fdi_ehb_crude - fdi_ehb_crude_mean,
    eq_debt_ehb_crude_dev = eq_debt_ehb_crude - eq_debt_ehb_crude_mean
    )


ehb_ea_cap_small <- ehb_ea_cap |>
  select(iso,year,EHB_dev, ehb_crude_dev, eq_ehb_crude_dev,debt_ehb_crude_dev,fdi_ehb_crude_dev,eq_debt_ehb_crude_dev)

write_csv(ehb_ea_cap_small, "../data/ehb_ea_cap_small.csv")


### analysis of the negative EHB values ###

# ── Source last-available values from wdi_world ───────────────────────────────
italy_2014_mktcap <- wdi_world |>
  filter(iso == "ITA", year == 2014) |>
  pull(mktcap)

nld_2017_mktcap <- wdi_world |>
  filter(iso == "NLD", year == 2017) |>
  pull(mktcap)

# ── Patch 1: Euro Area mktcap in wdi_ea ──────────────────────────────────────
# Italy missing 2015-2018, Netherlands missing 2018 only
wdi_ea_test <- wdi_ea |>
  mutate(
    mktcap = mktcap +
      if_else(iso == "EUR" & year %in% 2015:2018, italy_2014_mktcap, 0) +
      if_else(iso == "EUR" & year == 2018,         nld_2017_mktcap,   0)
  )

# ── Patch 2: World aggregate ───────────────────────────────────────────────────
# Same logic: both drop out via na.rm = TRUE in the original sum
wdi_world_agg_test <- wdi_world_agg |>
  mutate(
    world_mktcap = world_mktcap +
      if_else(year %in% 2015:2018, italy_2014_mktcap, 0) +
      if_else(year == 2018,         nld_2017_mktcap,   0)
  )

# ── Rebuild df_ea_test ────────────────────────────────────────────────────────
df_ea_test <- ewn_ea |>
  left_join(wdi_ea_test,        by = c("iso", "year")) |>
  left_join(wdi_world_agg_test, by = "year") |>
  mutate(
    mktcap       = mktcap       / 1e6,
    world_mktcap = world_mktcap / 1e6
  )

df_ea_test <-df_ea_test |>
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

### visual analysis ###


# Compute the three series
ehb_plot_data <- bind_rows(
  ehb_ea_cap %>%
    group_by(year) %>%
    summarise(EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE)) %>%
    mutate(group = "All countries"),

  ehb_ea_cap %>%
    filter(iso != "EUR") %>%
    group_by(year) %>%
    summarise(EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE)) %>%
    mutate(group = "Excl. Euro Area"),

  ehb_ea_cap %>%
    filter(iso == "EUR") %>%
    group_by(year) %>%
    summarise(EHB_mean = mean(EHB, na.rm = TRUE)) %>%
    mutate(group = "Euro Area only")
) %>%
  mutate(group = factor(group,
                        levels = c("All countries",
                                   "Excl. Euro Area",
                                   "Euro Area only")))

# Colours
group_colors <- c(
  "All countries"   = "#f472c7",  # pink 
  "Excl. Euro Area" = "#7c3aed",  # purple
  "Euro Area only"  = "#3b82f6"   # blue
)

# Plot
p_ea_full <- ggplot(ehb_plot_data, aes(x = year, y = EHB_mean,
                                color = group, group = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 15, size = 2.5) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(
    breaks = 1998:2014,
    limits = c(1998, 2014)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.1, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    caption = "Note: Total Equity Portfolio weighted cross-sectional means."
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position  = c(0.75, 0.85),   # top-right inside the plot area
    legend.text      = element_text(size = 8),
    legend.key.size  = unit(0.5, "lines")
  )

p_ea_full

ggsave(
  filename = "../output/equity_home_bias_three_series.png",
  plot     = p_ea_full,
  width    = 6,
  height   = 3.5,
  dpi      = 300
)


### same but now also USA separate
# Compute the three series
ehb_plot_data_usa <- bind_rows(
  #ehb_ea_cap %>%
  #  group_by(year) %>%
  #  summarise(EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE)) %>%
  #  mutate(group = "All countries"),

  #ehb_ea_cap %>%
  #  filter(iso != "EUR") %>%
  #  group_by(year) %>%
  #  summarise(EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE)) %>%
  #  mutate(group = "Excl. Euro Area"),

  ehb_ea_cap %>%
    filter(iso == "EUR") %>%
    group_by(year) %>%
    summarise(EHB_mean = mean(EHB, na.rm = TRUE)) %>%
    mutate(group = "Euro Area only"),

  ehb_ea_cap %>%
    filter(iso == "USA") %>%
    group_by(year) %>%
    summarise(EHB_mean = mean(EHB, na.rm = TRUE)) %>%
    mutate(group = "USA only"),

  ehb_ea_cap %>%
    filter(!iso %in% c("EUR", "USA")) %>%
    group_by(year) %>%
    summarise(EHB_mean = weighted.mean(EHB, w = total_eq_portfolio, na.rm = TRUE)) %>%
    mutate(group = "Excl. Euro Area & USA")
) %>%
  mutate(group = factor(group,
                        levels = c(
   #                                "All countries",
   #                                "Excl. Euro Area",
                                   "Excl. Euro Area & USA",
                                   "Euro Area only",
                                   "USA only")))

# Colours
group_colors_usa <- c(
  #"All countries"         = "#f472c7",  
  # "Excl. Euro Area"       = "#7c3aed",  
  "Excl. Euro Area & USA" = "#a855f7", 
  "Euro Area only"        = "#f472c7", 
  "USA only"              = "#0ea5e9"  
)

# Plot
p_ea_full_usa <- ggplot(ehb_plot_data_usa, aes(x = year, y = EHB_mean,
                                color = group, group = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 15, size = 2.5) +
  scale_color_manual(values = group_colors_usa) +
  scale_x_continuous(
    breaks = 1998:2014,
    limits = c(1998, 2014)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0.1, 1)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    caption = "Note: Total equity portfolio weighted cross-sectional means."
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y     = element_text(size = 9),
    axis.title      = element_text(size = 10),
    plot.caption    = element_text(hjust = 0, size = 8),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.72, 0.82),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.5, "lines")
  )

p_ea_full_usa

ggsave(
  filename = "../output_final/equity_home_bias_three_series.png",
  plot     = p_ea_full_usa,
  width    = 6,
  height   = 3.5,
  dpi      = 300
)

### now for crude ehb

# Compute the three series
crude_ehb_plot_data <- bind_rows(
  ehb_ea_cap %>%
    filter(!is.na(ehb_crude), !is.na(total_eq_portfolio)) %>%   # <-- add this
    group_by(year) %>%
    summarise(crude_ehb_mean = weighted.mean(ehb_crude, w = total_eq_portfolio)) %>%
    mutate(group = "All countries"),

  ehb_ea_cap %>%
    filter(iso != "EUR", !is.na(ehb_crude), !is.na(total_eq_portfolio)) %>%  # <--
    group_by(year) %>%
    summarise(crude_ehb_mean = weighted.mean(ehb_crude, w = total_eq_portfolio)) %>%
    mutate(group = "Excl. Euro Area"),

  ehb_ea_cap %>%
    filter(iso == "EUR") %>%
    group_by(year) %>%
    summarise(crude_ehb_mean = mean(ehb_crude, na.rm = TRUE)) %>%
    mutate(group = "Euro Area only")
) %>%
  mutate(group = factor(group, levels = c("All countries", "Excl. Euro Area", "Euro Area only")))

# Colours
group_colors <- c(
  "All countries"   = "#f472c7",  # pink 
  "Excl. Euro Area" = "#7c3aed",  # purple
  "Euro Area only"  = "#3b82f6"   # blue
)

# Plot
crude_p_ea_full <- ggplot(crude_ehb_plot_data, aes(x = year, y = crude_ehb_mean,
                                color = group, group = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 15, size = 2.5) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(
    breaks = 1998:2014,
    limits = c(1998, 2014)
  ) +
  scale_y_continuous(
    breaks = seq(-1.4, 0.8, by = 0.2),
    limits = c(-0.5, 0.7)
  ) +
  labs(
    x       = "Year",
    y       = "Home Bias Index",
    color   = NULL,
    caption = "Note: Unweighted cross-sectional means."
  ) +
  theme_classic() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y      = element_text(size = 9),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position  = c(0.75, 0.85),   # top-right inside the plot area
    legend.text      = element_text(size = 8),
    legend.key.size  = unit(0.5, "lines")
  )

crude_p_ea_full

ggsave(
  filename = "../output/crude_equity_home_bias_three_series.png",
  plot     = crude_p_ea_full,
  width    = 6,
  height   = 3.5,
  dpi      = 300
)


### same but now also USA separate
# Compute the five series
crude_ehb_plot_data_usa <- bind_rows(
  #ehb_ea_cap %>%
  #  filter(!is.na(ehb_crude)) %>%
  #  group_by(year) %>%
  #  summarise(crude_ehb_mean = mean(ehb_crude)) %>%
  #  mutate(group = "All countries"),

  #ehb_ea_cap %>%
  #  filter(iso != "EUR", !is.na(ehb_crude)) %>%
  #  group_by(year) %>%
  #  summarise(crude_ehb_mean = mean(ehb_crude)) %>%
  #  mutate(group = "Excl. Euro Area"),

  ehb_ea_cap %>%
    filter(iso == "EUR", !is.na(ehb_crude)) %>%
    group_by(year) %>%
    summarise(crude_ehb_mean = mean(ehb_crude)) %>%
    mutate(group = "Euro Area only"),

  ehb_ea_cap %>%
    filter(iso == "USA", !is.na(ehb_crude)) %>%
    group_by(year) %>%
    summarise(crude_ehb_mean = mean(ehb_crude)) %>%
    mutate(group = "USA only"),

  ehb_ea_cap %>%
    filter(!iso %in% c("EUR", "USA"), !is.na(ehb_crude)) %>%
    group_by(year) %>%
    summarise(crude_ehb_mean = mean(ehb_crude)) %>%
    mutate(group = "Excl. Euro Area & USA")
) %>%
  mutate(group = factor(group,
                        levels = c(
     #                              "All countries",
     #                              "Excl. Euro Area",
                                   "Excl. Euro Area & USA",
                                   "Euro Area only",
                                   "USA only")))

# Colours
group_colors_usa <- c(
  #"All countries"         = "#f472c7",  # pink
  #"Excl. Euro Area"       = "#7c3aed",  # purple
  "Excl. Euro Area & USA" = "#a855f7",  # light purple
  "Euro Area only"        = "#f472c7",  # blue
  "USA only"              = "#0ea5e9"   # light blue
)

# Plot
crude_p_ea_full_usa <- ggplot(crude_ehb_plot_data_usa, aes(x = year, y = crude_ehb_mean,
                                color = group, group = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 15, size = 2.5) +
  scale_color_manual(values = group_colors_usa) +
  scale_x_continuous(
    breaks = 1998:2014,
    limits = c(1998, 2014)
  ) +
  scale_y_continuous(
    breaks = seq(-1.4, 0.8, by = 0.2),
    limits = c(-1.3, 0.7)
  ) +
  labs(
    x       = "Year",
    y       = "Equity Assets over GDP",
    color   = NULL,
    caption = "Note: Total equity portfolio weighted cross-sectional means."
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y     = element_text(size = 9),
    axis.title      = element_text(size = 10),
    plot.caption    = element_text(hjust = 0, size = 8),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.72, 0.82),
    legend.text     = element_text(size = 8),
    legend.key.size = unit(0.5, "lines")
  )

crude_p_ea_full_usa

ggsave(
  filename = "../output_final/crude_equity_home_bias_five_series.png",
  plot     = crude_p_ea_full_usa,
  width    = 6,
  height   = 3.5,
  dpi      = 300
)




###### original sample #########

ehb_ea_orig <- df_ea_orig

# Exclude negative total equity portfolio values
# see log book for further discussion
sum(ehb_ea_orig$total_eq_portfolio<0, na.rm = TRUE)
  # 0 obs



# look for negative EHB values
sum(ehb_ea_orig$EHB<0, na.rm = TRUE)
    # 4
    # same as before

# preliminary solution - cap data at 2015

ehb_ea_cap_orig <- ehb_ea_orig |>
  filter(year<2015)

# unweighted EHB mean
ehb_ea_cap_orig <- ehb_ea_cap_orig |>
  group_by(year) |>
  mutate(
    EHB_mean = mean(EHB, na.rm = TRUE),
    n = sum(!is.na(EHB))
  )

# deviation from the mean
ehb_ea_cap_orig <- ehb_ea_cap_orig |>
  mutate(EHB_dev = EHB-EHB_mean)


# crude EHB in different compositions
ehb_ea_cap_orig <- ehb_ea_cap_orig |>
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
ehb_ea_cap_orig <- ehb_ea_cap_orig |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm=TRUE),
    eq_ehb_crude_mean = mean(eq_ehb_crude, na.rm = TRUE),
    debt_ehb_crude_mean = mean(debt_ehb_crude, na.rm = TRUE),
    fdi_ehb_crude_mean = mean(fdi_ehb_crude, na.rm = TRUE),
    eq_debt_ehb_crude_mean = mean(eq_debt_ehb_crude, na.rm = TRUE)
  )

# deviation from the mean
ehb_ea_cap_orig <- ehb_ea_cap_orig |>
  mutate(
    ehb_crude_dev = ehb_crude - ehb_crude_mean,
    eq_ehb_crude_dev = eq_ehb_crude - eq_ehb_crude_mean,
    debt_ehb_crude_dev = debt_ehb_crude - debt_ehb_crude_mean,
    fdi_ehb_crude_dev = fdi_ehb_crude - fdi_ehb_crude_mean,
    eq_debt_ehb_crude_dev = eq_debt_ehb_crude - eq_debt_ehb_crude_mean
    )


ehb_ea_orig_cap_small <- ehb_ea_cap_orig |>
  select(iso,year,EHB_dev, ehb_crude_dev, eq_ehb_crude_dev,debt_ehb_crude_dev,fdi_ehb_crude_dev,eq_debt_ehb_crude_dev)

write_csv(ehb_ea_orig_cap_small, "../data/ehb_ea_orig_cap_small.csv")

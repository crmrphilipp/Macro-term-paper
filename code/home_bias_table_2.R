
# ══════════════════════════════════════════════════════════════════════════════
#  
# Home-Bias Recreation
#
# ══════════════════════════════════════════════════════════════════════════════


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
    debt_assets = `Portfolio debt assets`,
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
    debt_assets = `Portfolio debt assets`,
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




# ══════════════════════════════════════════════════════════════════════════════
# Regression Ready EHB data
# prepare the EHB data for the replication
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

# Check for EHB values larger 1
sum(ehb_reg$EHB>1)

  # gives 0 counts

# we excluded all EHB values > 1 with all of them coming from total equity values lower 0

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

ehb_reg_small <- ehb_reg |>
  select(iso,year,EHB_dev)

write_csv(ehb_reg_small, "../data/ehb_reg_small.csv")


# prepare second ehb measure for regression
ehb_crude_raw <- df_full|>
  filter(year>1992, year<2004)

# create the "crude" EHB measure using foreign equity over GDP
ehb_crude_reg <- ehb_crude_raw |>
  mutate(ehb_crude = log(eq_assets/gdp))

# unweighted mean
ehb_crude_reg <- ehb_crude_reg |>
  group_by(year) |>
  mutate(
    ehb_crude_mean = mean(ehb_crude, na.rm = TRUE),
    n = sum(!is.na(ehb_crude))
  )

# deviation from the mean
ehb_crude_reg <- ehb_crude_reg |>
  mutate(ehb_crude_dev = ehb_crude-ehb_crude_mean)

ehb_crude_reg_small <- ehb_crude_reg |>
  select(iso,year,ehb_crude_dev)

write_csv(ehb_crude_reg_small, "../data/ehb_crude_reg_small.csv")

# test data with some graphs

library(ggplot2)

# ── 1. EHB (one line per country) ─────────────────────────────────────────────
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

# ── 2. EHB_dev (deviation from yearly mean, one line per country) ─────────────
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

# ── 3. EHB_mean (one observation per year — collapse first to avoid overplotting)
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

# furhter investigation of the variables on which EHB is built
df_replication_years <- df_full %>%
  filter(year>1992, year<2004)

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

# ── 4. ehb_crude (one line per country) ───────────────────────────────────────
ggplot(ehb_crude_reg, aes(x = year, y = ehb_crude, group = iso, color = iso)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Crude EHB [log(Foreign Equity / GDP)] by Country",
    x = "Year", y = "log(eq_assets / GDP)",
    color = "Country"
  ) +
  theme_minimal()

# ── 5. ehb_crude_dev (one line per country) ───────────────────────────────────
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

# ── 6. ehb_crude_mean (one observation per year) ──────────────────────────────
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


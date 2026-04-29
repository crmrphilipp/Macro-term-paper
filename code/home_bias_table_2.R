
# ══════════════════════════════════════════════════════════════════════════════
#  
# Home-Bias Recreation
#
# ══════════════════════════════════════════════════════════════════════════════


rm(list=ls())
setwd("/Users/philippcremer/Documents/Master/M1 S2/Macro 3/home_bias_recreation")

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
ewn_file <- "data/EWN_dataset.xlsx"

dir.create("data", showWarnings = FALSE)

if (!file.exists(ewn_file)) {
  download.file(ewn_url, destfile = ewn_file, mode = "wb")
  message("EWN dataset downloaded.")
}

# Inspect sheet names
ewn_sheets <- excel_sheets(ewn_file)
print(ewn_sheets)  

# Load the main data sheet 
ewn_raw <- read_excel(ewn_file, sheet = "Dataset")


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
# Merge the two Sources
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
  footnote(general = "The rows display the value of foreign equity, debt, and foreign direct investment holdings divided by GDP in the same year.",
           general_title = "Note.",
           footnote_as_chunk = TRUE)


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
                  "Foreign equity in portfolio (\\%)", "Equity Home Bias",
                  "Foreign equity in portfolio (\\%)", "Equity Home Bias"),
    align   = c("l", "r", "r", "r", "r"),
    escape  = FALSE
  ) |>
  add_header_above(c(" " = 1, "1993" = 2, "2003" = 2)) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  footnote(
    general       = "Equity Home Bias $= 1 -$ column (1)$/[1 - A]$. Column (1) $=$ total foreign equity held by country/country's total equity portfolio, where the total equity portfolio of a country $=$ stock market capitalization $+$ foreign equity held $-$ amount of country's equity held by foreigners. $A =$ stock market capitalization of a country/stock market capitalization of the world.",
    general_title = "Note.",
    footnote_as_chunk = TRUE,
    escape        = FALSE
  )






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
           general_title = "Note.", footnote_as_chunk = TRUE)


# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2: Year-level summary — how many countries are covered each year?
# ══════════════════════════════════════════════════════════════════════════════

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
  footnote(general = "Sample is the 24 OECD countries from Sørensen et al. (2007). Coverage is out of 24 countries.",
           general_title = "Note.", footnote_as_chunk = TRUE)

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

print(extension_gaps, n = 30)

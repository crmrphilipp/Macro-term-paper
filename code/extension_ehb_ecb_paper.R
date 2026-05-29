

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



# SHS Restated Bilateral External Portfolios 
shs_url  <- "https://globalcapitalallocation.s3.us-east-2.amazonaws.com/SHS_Based_Restated_Bilateral_External_Portfolios.zip"
shs_dir  <- "../data/beck_et_al/"
shs_zip  <- paste0(shs_dir, "SHS_Restated.zip")
shs_file <- paste0(shs_dir, "SHS_Based_Restated_Bilateral_External_Portfolios.csv")

dir.create(shs_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(shs_file)) {
  download.file(shs_url, destfile = shs_zip, mode = "wb")
  unzip(shs_zip, exdir = shs_dir)
  message("SHS Restated data downloaded and unzipped.")
}

# Fund Counterparty Statistics 
fcs_url <- "https://globalcapitalallocation.s3.us-east-2.amazonaws.com/Fund_Counterparty_Statistics.zip"
fcs_zip <- paste0(shs_dir, "Fund_Counterparty_Statistics.zip")

if (!file.exists(fcs_zip)) {
  download.file(fcs_url, destfile = fcs_zip, mode = "wb")
  unzip(fcs_zip, exdir = shs_dir)
  message("Fund Counterparty Statistics downloaded and unzipped.")
}



# understand download structure
list.files("../data/", pattern = "SHS|Holders|Counterparty", recursive = TRUE)



#====================================#
##### Who onw Luxembourg Funds ? #####
#====================================#


library(haven)

# Load Fund Counterparty Statistics (Regional) 
fcs_region <- read_dta("../data/beck_et_al/Holders_of_LUX_Fund_Shares_by_Counterparty_Region.dta")

# Quick look
glimpse(fcs_region)
unique(fcs_region$counterparty_region)

# grouping of countries the graph should show
fcs_region <- fcs_region |>
  mutate(
    holder_group = case_when(
      counterparty_region == "REA"                   ~ "Rest of Euro Area",
      counterparty_region == "LUX"                   ~ "Luxembourg (domestic)",
      counterparty_region == "GBR"                   ~ "United Kingdom",
      counterparty_region == "CHE"                   ~ "Switzerland",
      counterparty_region == "USA"                   ~ "United States",
      counterparty_region == "JPN"                   ~ "Japan", 
      TRUE                                           ~ "Other non-EA"
    )
  )

# aggregate other non-EA and compute shares
fcs_grouped <- fcs_region |>
  group_by(year, holder_group) |>
  summarise(holdings = sum(holdings_of_lux_funds, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = holdings / sum(holdings)) |>
  ungroup()

# Check totals look sensible
fcs_grouped |> group_by(year) |> summarise(total = sum(holdings)) |> print(n = 25)


# Factor ordering: EA at bottom, non-EA stacked on top
group_order <- c(
  "Luxembourg (domestic)", "Rest of Euro Area",
  "Switzerland", "United Kingdom", "Japan", "United States", "Other non-EA"
)

fcs_grouped <- fcs_grouped |>
  mutate(holder_group = factor(holder_group, levels = group_order))

# Colors: warm tones for EA, cool tones for non-EA
group_colors <- c(
  "Luxembourg (domestic)" = "#B22222",   # dark red
  "Rest of Euro Area"     = "#E8713A",   # orange
  "Switzerland"           = "#6BAED6",   # light blue
  "United Kingdom"        = "#2171B5",   # medium blue
  "Japan"                 = "#08519C",   # dark blue
  "United States"         = "#012656",   # darker blue
  "Other non-EA"          = "#9ECAE1"    # pale blue
)

# Plot 1: Shares (stacked to 100%)
lux_fund_ownership_shares <- ggplot(fcs_grouped, 
       aes(x = year, y = share, fill = holder_group)) +
  geom_area() +
  scale_fill_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1998, 2021, by = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Share of Luxembourg Fund Assets",
    fill    = NULL,
    caption = "Note: Ownership shares of Luxembourg-domiciled fund shares by immediate counterparty region.\nSource: CSSF via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

# Plot 2: Absolute amounts (EUR billions)
lux_fund_ownership_levels <- ggplot(fcs_grouped, 
       aes(x = year, y = holdings, fill = holder_group)) +
  geom_area() +
  scale_fill_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1998, 2021, by = 2)) +
  labs(
    x       = "Year",
    y       = "Holdings of Luxembourg Fund Shares (EUR bn)",
    fill    = NULL,
  #  caption = "Note: Holdings of Luxembourg-domiciled fund shares by immediate counterparty region.\nSource: CSSF via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

# Save both
ggsave("../output_final/lux_fund_ownership_shares.png", 
       lux_fund_ownership_shares, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/lux_fund_ownership_levels.png", 
       lux_fund_ownership_levels, width = 7, height = 5.5, dpi = 300)


#===================================================================#
##### Who owns Luxembourg and Ireland Funds ? (shorter series) #####
#===================================================================#

# Load country-level counterparty data
fcs_country <- read_dta("../data/beck_et_al/Holders_of_IRL_and_LUX_Fund_Shares_by_Counterparty_Country.dta")

# Define EA country codes (as of ~2020)
ea_countries <- c("AUT", "BEL", "CYP", "DEU", "ESP", "EST", "FIN", "FRA",
                  "GRC", "HRV", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT",
                  "NLD", "PRT", "SVK", "SVN")

# Pivot to long format: one row per year × counterparty × fund domicile
fcs_long <- fcs_country |>
  pivot_longer(
    cols      = c(holdings_of_irl_funds, holdings_of_lux_funds),
    names_to  = "fund_domicile",
    values_to = "holdings"
  ) |>
  mutate(
    fund_domicile = if_else(fund_domicile == "holdings_of_irl_funds", 
                            "Ireland", "Luxembourg")
  ) |>
  filter(!is.na(holdings))

# Assign holder groups (same as before)
fcs_long <- fcs_long |>
  mutate(
    holder_group = case_when(
      # Domestic: counterparty matches fund domicile
      counterparty == "IRL" & fund_domicile == "Ireland"     ~ "Domestic",
      counterparty == "LUX" & fund_domicile == "Luxembourg"  ~ "Domestic",
      # Other EA (excluding the domestic OOFC)
      counterparty %in% ea_countries                         ~ "Rest of Euro Area",
      counterparty == "CHE"                                  ~ "Switzerland",
      counterparty == "GBR"                                  ~ "United Kingdom",
      counterparty == "JPN"                                  ~ "Japan",
      counterparty == "USA"                                  ~ "United States",
      TRUE                                                   ~ "Other non-EA"
    )
  )

# Aggregate and compute shares
fcs_panel <- fcs_long |>
  group_by(year, fund_domicile, holder_group) |>
  summarise(holdings = sum(holdings, na.rm = TRUE), .groups = "drop") |>
  group_by(year, fund_domicile) |>
  mutate(share = holdings / sum(holdings)) |>
  ungroup()

# Factor ordering (same logic, but "Domestic" replaces country-specific)
group_order_panel <- c(
  "Domestic", "Rest of Euro Area",
  "Switzerland", "United Kingdom", "Japan", "United States", "Other non-EA"
)

fcs_panel <- fcs_panel |>
  mutate(holder_group = factor(holder_group, levels = group_order_panel))

# ── Colors ──
group_colors_panel <- c(
  "Domestic"          = "#8B0000",
  "Rest of Euro Area" = "#E8713A",
  "Switzerland"       = "#6BAED6",
  "United Kingdom"    = "#2171B5",
  "Japan"             = "#08519C",
  "United States"     = "#012656",
  "Other non-EA"      = "#9ECAE1"
)

# Two-panel share plot
irl_lux_fund_ownership_shares <- ggplot(fcs_panel, 
       aes(x = year, y = share, fill = holder_group)) +
  geom_area() +
  facet_wrap(~ fund_domicile) +
  scale_fill_manual(values = group_colors_panel) +
  scale_x_continuous(breaks = seq(2014, 2021, by = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Share of Fund Assets",
    fill    = NULL,
    caption = "Note: Ownership shares by immediate counterparty. 'Domestic' = Irish investors for Ireland funds, Luxembourg investors for Luxembourg funds.\nSource: Central Bank of Ireland and CSSF via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    strip.text      = element_text(size = 10, face = "bold"),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("../output/irl_lux_fund_ownership_shares.png",
       irl_lux_fund_ownership_shares, width = 9, height = 4.5, dpi = 300)


# Two-panel absolute amounts plot
irl_lux_fund_ownership_levels <- ggplot(fcs_panel, 
       aes(x = year, y = holdings, fill = holder_group)) +
  geom_area() +
  facet_wrap(~ fund_domicile) +
  scale_fill_manual(values = group_colors_panel) +
  scale_x_continuous(breaks = seq(2014, 2021, by = 2)) +
  labs(
    x       = "Year",
    y       = "Holdings of Fund Shares (EUR bn)",
    fill    = NULL,
    caption = "Note: Holdings by immediate counterparty. 'Domestic' = Irish investors for Ireland funds, Luxembourg investors for Luxembourg funds.\nSource: Central Bank of Ireland and CSSF via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    strip.text      = element_text(size = 10, face = "bold"),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("../output/irl_lux_fund_ownership_levels.png",
       irl_lux_fund_ownership_levels, width = 9, height = 4.5, dpi = 300)




# Ireland-only: filter and aggregate
fcs_irl <- fcs_long |>
  filter(fund_domicile == "Ireland") |>
  group_by(year, holder_group) |>
  summarise(holdings = sum(holdings, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = holdings / sum(holdings)) |>
  ungroup() |>
  mutate(holder_group = factor(holder_group, levels = group_order_panel))

#  Share plot 
irl_fund_ownership_shares <- ggplot(fcs_irl, 
       aes(x = year, y = share, fill = holder_group)) +
  geom_area() +
  scale_fill_manual(values = group_colors_panel) +
  scale_x_continuous(breaks = seq(2014, 2021, by = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Share of Irish Fund Assets",
    fill    = NULL,
    caption = "Note: Ownership shares of Irish-domiciled fund shares by immediate counterparty country.\n'Domestic' = Irish investors. Source: Central Bank of Ireland via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("../output_final/irl_fund_ownership_shares.png",
       irl_fund_ownership_shares, width = 7, height = 5.5, dpi = 300)

#  Absolute amounts plot
irl_fund_ownership_levels <- ggplot(fcs_irl, 
       aes(x = year, y = holdings, fill = holder_group)) +
  geom_area() +
  scale_fill_manual(values = group_colors_panel) +
  scale_x_continuous(breaks = seq(2014, 2021, by = 1)) +
  labs(
    x       = "Year",
    y       = "Holdings of Irish Fund Shares (EUR bn)",
    fill    = NULL,
    #caption = "Note: Holdings of Irish-domiciled fund shares by immediate counterparty country.\n'Domestic' = Irish investors. Source: Central Bank of Ireland via Beck et al. (2024)."
  ) +
  theme_classic() +
  theme(
    axis.text       = element_text(size = 9),
    axis.title      = element_text(size = 10),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("../output_final/irl_fund_ownership_levels.png",
       irl_fund_ownership_levels, width = 7, height = 5.5, dpi = 300)


#=====================================================#
##### The Round Tripping Problem (FRA, DEU, BEL) #####
#=====================================================#

# Load SHS Bilateral Data
shs <- read_csv("../data/beck_et_al/SHS_Based_Restated_Bilateral_External_Portfolios.csv")

glimpse(shs)

## explanation of the columns ##

# `position_residency` 
    # The raw, unadjusted position. This is what standard statistics (CPIS/IIP) report. 
    # Securities are attributed to wherever they're legally domiciled and investors are counted by where they reside. 
    # So Germany holding Luxembourg fund shares shows up as DEU → LUX, and a bond issued by BMW Finance NV in the Netherlands 
    # shows up as a claim on NLD.

# `restatement_nat` 
    # Nationality-adjusted only. This corrects the issuer side: securities issued by foreign subsidiaries get reassigned 
    # to the ultimate parent's country. BMW Finance NV bonds get moved from NLD to DEU. But fund shares are left untouched,
    # if Germany holds Luxembourg fund shares, it still shows as DEU → LUX. This addresses the securities issuance role of OOFCs
    # but not the fund intermediation role.

# `restatement_uw` 
    # Fund-unwind-adjusted only. This corrects the investor side: holdings of Luxembourg and Ireland fund shares are replaced 
    # by the underlying securities the fund actually holds. If Germany owns a Luxembourg fund that holds US equities, it becomes 
    # DEU → USA instead of DEU → LUX. But securities issuance is left on a residency basis — BMW Finance bonds still count as NLD. T
    # his also creates the ROW investor rows (non-EA money extracted from the funds).

# `restatement_nat_uw`
    # Both corrections applied. Fund shares are unwound and securities are reassigned to the ultimate parent. This is the fully
    # corrected picture.

# For the reallocation / round tripping chart, restatement_uw (fund unwind only) is the most relevant comparison against 
# position_residency, because the fund intermediation channel is the main driver of the EHB distortion of the Euro Area Countries (round trippig)

# But just to check I understood correctly, we want to investigate round tripping, whether germany has reported claims against 
# Luxembourg because it owns a fund where german equity is included in luxemborug. But for this we dont care, that the securities
# on Volkswagen are actually issued by a dutch holding because its just about the fund which germans own and which is in the
# luxembourg but is based on german equity so its no real position of germany against luxembourg but against itself.
# the correction also reconfugres external equity holdings to the true country like USA or france
# but (while interesting) this is not what causes the bias in the EHB data

# Filter to equity positions, pick a reference year
shs_equity <- shs |>
  filter(asset_class == "Equity (All)")

#### This first function shows the reallocation but not the round tripping ####
# we cannot look at the absolute comparison and the round tripping at the same time becaue the DEU-DEU position is huge and would completely destry the chart

# Function to prepare reallocation data for one country 
prepare_reallocation <- function(data, investor_iso, ref_year = "2018q4", top_n = 10) {
  
  df <- data |>
    filter(investor == investor_iso, date_q == ref_year, issuer != investor_iso) |>
    select(issuer, position_residency, restatement_uw) |>
    # Get top destinations by EITHER measure (so we catch the ones that appear/disappear)
    mutate(max_pos = pmax(position_residency, restatement_uw, na.rm = TRUE)) |>
    slice_max(max_pos, n = top_n) |>
    select(-max_pos) |>
    pivot_longer(
      cols      = c(position_residency, restatement_uw),
      names_to  = "method",
      values_to = "position"
    ) |>
    mutate(
      method = if_else(method == "position_residency", 
                       "Residency-based", "Fund-unwind adjusted"),
      method = factor(method, levels = c("Residency-based", "Fund-unwind adjusted"))
    )
  
  # Order issuers by residency-based position (descending)
  issuer_order <- df |>
    filter(method == "Residency-based") |>
    arrange(desc(position)) |>
    pull(issuer)
  
  df |> mutate(issuer = factor(issuer, levels = rev(issuer_order)))
}

# Plotting function
plot_reallocation <- function(data, investor_iso, ref_year = "2018q4", top_n = 10) {
  
  df <- prepare_reallocation(data, investor_iso, ref_year, top_n)
  
  ggplot(df, aes(x = issuer, y = position, fill = method)) +
    geom_col(position = "dodge", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Residency-based"       = "#8B0000",
                                 "Fund-unwind adjusted"   = "#08519C")) +
    labs(
      x       = NULL,
      y       = "Equity Position (EUR bn)",
      fill    = NULL,
    #  caption = paste0(
    #    "Note: Top ", top_n, " foreign equity destinations for ", investor_iso,
    #    " (", ref_year, "). Residency-based vs. fund-unwind adjusted.\n",
    #    "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
    #  )
    ) +
    theme_classic() +
    theme(
      axis.text       = element_text(size = 9),
      axis.title      = element_text(size = 10),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

# Generate for all three countries
realloc_deu <- plot_reallocation(shs_equity, "DEU")
realloc_fra <- plot_reallocation(shs_equity, "FRA")
realloc_bel <- plot_reallocation(shs_equity, "BEL")
realloc_ita <- plot_reallocation(shs_equity, "ITA")


ggsave("../output_final/reallocation_deu.png", realloc_deu, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/reallocation_fra.png", realloc_fra, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/reallocation_bel.png", realloc_bel, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/reallocation_ita.png", realloc_ita, width = 7, height = 5.5, dpi = 300)


#### now we go to the round trippng part by looking at changes of the position ####
# we look at changes to see the extent of round tripping

# Function to prepare net reallocation data
prepare_net_reallocation <- function(data, investor_iso, ref_year = "2018q4", top_n = 12) {
  
  df <- data |>
    filter(investor == investor_iso, date_q == ref_year) |>
    select(issuer, position_residency, restatement_uw) |>
    mutate(
      delta = restatement_uw - position_residency,
      # Label domestic position explicitly
      issuer_label = if_else(issuer == investor_iso, 
                             paste0(issuer, " (domestic)"), issuer)
    ) |>
    # Keep top destinations by absolute change
    slice_max(abs(delta), n = top_n) |>
    arrange(delta)
  
  df |> mutate(issuer_label = factor(issuer_label, levels = issuer_label))
}

# Plotting function
plot_net_reallocation <- function(data, investor_iso, ref_year = "2018q4", top_n = 12) {
  
  df <- prepare_net_reallocation(data, investor_iso, ref_year, top_n)
  
  ggplot(df, aes(x = issuer_label, y = delta, 
                 fill = if_else(delta >= 0, "Gains exposure", "Loses exposure"))) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Loses exposure" = "#8B0000",
                                 "Gains exposure"  = "#08519C")) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    labs(
      x       = NULL,
      y       = "Change in Equity Position (EUR bn)",
      fill    = NULL,
     # caption = paste0(
     #   "Note: Difference between fund-unwind adjusted and residency-based equity positions for ", 
     #   investor_iso, " (", ref_year, ").\n",
     #   "Negative = position shrinks after correction; positive = position grows. ",
     #   "'(domestic)' captures round-tripping.\n",
     #   "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
     # )
    ) +
    theme_classic() +
    theme(
      axis.text       = element_text(size = 9),
      axis.title      = element_text(size = 10),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      plot.caption    = element_text(hjust = 0, size = 8, color = "grey30"),
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

# Generate for all three countries
net_realloc_deu <- plot_net_reallocation(shs_equity, "DEU")
net_realloc_fra <- plot_net_reallocation(shs_equity, "FRA")
net_realloc_bel <- plot_net_reallocation(shs_equity, "BEL")
net_realloc_ita <- plot_net_reallocation(shs_equity, "ITA")


ggsave("../output_final/net_reallocation_deu.png", net_realloc_deu, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/net_reallocation_fra.png", net_realloc_fra, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/net_reallocation_bel.png", net_realloc_bel, width = 7, height = 5.5, dpi = 300)
ggsave("../output_final/net_reallocation_ita.png", net_realloc_bel, width = 7, height = 5.5, dpi = 300)


#=====================================================#
 ##### Foreign Portfolio Change EA and countries #####
#=====================================================#


# Compute foreign vs domestic equity for each EA country-year 
ea_investors <- c("AUT", "BEL", "CYP", "DEU", "ESP", "EST", "FIN", "FRA",
                  "GRC", "HRV", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT",
                  "NLD", "PRT", "SVK", "SVN")

# OOFCs to exclude from the average (they distort it mechanically)
oofcs <- c("LUX", "IRL", "NLD")

portfolio_shares <- shs_equity |>
  filter(investor %in% ea_investors) |>
  mutate(is_domestic = (investor == issuer)) |>
  group_by(date_q, investor, is_domestic) |>
  summarise(
    pos_resid = sum(position_residency, na.rm = TRUE),
    pos_uw    = sum(restatement_uw, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  pivot_wider(
    names_from  = is_domestic,
    values_from = c(pos_resid, pos_uw),
    names_glue  = "{.value}_{ifelse(is_domestic, 'dom', 'for')}"
  ) |>
  mutate(
    omega_resid = pos_resid_for / (pos_resid_for + pos_resid_dom),
    omega_uw    = pos_uw_for    / (pos_uw_for    + pos_uw_dom),
    year        = as.integer(substr(date_q, 1, 4))
  )

# Slovakia has a negative share in 2019 coming from a negative value in pos_uw_for for that year
# this is an error from the downloaded dtaset
# we set the observation NA
portfolio_shares <- portfolio_shares |>
  mutate(omega_uw = if_else(investor == "SVK" & year == 2019, NA_real_, omega_uw))

# EA average, unweighted (excluding OOFCs)
ea_avg <- portfolio_shares |>
  filter(!investor %in% oofcs) |>
  group_by(year) |>
  summarise(
    omega_resid = mean(omega_resid, na.rm = TRUE),
    omega_uw    = mean(omega_uw,    na.rm = TRUE),
    .groups     = "drop"
  )

# Plot 1: EA average with shaded gap
foreign_share_ea_avg <- ggplot(ea_avg, aes(x = year)) +
  geom_ribbon(aes(ymin = omega_uw, ymax = omega_resid), 
              fill = "#E8713A", alpha = 0.25) +
  geom_line(aes(y = omega_resid, color = "Residency-based"), linewidth = 0.8) +
  geom_point(aes(y = omega_resid, color = "Residency-based"), size = 2) +
  geom_line(aes(y = omega_uw, color = "Fund-unwind adjusted"), linewidth = 0.8) +
  geom_point(aes(y = omega_uw, color = "Fund-unwind adjusted"), size = 2) +
  scale_color_manual(values = c("Residency-based"     = "#8B0000",
                                "Fund-unwind adjusted" = "#08519C")) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Foreign Equity Portfolio Share",
    color   = NULL,
    caption = paste0(
      "Note: Unweighted mean across EA countries excluding Luxembourg, Ireland, and the Netherlands.\n",
      "Foreign share = foreign equity / (foreign + domestic equity). ",
      "Shaded area shows overstatement due to fund intermediation.\n",
      "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
    )
  ) +
  theme_classic() +
  theme(
    axis.text        = element_text(size = 9),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("../output_final/foreign_share_ea_avg.png", 
       foreign_share_ea_avg, width = 7, height = 4.5, dpi = 300)

# Plot 2: Country-level panels
country_data <- portfolio_shares |>
  filter(!investor %in% oofcs) |>
  pivot_longer(
    cols      = c(omega_resid, omega_uw),
    names_to  = "method",
    values_to = "omega"
  ) |>
  mutate(
    method = if_else(method == "omega_resid", 
                     "Residency-based", "Fund-unwind adjusted"),
    method = factor(method, levels = c("Residency-based", "Fund-unwind adjusted"))
  )

foreign_share_by_country <- ggplot(country_data, 
       aes(x = year, y = omega, color = method)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.2) +
  facet_wrap(~ investor, scales = "free_y") +
  scale_color_manual(values = c("Residency-based"     = "#8B0000",
                                "Fund-unwind adjusted" = "#08519C")) +
  scale_x_continuous(breaks = c(2015, 2019, 2023)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Foreign Equity Portfolio Share",
    color   = NULL,
    caption = paste0(
      "Note: Foreign share = foreign equity / (foreign + domestic equity). ",
      "EA countries excluding Luxembourg, Ireland, and the Netherlands.\n",
      "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
    )
  ) +
  theme_classic() +
  theme(
    axis.text        = element_text(size = 7),
    axis.title       = element_text(size = 9),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    strip.text       = element_text(size = 8, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 7, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("../output_final/foreign_share_by_country.png",
       foreign_share_by_country, width = 10, height = 7, dpi = 300)



# EA aggregate: treat the Euro Area as one entity
ea_aggregate <- shs_equity |>
  filter(investor %in% ea_investors, investor != "ROW") |>
  mutate(
    is_ea_issuer = issuer %in% ea_investors,
    year         = as.integer(substr(date_q, 1, 4))
  ) |>
  group_by(year, is_ea_issuer) |>
  summarise(
    pos_resid = sum(position_residency, na.rm = TRUE),
    pos_uw    = sum(restatement_uw, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  pivot_wider(
    names_from  = is_ea_issuer,
    values_from = c(pos_resid, pos_uw),
    names_glue  = "{.value}_{ifelse(is_ea_issuer, 'ea', 'non_ea')}"
  ) |>
  mutate(
    omega_resid = pos_resid_non_ea / (pos_resid_non_ea + pos_resid_ea),
    omega_uw    = pos_uw_non_ea    / (pos_uw_non_ea    + pos_uw_ea)
  )

# Plot: EA aggregate foreign equity share
foreign_share_ea_aggregate <- ggplot(ea_aggregate, aes(x = year)) +
  geom_ribbon(aes(ymin = omega_uw, ymax = omega_resid), 
              fill = "#E8713A", alpha = 0.25) +
  geom_line(aes(y = omega_resid, color = "Residency-based"), linewidth = 0.8) +
  geom_point(aes(y = omega_resid, color = "Residency-based"), size = 2) +
  geom_line(aes(y = omega_uw, color = "Fund-unwind adjusted"), linewidth = 0.8) +
  geom_point(aes(y = omega_uw, color = "Fund-unwind adjusted"), size = 2) +
  scale_color_manual(values = c("Residency-based"      = "#8B0000",
                                "Fund-unwind adjusted"  = "#08519C")) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Foreign Equity Portfolio Share",
    color   = NULL,
    caption = paste0(
      "Note: Euro Area treated as a single entity. Intra-EA holdings counted as domestic.\n",
      "Foreign share = EA holdings of non-EA equity / total EA equity holdings. ",
      "Shaded area shows overstatement due to fund intermediation.\n",
      "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
    )
  ) +
  theme_classic() +
  theme(
    axis.text        = element_text(size = 9),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("../output_final/foreign_share_ea_aggregate.png",
       foreign_share_ea_aggregate, width = 7, height = 4.5, dpi = 300)



# Portfolio-weighted average INCLUDING OOFCs
ea_weighted_avg_all <- portfolio_shares |>
  mutate(
    total_resid = pos_resid_for + pos_resid_dom,
    total_uw    = pos_uw_for    + pos_uw_dom
  ) |>
  group_by(year) |>
  summarise(
    omega_resid = weighted.mean(omega_resid, w = total_resid, na.rm = TRUE),
    omega_uw    = weighted.mean(omega_uw,    w = total_uw,    na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(series = "EA average (portfolio-weighted)")

# Combine with aggregate
ea_comparison <- ea_aggregate |>
  select(year, omega_resid, omega_uw) |>
  mutate(series = "EA aggregate") |>
  bind_rows(ea_weighted_avg_all)

# Residency-based ex-OOFC reference
ea_weighted_avg_ex_oofc <- portfolio_shares |>
  filter(!investor %in% oofcs) |>
  mutate(total_resid = pos_resid_for + pos_resid_dom) |>
  group_by(year) |>
  summarise(
    omega_resid_ex_oofc = weighted.mean(omega_resid, w = total_resid, na.rm = TRUE),
    .groups = "drop"
  )

ea_comparison <- ea_comparison |>
  select(-any_of("omega_uw_ex_oofc")) |>
  left_join(ea_weighted_avg_ex_oofc, by = "year")

ea_comparison <- ea_comparison |>
  mutate(omega_resid_ex_oofc = if_else(series == "EA aggregate", 
                                        NA_real_, omega_resid_ex_oofc))

# ── Fund-unwind adjusted, ex-OOFCs ──
ea_weighted_uw_ex_oofc <- portfolio_shares |>
  filter(!investor %in% oofcs) |>
  mutate(total_uw = pos_uw_for + pos_uw_dom) |>
  group_by(year) |>
  summarise(
    omega_uw_ex_oofc = weighted.mean(omega_uw, w = total_uw, na.rm = TRUE),
    .groups = "drop"
  )

ea_comparison <- ea_comparison |>
  select(-any_of("omega_uw_ex_oofc")) |>
  left_join(ea_weighted_uw_ex_oofc, by = "year") |>
  mutate(omega_uw_ex_oofc = if_else(series == "EA aggregate", 
                                     NA_real_, omega_uw_ex_oofc))

# comparison plot with grey reference line
foreign_share_ea_comparison <- ggplot(ea_comparison, aes(x = year)) +
  geom_ribbon(aes(ymin = omega_uw, ymax = omega_resid), 
              fill = "#E8713A", alpha = 0.2) +
  geom_line(aes(y = omega_resid, color = "Residency-based"), linewidth = 0.8) +
  geom_point(aes(y = omega_resid, color = "Residency-based"), size = 2) +
  geom_line(aes(y = omega_resid_ex_oofc, color = "Residency-based (ex. OOFCs)"), 
            linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(y = omega_resid_ex_oofc, color = "Residency-based (ex. OOFCs)"), 
             size = 1.5, shape = 1) +
  geom_line(aes(y = omega_uw_ex_oofc, color = "Fund-unwind adj. (ex. OOFCs)"), 
            linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(y = omega_uw_ex_oofc, color = "Fund-unwind adj. (ex. OOFCs)"), 
             size = 1.5, shape = 1) +
  geom_line(aes(y = omega_uw, color = "Fund-unwind adjusted"), linewidth = 0.8) +
  geom_point(aes(y = omega_uw, color = "Fund-unwind adjusted"), size = 2) +
  facet_wrap(~ series) +
  scale_color_manual(values = c("Residency-based"              = "#8B0000",
                                "Fund-unwind adjusted"          = "#08519C",
                                "Residency-based (ex. OOFCs)"   = "grey50",
                                "Fund-unwind adj. (ex. OOFCs)"  = "black"
                              )
                              ) +
  scale_x_continuous(breaks = c(2015, 2019, 2023)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Year",
    y       = "Foreign Equity Portfolio Share",
    color   = NULL,
   # caption = paste0(
   #   "Note: Left panel — EA as single entity. Right panel — portfolio-weighted average.\n",
   #   "Solid lines include all EA countries; dashed lines exclude LUX, IRL, NLD.\n",
   #   "Source: Beck et al. (2024), SHS-based restated bilateral portfolios."
   # )
  ) +
  theme_classic() +
  theme(
    axis.text        = element_text(size = 9),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),
    strip.text       = element_text(size = 9, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 7, color = "grey30"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("../output_final/foreign_share_ea_comparison.png",
       foreign_share_ea_comparison, width = 9, height = 3.5, dpi = 300)

#ggsave("../output_final/foreign_share_ea_comparison.png",
#       foreign_share_ea_comparison, width = 9, height = 9, dpi = 300)




#  Recompute with full correction (nationality + fund unwind) 
portfolio_shares_natuw <- shs_equity |>
  filter(investor %in% ea_investors) |>
  mutate(is_domestic = (investor == issuer)) |>
  group_by(date_q, investor, is_domestic) |>
  summarise(
    pos_natuw = sum(restatement_nat_uw, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  pivot_wider(
    names_from  = is_domestic,
    values_from = pos_natuw,
    names_glue  = "pos_natuw_{ifelse(is_domestic, 'dom', 'for')}"
  ) |>
  mutate(
    omega_natuw = pos_natuw_for / (pos_natuw_for + pos_natuw_dom),
    year        = as.integer(substr(date_q, 1, 4))
  )

# ── Quick comparison: EA aggregate ──
ea_agg_natuw <- shs_equity |>
  filter(investor %in% ea_investors, investor != "ROW") |>
  mutate(is_ea_issuer = issuer %in% ea_investors,
         year = as.integer(substr(date_q, 1, 4))) |>
  group_by(year, is_ea_issuer) |>
  summarise(
    pos_uw    = sum(restatement_uw, na.rm = TRUE),
    pos_natuw = sum(restatement_nat_uw, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  pivot_wider(
    names_from  = is_ea_issuer,
    values_from = c(pos_uw, pos_natuw),
    names_glue  = "{.value}_{ifelse(is_ea_issuer, 'ea', 'non_ea')}"
  ) |>
  mutate(
    omega_uw    = pos_uw_non_ea    / (pos_uw_non_ea    + pos_uw_ea),
    omega_natuw = pos_natuw_non_ea / (pos_natuw_non_ea + pos_natuw_ea)
  )

# ── Print side by side ──
ea_agg_natuw |> select(year, omega_uw, omega_natuw) |> print(n = 12)

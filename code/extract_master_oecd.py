"""
Build one consolidated CSV with all OECD 2005 data for the
Sørensen–Wu–Yosha–Zhu (2007) replication.

Output: data/oecd_master.csv  (24 countries × 12 years = 288 rows)

Columns:
  iso, country, year
  gdp_nc       – GDP, constant 2000 prices, national currency  (T1 row 54)
  cons_nc      – Final consumption expenditure, const 2000, NC (T1 row 28)
  gni_nc       – Real GNI, const 2000, NC                      (T4 row 29)
  gni_approx   – 1 if gni_nc is deflator-approximated, 0 if direct from T4
  gdp_a3       – GDP A.3: 2000 exchange rates, bn USD
  acons_a4     – Actual individual consumption A.4: 2000 XR, bn USD
  gdp_b3       – GDP B.3: 2000 PPP, bn USD
  acons_b4     – Actual individual consumption B.4: 2000 PPP, bn USD
  xr_c1        – Exchange rate C.1: NC per USD (annual average)
  ppp_gdp_c2   – PPP for GDP C.2: NC per USD
  ppp_cons_c3  – PPP for actual indiv. cons C.3: NC per USD
  pop_c4       – Population C.4: mid-year, thousands

Unit note for NC columns:
  Billions of national currency : JPN, USA
  Millions of national currency : all other 22 countries
"""

import re, csv
from pathlib import Path

# ── 1. Country mappings ───────────────────────────────────────────────────────
ISO_NAMES = {
    "AUS": "Australia",    "AUT": "Austria",       "BEL": "Belgium",
    "CAN": "Canada",       "DNK": "Denmark",       "FIN": "Finland",
    "FRA": "France",       "DEU": "Germany",       "GRC": "Greece",
    "ISL": "Iceland",      "IRL": "Ireland",       "ITA": "Italy",
    "JPN": "Japan",        "MEX": "Mexico",        "NLD": "Netherlands",
    "NZL": "New Zealand",  "NOR": "Norway",        "PRT": "Portugal",
    "ESP": "Spain",        "SWE": "Sweden",        "CHE": "Switzerland",
    "TUR": "Türkiye",      "GBR": "United Kingdom","USA": "United States",
}
TARGET_ISO = set(ISO_NAMES)

COUNTRIES_FR = {
    "Australie":       "AUS", "Autriche":        "AUT", "Belgique":   "BEL",
    "Canada":          "CAN", "Danemark":        "DNK", "Finlande":   "FIN",
    "France":          "FRA", "Allemagne":       "DEU", "Grèce":      "GRC",
    "Islande":         "ISL", "Irlande":         "IRL", "Italie":     "ITA",
    "Japon":           "JPN", "Mexique":         "MEX", "Pays-Bas":   "NLD",
    "Nouvelle-Zélande":"NZL", "Norvège":         "NOR", "Portugal":   "PRT",
    "Espagne":         "ESP", "Suède":           "SWE", "Suisse":     "CHE",
    "Turquie":         "TUR", "Royaume-Uni":     "GBR", "États-Unis": "USA",
}
SORTED_FR = sorted(COUNTRIES_FR.items(), key=lambda x: -len(x[0]))

FR_YEARS = list(range(1992, 2004))   # 12 years on each French comparative page

# ── 2. Table header patterns ──────────────────────────────────────────────────
# IMPORTANT: Use specific keywords so TOC entries like "A.4 At the price levels"
# and other tables (A.1, A.5, B.5...) do NOT accidentally trigger the wrong slot.
TABLE_PATS = [
    ("A3", re.compile(r'^A\.3\s+(Gross|Produit)\b')),
    ("A4", re.compile(r'^A\.4\s+(Actual|Consommation)\b')),
    ("B3", re.compile(r'^B\.3\s+(Gross|Produit)\b')),
    ("B4", re.compile(r'^B\.4\s+(Actual|Consommation)\b')),
    ("C1", re.compile(r'^C\.1\s+(Exchange|Taux)\b')),
    ("C2", re.compile(r'^C\.2\s+(Purchasing|Par)', re.UNICODE)),
    ("C3", re.compile(r'^C\.3\s+(Purchasing|Par)', re.UNICODE)),
    ("C4", re.compile(r'^C\.4\s+Population\b')),
]
# Any "X.N <word>" line that doesn't match TABLE_PATS → unrecognised table →
# reset current_table so its rows are never stored.
ANY_TABLE_PAT = re.compile(r'^[A-Z]\.\d+\s+\S')

FR_YR_PAT = re.compile(r'\b1992\b.*\b1993\b')
EN_YR_PAT = re.compile(r'\b1970\b.*\b1981\b')


# ── 3. Value parsing ──────────────────────────────────────────────────────────
def parse_12(s):
    """Parse exactly 12 values from a multi-column number string.
    Columns separated by 2+ spaces; single space = thousands separator.
    '..' / '-' / '' → None;  '*' = estimate mark stripped.
    Returns list of 12 float/None, or None if fewer than 12 parts.
    """
    parts = re.split(r'  +', s.strip())
    if len(parts) < 12:
        return None
    out = []
    for p in parts[:12]:
        p = p.strip().replace('*', '').replace(' ', '')
        if p in ('..', '-', ''):
            out.append(None)
        else:
            try:
                out.append(float(p))
            except ValueError:
                out.append(None)
    return out


def try_parse_fr_row(line):
    """Parse French-format row: '  val1  val2 ... val12  CountryName'
    Returns (iso, [12 values]) or None.
    """
    line = line.rstrip()
    for name, iso in SORTED_FR:
        if line.endswith(name):
            prefix = line[: -len(name)].rstrip()
            if not prefix:
                continue
            vals = parse_12(prefix)
            if vals is not None:
                return iso, vals
    return None


def safe_float(s):
    try:
        v = s.strip() if isinstance(s, str) else str(s)
        return float(v) if v not in ("", "None") else None
    except (ValueError, TypeError, AttributeError):
        return None


# ── 4. Parse comparative tables from layout text ──────────────────────────────
txt   = Path("data/oecd2005_layout.txt").read_text(encoding="utf-8", errors="replace")
pages = txt.split("\x0c")
print(f"Total pages in layout: {len(pages)}")

# comp[table_key][iso][year] = value
comp = {tbl: {iso: {} for iso in TARGET_ISO} for tbl, _ in TABLE_PATS}

current_table = None
current_lang  = None   # 'fr' | 'en' | None

for page in pages:
    for line in page.split("\n"):
        stripped = line.strip()

        # Update current table when a table header line is encountered.
        # If the line looks like a table header (X.N keyword…) but matches none
        # of our target tables, reset to None so stray rows are never collected.
        if ANY_TABLE_PAT.match(stripped):
            current_table = None          # default: unrecognised table
            for tbl, pat in TABLE_PATS:
                if pat.match(stripped):
                    current_table = tbl
                    break

        # Update language from year-header lines
        if FR_YR_PAT.search(line):
            current_lang = "fr"
        elif EN_YR_PAT.search(line):
            current_lang = "en"

        # Collect data only from French-page data rows
        if current_table is None or current_lang != "fr":
            continue

        result = try_parse_fr_row(line)
        if result is None:
            continue
        iso, vals = result
        if iso not in TARGET_ISO:
            continue
        for yr, v in zip(FR_YEARS, vals):
            if v is not None:
                comp[current_table][iso][yr] = v


# ── 5. Coverage QC ────────────────────────────────────────────────────────────
print("\nCoverage per comparative table (target = 288 obs per table):")
for tbl, _ in TABLE_PATS:
    n = sum(1 for iso in TARGET_ISO for yr in FR_YEARS
            if comp[tbl][iso].get(yr) is not None)
    missing = [(iso, yr) for iso in sorted(TARGET_ISO) for yr in FR_YEARS
               if comp[tbl][iso].get(yr) is None]
    status = "OK" if not missing else f"MISSING {len(missing)}"
    print(f"  {tbl}: {n:3d}/288  {status}", end="")
    if missing:
        shown = missing[:5]
        for iso, yr in shown:
            print(f"  {iso}-{yr}", end="")
        if len(missing) > 5:
            print(f"  (+{len(missing)-5})", end="")
    print()

# ── 6. Spot checks ────────────────────────────────────────────────────────────
print("\nSpot checks (against PDF values):")
checks = [
    ("A3", "AUS", 2000, 387.5),
    ("A3", "USA", 2000, 9764.8),
    ("A3", "JPN", 2000, 4746.1),
    ("A3", "TUR", 2000, 199.3),
    ("A4", "USA", 2000, 7327.7),
    ("A4", "AUS", 2000, 272.7),
    ("B3", "DEU", 2000, 2068.9),
    ("B3", "JPN", 2000, 3308.6),
    ("B4", "FRA", 2000, 1103.9),
    ("B4", "USA", 2000, 7327.7),   # USA: PPP=XR so B4=A4
    ("C1", "JPN", 2000, 107.77),
    ("C1", "TUR", 2000, 625218.0),
    ("C2", "USA", 2000, 1.0),
    ("C2", "JPN", 2000, 155.0),    # approximate
    ("C3", "GBR", 2000, 0.607),
    ("C4", "AUS", 2000, 19282.0),
    ("C4", "USA", 2000, 282425.0),
]
for tbl, iso, yr, expected in checks:
    got = comp[tbl][iso].get(yr)
    if got is None:
        print(f"  {tbl} {iso} {yr}: MISSING (expected {expected})")
    else:
        ratio = abs(got - expected) / (abs(expected) + 1e-9)
        ok = ratio < 0.01
        print(f"  {tbl} {iso} {yr}: got={got:<12}  expected={expected:<12}  {'OK' if ok else f'DIFF {ratio:.2%}'}")


# ── 7. Load country-table data from existing CSVs ─────────────────────────────
print("\nLoading country-table (Part I) data from existing CSVs...")

ct = {iso: {yr: {} for yr in FR_YEARS} for iso in TARGET_ISO}

with open("data/oecd2005_extracted.csv") as f:
    for row in csv.DictReader(f):
        iso = row["iso"]
        yr  = int(row["year"])
        if iso not in TARGET_ISO or yr not in FR_YEARS:
            continue
        ct[iso][yr]["gdp_nc"]  = safe_float(row.get("gdp_const2000"))
        ct[iso][yr]["cons_nc"] = safe_float(row.get("cons_const2000"))

with open("data/gni_extracted.csv") as f:
    for row in csv.DictReader(f):
        iso = row["iso"]
        yr  = int(row["year"])
        if iso not in TARGET_ISO or yr not in FR_YEARS:
            continue
        gni_nc  = safe_float(row.get("gni_real"))
        gni_t4  = safe_float(row.get("gni_real_t4"))
        ct[iso][yr]["gni_nc"]     = gni_nc
        ct[iso][yr]["gni_approx"] = (0 if gni_t4 is not None
                                     else (1 if gni_nc is not None else None))

# Quick country-table coverage check
missing_ct = [(iso, yr, fld)
              for iso in sorted(TARGET_ISO)
              for yr in FR_YEARS
              for fld in ("gdp_nc", "cons_nc", "gni_nc")
              if ct[iso][yr].get(fld) is None]
if missing_ct:
    print(f"  Country-table missing: {len(missing_ct)} cells")
    for iso, yr, fld in missing_ct[:10]:
        print(f"    {iso} {yr} {fld}")
else:
    print("  Country-table data: fully complete")


# ── 8. Write master CSV ───────────────────────────────────────────────────────
FIELDS = [
    "iso", "country", "year",
    "gdp_nc", "cons_nc", "gni_nc", "gni_approx",
    "gdp_a3", "acons_a4",
    "gdp_b3", "acons_b4",
    "xr_c1", "ppp_gdp_c2", "ppp_cons_c3",
    "pop_c4",
]

out_path = Path("data/oecd_master.csv")
with open(out_path, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=FIELDS)
    w.writeheader()
    for iso in sorted(TARGET_ISO):
        for yr in FR_YEARS:
            c = ct[iso][yr]
            w.writerow({
                "iso":         iso,
                "country":     ISO_NAMES[iso],
                "year":        yr,
                "gdp_nc":      c.get("gdp_nc"),
                "cons_nc":     c.get("cons_nc"),
                "gni_nc":      c.get("gni_nc"),
                "gni_approx":  c.get("gni_approx"),
                "gdp_a3":      comp["A3"][iso].get(yr),
                "acons_a4":    comp["A4"][iso].get(yr),
                "gdp_b3":      comp["B3"][iso].get(yr),
                "acons_b4":    comp["B4"][iso].get(yr),
                "xr_c1":       comp["C1"][iso].get(yr),
                "ppp_gdp_c2":  comp["C2"][iso].get(yr),
                "ppp_cons_c3": comp["C3"][iso].get(yr),
                "pop_c4":      comp["C4"][iso].get(yr),
            })

print(f"\nWritten: {out_path}  ({24*12} rows)")


# ── 9. Cross-validation: GDP from country table vs GDP from A.3 ───────────────
# Expected relationship (constant 2000 prices by definition of A.3):
#   For countries in MILLIONS NC : gdp_a3 [bn USD] = gdp_nc [mn NC] / xr_2000 / 1000
#   For JPN, USA in BILLIONS NC  : gdp_a3 [bn USD] = gdp_nc [bn NC] / xr_2000
#
# We use the ANNUAL exchange rate C.1 as a proxy.  Because A.3 uses the YEAR-2000
# exchange rate (fixed) rather than the annual average, the cross-check is exact
# only in year 2000 itself; nearby years will differ.

print("\nCross-validation: gdp_nc / xr_c1 ≈ gdp_a3  (exact in 2000; proxy otherwise)")
print(f"  {'ISO':<5} {'Year':<6} {'gdp_nc/xr→bnUSD':>18} {'gdp_a3':>10} {'ratio':>8}")

BILLION_NC = {"JPN", "USA", "TUR"}   # these NC columns are in billions of NC

with open(out_path) as f:
    rows_2000 = [r for r in csv.DictReader(f) if int(r["year"]) == 2000]

for row in rows_2000:
    iso = row["iso"]
    try:
        gdp_nc = float(row["gdp_nc"])
        gdp_a3 = float(row["gdp_a3"])
        xr     = float(row["xr_c1"])
        if iso in BILLION_NC:
            implied = gdp_nc / xr           # bn NC / (NC/USD) = bn USD
        else:
            implied = gdp_nc / xr / 1000    # mn NC / (NC/USD) / 1000 = bn USD
        ratio = implied / gdp_a3
        flag = "" if abs(ratio - 1) < 0.02 else "  <-- CHECK"
        print(f"  {iso:<5} 2000   {implied:>18.1f} {gdp_a3:>10.1f} {ratio:>8.4f}{flag}")
    except (ValueError, TypeError, ZeroDivisionError) as e:
        print(f"  {iso:<5} 2000   ERROR: {e}")

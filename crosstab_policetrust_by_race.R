# ============================================================
# Weighted crosstab: Police trust x Race/ethnicity
# Data: Baltimore Area Survey 2023
# Weight column: bas23_svy_fwgt
# ============================================================

library(survey)

# ----------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------
load("data/bas-2023/baltimore-area-survey-2023.Rdata")
# Loads object: bas23 (1,352 rows x 148 columns)

# ----------------------------------------------------------
# 2. Remove rows with missing values in either variable
# ----------------------------------------------------------
bas23_clean <- bas23[
  !bas23[["bas23_con_plctrust"]]  %in% c("Missing: Item non-response") &
  !bas23[["bas23_dem_raceeth4"]]  %in% c(
    "Missing: Item non-response",
    "Missing: Could not be constructed from available information"
  ),
]

cat("Rows after removing missing values:", nrow(bas23_clean), "\n")

# ----------------------------------------------------------
# 3. Set up the survey design
# ----------------------------------------------------------
design <- svydesign(
  ids     = ~1,
  weights = ~bas23_svy_fwgt,
  data    = bas23_clean
)

# ----------------------------------------------------------
# 4. Build the weighted crosstab
# ----------------------------------------------------------
weighted_counts <- svytable(
  ~bas23_con_plctrust + bas23_dem_raceeth4,
  design = design
)

# ----------------------------------------------------------
# 5. Convert to column percentages
#    Column % = within each race group, what share reports
#    each level of police trust?
# ----------------------------------------------------------
col_pct <- prop.table(weighted_counts, margin = 2) * 100

# ----------------------------------------------------------
# 6. Order trust categories from most to least trusting
# ----------------------------------------------------------
trust_order <- c(
  "Trust completely",
  "Somewhat trust",
  "Neither trust nor distrust",
  "Somewhat distrust",
  "Distrust completely"
)

race_order <- c("Black", "White", "Other race", "Latinx")

col_pct_ordered  <- col_pct[trust_order, race_order]

# ----------------------------------------------------------
# 7. Print results
# ----------------------------------------------------------
cat("\n=== Weighted crosstab: Police trust x Race/ethnicity ===\n")
cat("(Column percentages — % within each race group)\n\n")
print(round(col_pct_ordered, 1))

cat("\n=== Weighted population counts (for reference) ===\n")
print(round(weighted_counts[trust_order, race_order], 0))

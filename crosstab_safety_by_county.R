# ============================================================
# Weighted crosstab: Neighborhood safety perception x County
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
#    bas23_svy_cty has no missing values, so we only need
#    to filter the safety question.
# ----------------------------------------------------------
bas23_clean <- bas23[
  !bas23[["bas23_nhd_safe"]] %in% c("Missing: Item non-response"),
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
  ~bas23_nhd_safe + bas23_svy_cty,
  design = design
)

# ----------------------------------------------------------
# 5. Convert to column percentages
#    Column % = within each county, what share reports
#    each safety level?
# ----------------------------------------------------------
col_pct <- prop.table(weighted_counts, margin = 2) * 100

# ----------------------------------------------------------
# 6. Order safety categories from most to least satisfied
# ----------------------------------------------------------
safety_order <- c(
  "Very satisfied",
  "Somewhat satisfied",
  "Somewhat dissatisfied",
  "Very dissatisfied"
)
col_pct_ordered <- col_pct[safety_order, ]

# ----------------------------------------------------------
# 7. Print results
# ----------------------------------------------------------
cat("\n=== Weighted crosstab: Neighborhood safety x County ===\n")
cat("(Column percentages — % within each county)\n\n")
print(round(col_pct_ordered, 1))

cat("\n=== Weighted population counts (for reference) ===\n")
print(round(weighted_counts[safety_order, ], 0))

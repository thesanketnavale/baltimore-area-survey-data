# ============================================================
# Weighted crosstab: Self-rated health x Race/ethnicity
# Data: Baltimore Area Survey 2023
# Weight column: bas23_svy_fwgt
# Package: survey
# ============================================================

library(survey)

# ----------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------
load("data/bas-2023/baltimore-area-survey-2023.Rdata")
# Loads object: bas23 (1,352 rows x 148 columns)

# ----------------------------------------------------------
# 2. Remove rows with missing values in either variable
#    The data uses string labels like "Missing: ..." for
#    non-response, so we drop those explicitly.
# ----------------------------------------------------------
bas23_clean <- bas23[
  !bas23[["bas23_hlt_srh"]]      %in% c("Missing: Item non-response") &
  !bas23[["bas23_dem_raceeth4"]] %in% c(
    "Missing: Item non-response",
    "Missing: Could not be constructed from available information"
  ),
]

cat("Rows after removing missing values:", nrow(bas23_clean), "\n")

# ----------------------------------------------------------
# 3. Set up the survey design
#    svydesign() tells R which column holds the weights.
#    ids = ~1 means no clustering (simple random sample structure).
# ----------------------------------------------------------
design <- svydesign(
  ids     = ~1,
  weights = ~bas23_svy_fwgt,
  data    = bas23_clean
)

# ----------------------------------------------------------
# 4. Build the weighted crosstab
#    svytable() produces weighted counts (population estimates).
#    Formula: ~ row_variable + column_variable
# ----------------------------------------------------------
weighted_counts <- svytable(
  ~bas23_hlt_srh + bas23_dem_raceeth4,
  design = design
)

# ----------------------------------------------------------
# 5. Convert to column percentages
#    Column % = within each race group, what share reports
#    each health level? This is the most useful comparison.
# ----------------------------------------------------------
col_pct <- prop.table(weighted_counts, margin = 2) * 100

# ----------------------------------------------------------
# 6. Order health categories from best to worst (for display)
# ----------------------------------------------------------
health_order <- c("Excellent", "Very Good", "Good", "Fair", "Poor")
col_pct_ordered <- col_pct[health_order, ]

# ----------------------------------------------------------
# 7. Print results
# ----------------------------------------------------------
cat("\n=== Weighted crosstab: Self-rated health x Race/ethnicity ===\n")
cat("(Column percentages — % within each race group)\n\n")
print(round(col_pct_ordered, 1))

cat("\n=== Weighted population counts (for reference) ===\n")
print(round(weighted_counts[health_order, ], 0))

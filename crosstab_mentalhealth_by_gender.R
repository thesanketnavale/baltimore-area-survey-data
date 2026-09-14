# ============================================================
# Weighted crosstab: Mental health self-rating x Gender
# Data: Baltimore Area Survey 2023
# Weight column: bas23_svy_fwgt
#
# NOTE: "Transgender" (n=1) and "I use a different term" (n=7)
# have very small sample sizes. Their weighted estimates are
# included but should be interpreted with caution — the
# margins of error are large for these groups.
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
  !bas23[["bas23_hlt_srmh"]]   %in% c("Missing: Item non-response") &
  !bas23[["bas23_dem_gender"]] %in% c("Missing: Item non-response"),
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
  ~bas23_hlt_srmh + bas23_dem_gender,
  design = design
)

# ----------------------------------------------------------
# 5. Convert to column percentages
#    Column % = within each gender group, what share reports
#    each mental health level?
# ----------------------------------------------------------
col_pct <- prop.table(weighted_counts, margin = 2) * 100

# ----------------------------------------------------------
# 6. Order mental health categories from best to worst
# ----------------------------------------------------------
mh_order     <- c("Excellent", "Very Good", "Good", "Fair", "Poor")
gender_order <- c("Female", "Male", "Transgender", "I use a different term")

col_pct_ordered <- col_pct[mh_order, gender_order]

# ----------------------------------------------------------
# 7. Print results
# ----------------------------------------------------------
cat("\n=== Weighted crosstab: Mental health self-rating x Gender ===\n")
cat("(Column percentages — % within each gender group)\n")
cat("* Transgender n=1 and 'I use a different term' n=7: interpret with caution\n\n")
print(round(col_pct_ordered, 1))

cat("\n=== Weighted population counts (for reference) ===\n")
print(round(weighted_counts[mh_order, gender_order], 0))

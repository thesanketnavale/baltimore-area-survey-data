# ============================================================
# Weighted crosstab: Food insecurity x Income
# Data: Baltimore Area Survey 2023
# Weight column: bas23_svy_fwgt
# Variable: bas23_fin_fsirunout — "In the past 12 months,
#   did you run out of food?" (Often / Sometimes / Never true)
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
  !bas23[["bas23_fin_fsirunout"]] %in% c("Missing: Item non-response") &
  !bas23[["bas23_dem_income"]]    %in% c("Missing: Item non-response"),
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
  ~bas23_fin_fsirunout + bas23_dem_income,
  design = design
)

# ----------------------------------------------------------
# 5. Convert to column percentages
#    Column % = within each income bracket, what share
#    reports each food insecurity level?
# ----------------------------------------------------------
col_pct <- prop.table(weighted_counts, margin = 2) * 100

# ----------------------------------------------------------
# 6. Order both axes for clear display
#    Food insecurity: most severe first
#    Income: lowest to highest (already in data order)
# ----------------------------------------------------------
food_order <- c("Often true", "Sometimes true", "Never true")

income_order <- c(
  "Under $15,000",
  "$15,000 to $29,999",
  "$30,000 to $39,999",
  "$40,000 to $54,999",
  "$55,000 to $69,999",
  "$70,000 to $89,999",
  "$90,000 to $109,999",
  "$110,000 to $139,999",
  "$140,000 to $199,999",
  "$200,000 or more"
)

col_pct_ordered <- col_pct[food_order, income_order]

# ----------------------------------------------------------
# 7. Print results
# ----------------------------------------------------------
cat("\n=== Weighted crosstab: Food insecurity x Income ===\n")
cat("(Column percentages — % within each income bracket)\n\n")
print(round(col_pct_ordered, 1))

cat("\n=== Weighted population counts (for reference) ===\n")
print(round(weighted_counts[food_order, income_order], 0))

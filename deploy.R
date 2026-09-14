# ============================================================
# deploy.R  —  Publish the BAS dashboard to shinyapps.io
# ============================================================
#
# BEFORE YOU RUN THIS:
#
#   1. Go to https://www.shinyapps.io and log in.
#   2. Click your account name (top-right) → Account Settings.
#   3. In the left sidebar click "Tokens".
#   4. Click "Show" next to your token, then "Copy to Clipboard".
#   5. Paste that command into R and run it. It looks like:
#
#        rsconnect::setAccountInfo(
#          name   = "your-account-name",
#          token  = "XXXXXXXXXXXXXXXXXXXX",
#          secret = "XXXXXXXXXXXXXXXXXXXX"
#        )
#
#   You only need to do steps 1-5 ONCE per machine.
#
# THEN run this whole file (Ctrl+Shift+Enter in VS Code, or
# source("deploy.R") in an R console).
# ============================================================

library(rsconnect)

# ---- Exact list of files the app needs ----
# Only these files are uploaded — not the full repo.
# The docs/ folder is huge but labels.R only needs the
# three search_index.json files inside it, so we include
# just those three and skip everything else.

app_files <- c(
  "app.R",
  "labels.R",

  # Survey data (one .Rdata per year)
  "data/bas-2023/baltimore-area-survey-2023.Rdata",
  "data/bas-2024/baltimore-area-survey-2024.Rdata",
  "data/bas-2025/baltimore-area-survey-2025.Rdata",

  # Map tab — census tract shapefiles + pre-aggregated BAS estimates
  "data/map_tracts.rds",
  "data/nhd-mapping/nhd_mapping_vars.csv",

  # Variable labels (parsed from these JSON codebook indexes)
  "docs/bas-2023/search_index.json",
  "docs/bas-2024/search_index.json",
  "docs/bas-2025/search_index.json",

  # Logos shown in the navbar
  "www/21cc-logo.png",
  "www/bas-logo.png"
)

# ---- Deploy ----
deployApp(
  appDir   = "f:/BAS - RA/baltimore-area-survey-data-main",
  appFiles = app_files,
  appName  = "bas-dashboard",   # becomes part of your URL
  account  = NULL,              # uses the account you set up in step 5
  forceUpdate = TRUE
)

# After a successful deploy, your URL will be:
#   https://<your-account-name>.shinyapps.io/bas-dashboard

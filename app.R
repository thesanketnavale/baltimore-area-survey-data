# ============================================================
# Baltimore Area Survey (BAS) - Dashboard
# Weighted Crosstab Explorer
# ============================================================
# HOW TO RUN (from the repo root in a terminal):
#   Rscript -e "shiny::runApp(launch.browser = TRUE)"
#
# Install packages once if needed:
#   install.packages(c("shiny", "bslib", "survey", "DT",
#                      "ggplot2", "jsonlite", "shinycssloaders", "sf"))
# ============================================================

library(shiny)
library(bslib)
library(survey)
library(DT)
library(ggplot2)
library(jsonlite)
library(shinycssloaders)
library(sf)      # choropleth map — geom_sf for geometry handling
library(plotly)  # interactive map (zoom / pan / hover) via ggplotly()

source("labels.R")

# ---- Which years to attempt loading ----
CANDIDATE_YEARS <- 2021:2025

# ---- Map the "Compare by" radio choices to variable name suffixes ----
# These three variables are used as the COLUMN in every crosstab.
DEMO_VAR_MAP <- c(
  race   = "dem_raceeth4",
  income = "dem_income",
  gender = "dem_gender"
)

# ---- Year-band to CSV file mapping (Map tab) ----
# Each entry: "display label" = "path to pre-aggregated tract-level CSV"
# The CSV contains pooled BAS estimates (TSI, FSI, cohesion) for all tracts
# that had respondents across that band of years.
#
# To add a future band (e.g., 2024-2026): drop the new CSV in data/nhd-mapping/
# and add one line here — no other code changes needed.
BAND_FILES <- c(
  "2023-2025" = "data/nhd-mapping/nhd_mapping_vars.csv"
)

# ---- Load one year of BAS data ----
load_bas_year <- function(year) {
  yy   <- substr(as.character(year), 3, 4)
  obj  <- paste0("bas", yy)
  path <- file.path("data", paste0("bas-", year),
                    paste0("baltimore-area-survey-", year, ".Rdata"))
  if (!file.exists(path)) return(NULL)
  e <- new.env()
  load(path, envir = e)
  if (!exists(obj, envir = e)) return(NULL)
  df <- get(obj, envir = e)
  wt <- paste0(obj, "_svy_fwgt")
  if (!wt %in% names(df)) return(NULL)
  list(data = df, weight = wt)
}

# Load all available years at startup (skips missing files silently).
BAS <- list()
for (y in CANDIDATE_YEARS) {
  loaded <- load_bas_year(y)
  if (!is.null(loaded)) BAS[[as.character(y)]] <- loaded
}
if (length(BAS) == 0) {
  stop("No BAS data files found. Open the REPO ROOT in VS Code (the folder ",
       "that contains data/) and rerun.")
}
AVAILABLE_YEARS <- names(BAS)

# ---- Build human-readable label lookups (once, at startup) ----
message("Loading variable labels from codebooks...")
LABELS <- setNames(
  lapply(as.integer(AVAILABLE_YEARS), parse_codebook_labels),
  AVAILABLE_YEARS
)

# ---- Load Baltimore census tract shapefiles for the Map tab ----
# The RDS is stored as an sf object (converted from sp locally before deploy).
# The guard handles both formats in case the file is ever regenerated as sp.
map_tracts <- tryCatch({
  tracts <- readRDS("data/map_tracts.rds")
  if (!inherits(tracts, "sf")) sf::st_as_sf(tracts) else tracts
}, error = function(e) {
  message("Could not load map_tracts.rds: ", conditionMessage(e))
  NULL
})

# ---- Helper: which topic groups have at least one pickable variable? ----
# "Pickable" = factor variable, not a svy_ admin var, not one of the 3 demo vars.
available_topics <- function(year) {
  df      <- BAS[[year]]$data
  yy      <- substr(year, 3, 4)
  exclude <- paste0("bas", yy, "_", unname(DEMO_VAR_MAP))
  vars    <- names(df)
  vars    <- vars[!grepl("_svy_", vars)]
  vars    <- vars[!vars %in% exclude]
  vars    <- vars[sapply(vars, function(v) is.factor(df[[v]]))]
  topics  <- unique(sapply(vars, var_topic))
  # Return in canonical order
  canonical <- unname(TOPIC_MAP)
  canonical[canonical %in% topics]
}

# ---- Helper: choices list for row_var within one topic ----
# Returns named character vector: c("Display label" = "bas23_hlt_srh", ...)
pickable_choices_for_topic <- function(year, topic) {
  df      <- BAS[[year]]$data
  yy      <- substr(year, 3, 4)
  exclude <- paste0("bas", yy, "_", unname(DEMO_VAR_MAP))
  vars    <- names(df)
  vars    <- vars[!grepl("_svy_", vars)]
  vars    <- vars[!vars %in% exclude]
  vars    <- vars[sapply(vars, function(v) is.factor(df[[v]]))]
  vars    <- vars[sapply(vars, var_topic) == topic]
  lbl     <- LABELS[[year]]
  labels  <- sapply(vars, function(v) {
    l <- lbl[v]
    if (is.na(l) || !nzchar(l) || l == v) v else l
  })
  setNames(vars, labels)   # c("Label" = "varname", ...)
}

# ============================================================
# THEME  — clean, professional research-organization style
# ============================================================
app_theme <- bs_theme(
  version      = 5,
  bg           = "#FFFFFF",
  fg           = "#1c2331",
  primary      = "#1d4e89",
  secondary    = "#4a7fb5",
  success      = "#2a7f4f",
  info         = "#2e86ab",
  "sidebar-bg" = "#f4f6f9",
  "navbar-bg"  = "#1d4e89",
  "navbar-fg"  = "#ffffff",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ============================================================
# USER INTERFACE
# ============================================================
ui <- page_navbar(

  # Custom CSS — injected into the page <head> by htmltools
  tags$head(tags$style(HTML("

    /* ── Sidebar: tighten label/input vertical spacing ───────────────── */
    /* Shiny wraps every input in .shiny-input-container which adds 15px  */
    /* bottom margin by default — reduce it so sections feel snug.        */
    .sidebar .shiny-input-container {
      margin-bottom: 0.15rem !important;
    }
    /* Individual radio / checkbox rows */
    .sidebar .form-check {
      margin-bottom: 0.06rem !important;
    }
    /* Sidebar hr dividers */
    .sidebar hr {
      margin-top:    0.55rem !important;
      margin-bottom: 0.55rem !important;
    }

    /* ── Main content card: cut the excess top padding in tab panes ───── */
    /* navset_card_underline wraps tabs in a .card with 1rem top padding;  */
    /* the small title paragraph already provides visual spacing.          */
    .card > .card-body {
      padding-top: 0.45rem !important;
    }
    /* ── Slim stats bar (replaces the four value-box tiles) ──────────── */
    /* A single horizontal row showing year, N, population, city/county.  */
    /* Compact (~65px tall), white, consistent with professional dashboards. */
    .bas-stats-bar {
      display: flex;
      align-items: stretch;
      background: #ffffff;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      overflow: hidden;
      margin-bottom: 0.85rem;
      box-shadow: 0 1px 4px rgba(0,0,0,0.06);
    }
    /* Each stat cell: label stacked above value */
    .stat-cell {
      display: flex;
      flex-direction: column;
      justify-content: center;
      padding: 0.5rem 1.25rem;
      flex: 1;
      border-right: 1px solid #dee2e6;
      min-width: 0;
    }
    .stat-cell:last-child { border-right: none; }
    /* Left accent stripe on the first cell to anchor the bar visually */
    .stat-cell:first-child { border-left: 4px solid #1d4e89; }
    /* Tiny uppercase label above the number */
    .stat-label {
      font-size: 0.59rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.06em;
      color: #6c757d;
      margin-bottom: 2px;
      white-space: nowrap;
    }
    /* The number / value itself */
    .stat-val {
      font-size: 1.05rem;
      font-weight: 700;
      color: #1c2331;
      line-height: 1.25;
      white-space: nowrap;
    }

    /* ── Data transparency note (below table and chart) ─────────────── */
    /* Shows respondent count + weighted pop; replaces the alarming banner. */
    .bas-data-note {
      font-size: 0.78em;
      color: #555;
      padding: 0.35rem 0.65rem;
      background: #f8f9fa;
      border-radius: 4px;
      border-left: 3px solid #dee2e6;
      margin-top: 0.6rem;
      line-height: 1.55;
    }
    /* Small-cell rider — subtle amber, inline, not alarming */
    .bas-data-note .small-cell-note {
      color: #7a5500;
      margin-left: 0.4rem;
    }

    /* ── Navbar page tabs (Data Explorer / Map) ────────────────────────────── */
    /* bslib nav-underline is calibrated for light backgrounds; these rules    */
    /* restore readable contrast on the dark navy bar.                         */
    .navbar .nav-underline .nav-link {
      color: rgba(255,255,255,0.82) !important;
      font-size: 0.875rem;
      font-weight: 500;
      padding: 0.5rem 1.1rem !important;
      border-bottom: 3px solid transparent !important;
      border-radius: 3px 3px 0 0;
      transition: color 0.15s, background 0.15s, border-color 0.15s;
    }
    .navbar .nav-underline .nav-link:hover {
      color: #ffffff !important;
      background: rgba(255,255,255,0.10) !important;
      border-bottom-color: rgba(255,255,255,0.5) !important;
    }
    .navbar .nav-underline .nav-link.active {
      color: #ffffff !important;
      font-weight: 600;
      background: rgba(255,255,255,0.12) !important;
      border-bottom-color: #ffffff !important;
    }

    /* ── Sidebar collapse/expand arrow ────────────────────────────────────── */
    /* The bslib collapse toggle is functional but visually heavy; dial it     */
    /* back so it doesn't compete with the main content at first glance.       */
    button.collapse-toggle {
      opacity: 0.30;
      transition: opacity 0.2s;
    }
    button.collapse-toggle:hover {
      opacity: 0.90;
    }

    /* ── Navbar brand area: prevent line-wrap on smaller screens ───────────── */
    .navbar-brand {
      white-space: nowrap;
    }

  "))),

  # Navbar brand: logos + app name.
  # Uses inline-flex (not block flex) so the brand stays compact in the
  # Bootstrap navbar. The separator is a border-left on the text itself —
  # one element, no extra flex gap drift.
  title = tags$span(
    style = "display:inline-flex; align-items:center; gap:6px; white-space:nowrap;",

    tags$img(
      src     = "21cc-logo.png",
      height  = "26px",
      alt     = "JHU 21CC",
      style   = "border-radius:3px;",
      onerror = "this.style.display='none'"
    ),
    tags$img(
      src     = "bas-logo.png",
      height  = "26px",
      alt     = "BAS",
      style   = "border-radius:3px;",
      onerror = "this.style.display='none'"
    ),

    # border-left acts as the visual divider without adding a flex item
    tags$span(
      style = paste0(
        "font-weight:700; font-size:0.97em; letter-spacing:-0.1px;",
        " padding-left:10px;",
        " border-left:1px solid rgba(255,255,255,0.28);"
      ),
      "Baltimore Area Survey"
    )
  ),

  theme = app_theme,

  # ── DATA EXPLORER PAGE ────────────────────────────────────────────────────
  # Contains the sidebar controls + Table / Chart / About tabs.
  # The Map page lives at the same level — see below.
  nav_panel(
    title = "Data Explorer",
    icon  = icon("table-cells"),
    layout_sidebar(

      # --------------------------------------------------------
      # SIDEBAR
      # --------------------------------------------------------
      sidebar = sidebar(
    width = 300,

    # -- Geography filter --
    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Geography"),
    radioButtons(
      "geo_filter", label = NULL,
      choices  = c("All respondents"      = "all",
                   "Baltimore City only"  = "City",
                   "Baltimore County only"= "County"),
      selected = "all"
    ),

    hr(class = "my-2"),

    # -- Survey year --
    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Survey year"),
    selectInput("year", label = NULL,
                choices  = AVAILABLE_YEARS,
                selected = tail(AVAILABLE_YEARS, 1)),

    # -- Missing data --
    checkboxInput("exclude_missing",
                  label = "Exclude missing responses",
                  value = TRUE),

    hr(class = "my-2"),

    # -- Compare by (column variable) --
    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Compare by"),
    radioButtons(
      "col_demo", label = NULL,
      choices  = c("Race / Ethnicity"      = "race",
                   "Income"                = "income",
                   "Gender"                = "gender",
                   "None (overall totals)" = "overall"),
      selected = "race"
    ),

    hr(class = "my-2"),

    # -- Two-stage row variable: topic first, then specific question --
    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Survey topic"),
    selectInput("topic_cat", label = NULL, choices = NULL),

    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Survey question"),
    # selectizeInput gives a built-in search box inside the dropdown
    selectizeInput(
      "row_var", label = NULL, choices = NULL,
      options = list(placeholder = "Search questions...", maxOptions = 200)
    ),

    hr(class = "my-2"),

    # -- Display options --
    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Numbers to show"),
    radioButtons(
      "metric", label = NULL,
      choices  = c("Weighted %" = "pct",
                   "Weighted counts"= "count"),
      selected = "pct"
    ),

    # Direction toggle — hidden when showing counts or when no comparison group is selected
    conditionalPanel(
      condition = "input.metric === 'pct' && input.col_demo !== 'overall'",
      tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1 mt-1",
                 "Percentage direction"),
      radioButtons(
        "pct_dir", label = NULL,
        choices  = c("Column % — each group sums to 100%" = "col",
                     "Row % — each response sums to 100%" = "row"),
        selected = "col"
      )
    ),

    tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
               "Chart style"),
    radioButtons(
      "chart_style", label = NULL,
      choices  = c("Grouped bars"      = "dodge",
                   "100% stacked bars" = "stack"),
      selected = "dodge"
    ),

    hr(class = "my-2"),

    helpText(
      tags$small(
        icon("circle-info"), " ",
        "All numbers use BAS survey weights and represent ",
        "Baltimore-area residents, not just survey respondents."
      )
    )
  ),

  # --------------------------------------------------------
  # SLIM STATS BAR  (replaces the four coloured value-box tiles)
  # Shows the same four numbers in a compact single-row strip.
  # --------------------------------------------------------
  div(
    class = "bas-stats-bar",

    div(class = "stat-cell",
      div(class = "stat-label", "Survey year"),
      div(class = "stat-val",   textOutput("vb_year", inline = TRUE))
    ),
    div(class = "stat-cell",
      div(class = "stat-label", "Respondents (filtered)"),
      div(class = "stat-val",   textOutput("vb_n", inline = TRUE))
    ),
    div(class = "stat-cell",
      div(class = "stat-label", "Est. population (weighted)"),
      div(class = "stat-val",   textOutput("vb_pop", inline = TRUE))
    ),
    div(class = "stat-cell",
      div(class = "stat-label", "City / County split (weighted est.)"),
      div(class = "stat-val",   uiOutput("vb_split"))
    )
  ),

  # --------------------------------------------------------
  # MAIN TABS
  # --------------------------------------------------------
  navset_card_underline(
    id = "main_tabs",

    # ---- TABLE TAB ----
    nav_panel(
      title = "Table",
      icon  = icon("table"),

      # Title + weighted-vs-unweighted toggle on the same row
      div(
        class = "d-flex justify-content-between align-items-center mt-1 mb-2 gap-3 flex-wrap",
        p(class = "text-muted small mb-0 flex-grow-1",
          textOutput("tab_title", inline = TRUE)),
        div(
          radioButtons(
            "table_mode", label = NULL,
            choices  = c("Weighted estimates"             = "weighted",
                         "Compare: weighted vs. raw N"    = "compare"),
            selected = "weighted",
            inline   = TRUE
          )
        )
      ),

      uiOutput("reading_dir_note"),

      withSpinner(
        DTOutput("tab"),
        type             = 1,
        color            = "#4a7fb5",
        color.background = "white"
      ),
      div(class = "pt-2",
        downloadButton("download_csv", "Download table as CSV",
                       class = "btn-sm btn-outline-secondary")),
      # Data transparency note — shows respondent base + small-cell asterisk key
      uiOutput("data_note_tab")
    ),

    # ---- CHART TAB ----
    nav_panel(
      title = "Chart",
      icon  = icon("chart-bar"),

      p(class = "text-muted small mb-2 mt-1",
        textOutput("chart_title", inline = TRUE)),

      uiOutput("plot_container"),

      div(
        class = "pt-3",
        downloadButton("download_png", "Download chart as PNG",
                       class = "btn-sm btn-outline-secondary")
      ),
      # Data transparency note — mirrors the one in the Table tab
      uiOutput("data_note_chart")
    ),

    # ---- ABOUT TAB ----
    nav_panel(
      title = "About",
      icon  = icon("circle-info"),

      div(
        style = "max-width:680px; padding:1.5rem 0.25rem;",

        h5("About the Baltimore Area Survey"),
        p("The Baltimore Area Survey (BAS) is an annual address-based survey of adult ",
          "residents of Baltimore City and Baltimore County, conducted by the ",
          "Johns Hopkins University 21st Century Cities Initiative. It covers ",
          "neighborhood quality, health, finances, civic engagement, and demographics."),

        h5("About this app"),
        p("This tool lets anyone explore BAS results interactively — no statistics ",
          "background required. Choose a demographic comparison group and a survey ",
          "question to get a properly weighted cross-tabulation that represents the ",
          "full Baltimore-area adult population."),

        h5("How to use"),
        tags$ol(
          tags$li("Choose a geography (All, City only, or County only) and a survey year."),
          tags$li("Select what to Compare by: Race/Ethnicity, Income, or Gender."),
          tags$li("Pick a topic category, then search for a specific question."),
          tags$li("Switch between weighted % and weighted counts."),
          tags$li("In the Table tab, use \"Compare: weighted vs. raw N\" to see how ",
                  "weights change the picture."),
          tags$li("Use the Download buttons to export results.")
        ),

        h5("About survey weights"),
        p("Survey weights (", tags$code("svy_fwgt"), ") are applied using R's ",
          tags$code("survey"), " package. Weights correct for Baltimore City and ",
          "County being sampled at different rates, and for non-response bias. ",
          "Raw unweighted counts are never shown as population estimates."),

        hr(),
        tags$small(class = "text-muted",
          "Built by JHU 21CC Research Assistants | ",
          tags$a("21cc.jhu.edu", href = "https://21cc.jhu.edu", target = "_blank"))
      )
    )           # close About nav_panel
    )           # close navset_card_underline
  )             # close layout_sidebar
  ),            # close nav_panel("Data Explorer")

  # ── MAP PAGE ─────────────────────────────────────────────────────────────
  # Choropleth rendered with ggplot2 + geom_sf — no leaflet/terra/raster needed.
  nav_panel(
    title = "Map",
    icon  = icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 270,

        tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
                   "Year band"),
        selectInput("map_band", label = NULL,
                    choices  = names(BAND_FILES),
                    selected = names(BAND_FILES)[1]),

        hr(class = "my-2"),

        tags$label(class = "form-label fw-semibold small text-uppercase text-muted mb-1",
                   "Measure"),
        radioButtons(
          "map_measure", label = NULL,
          choices  = c(
            "Transportation Insecurity" = "tsi6_mean",
            "Food Insecurity"           = "fsi6_mean",
            "Neighborhood Cohesion"     = "nhd_cohes_mean"
          ),
          selected = "tsi6_mean"
        ),

        hr(class = "my-2"),

        helpText(
          tags$small(
            icon("circle-info"), " ",
            "Weighted BAS estimates pooled across respondents in the ",
            "selected year band, mapped to census tract."
          )
        )
      ),

      withSpinner(
        plotlyOutput("bas_map", height = "620px"),
        type = 1, color = "#4a7fb5", color.background = "white"
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Derive the actual column variable name from the radio selection ----
  # Returns "__OVERALL__" when no comparison is requested — a sentinel string
  # that can't be a real BAS variable name.
  col_var_actual <- reactive({
    req(input$year, input$col_demo)
    if (input$col_demo == "overall") return("__OVERALL__")
    yy     <- substr(input$year, 3, 4)
    suffix <- DEMO_VAR_MAP[[input$col_demo]]
    varname <- paste0("bas", yy, "_", suffix)
    # Verify it exists in this year's data
    if (!varname %in% names(BAS[[input$year]]$data)) {
      shiny::validate(paste("The variable", varname, "was not found in the",
                            input$year, "dataset."))
    }
    varname
  })

  # ---- When year changes: update the topic dropdown ----
  observe({
    req(input$year)
    topics <- available_topics(input$year)
    updateSelectInput(session, "topic_cat",
                      choices  = topics,
                      selected = topics[1])
  })

  # ---- When topic or year changes: update the question (row_var) dropdown ----
  observe({
    req(input$topic_cat, input$year)
    choices <- pickable_choices_for_topic(input$year, input$topic_cat)
    updateSelectizeInput(session, "row_var",
                         choices  = choices,
                         selected = if (length(choices) > 0) choices[[1]] else NULL,
                         server   = FALSE)
  })

  # ---- Geography-filtered data ----
  # Applies the City/County filter. Missing-value exclusion is handled later
  # in crosstab_r() because it depends on which variables are selected.
  filtered_data_r <- reactive({
    req(input$year)
    d  <- BAS[[input$year]]
    df <- d$data
    yy <- substr(input$year, 3, 4)

    if (input$geo_filter != "all") {
      cty_col <- paste0("bas", yy, "_svy_cty")
      if (cty_col %in% names(df)) {
        keep <- !is.na(df[[cty_col]]) &
                as.character(df[[cty_col]]) == input$geo_filter
        df <- df[keep, ]
      }
    }
    df
  })

  # ---- City / County weighted split (always from the full year data) ----
  city_county_split_r <- reactive({
    req(input$year)
    d      <- BAS[[input$year]]
    df     <- d$data
    yy     <- substr(input$year, 3, 4)
    cty_col <- paste0("bas", yy, "_svy_cty")
    if (!cty_col %in% names(df)) return(NULL)

    wt_col    <- d$weight
    city_wt   <- sum(df[[wt_col]][as.character(df[[cty_col]]) == "City"],   na.rm = TRUE)
    county_wt <- sum(df[[wt_col]][as.character(df[[cty_col]]) == "County"], na.rm = TRUE)
    total     <- city_wt + county_wt
    if (total == 0) return(NULL)
    list(
      city_pct   = round(city_wt   / total * 100),
      county_pct = round(county_wt / total * 100)
    )
  })

  # ---- Value boxes ----
  output$vb_year <- renderText({ input$year })

  output$vb_n <- renderText({
    format(nrow(filtered_data_r()), big.mark = ",")
  })

  output$vb_pop <- renderText({
    df  <- filtered_data_r()
    wt  <- BAS[[input$year]]$weight
    pop <- sum(df[[wt]], na.rm = TRUE)
    if (pop >= 1e6) paste0("~", round(pop / 1e6, 1), "M")
    else            paste0("~", format(round(pop / 1e3), big.mark = ","), "K")
  })

  # City/County split — single-line inline display for the stats bar
  output$vb_split <- renderUI({
    sp <- city_county_split_r()
    if (is.null(sp)) return(span("N/A"))
    span(
      tags$b(paste0(sp$city_pct, "%")), " City",
      tags$span(style = "color:#bbb; margin:0 0.35em;", "|"),
      tags$b(paste0(sp$county_pct, "%")), " County"
    )
  })

  # ---- Weighted crosstab (core reactive) ----
  crosstab_r <- reactive({
    req(input$row_var, col_var_actual())
    rv <- input$row_var
    cv <- col_var_actual()

    if (cv != "__OVERALL__" && rv == cv) {
      shiny::validate("Please select two different variables.")
    }

    df <- filtered_data_r()
    wt <- BAS[[input$year]]$weight

    # Optionally drop rows where either variable is "Missing: ..."
    if (isTRUE(input$exclude_missing)) {
      rv_chars <- as.character(df[[rv]])
      keep     <- !grepl("^Missing", rv_chars)
      if (cv != "__OVERALL__") {
        cv_chars <- as.character(df[[cv]])
        keep     <- keep & !grepl("^Missing", cv_chars)
      }
      df <- df[keep, ]
      # Drop unused factor levels so "Missing" categories don't appear as empty
      # columns in svytable() output (fix: missing column still shown after filter)
      df <- droplevels(df)
    }

    # Suppress small non-binary/transgender categories from gender comparison.
    # "Transgender" (N=1-14) and "I use a different term" (N=7-11) are both
    # below the 30-respondent reliability threshold (team decision, Sep 2026).
    if (cv != "__OVERALL__" && grepl("dem_gender", cv)) {
      keep_g <- !grepl("(?i)transgender|non.?binary|different term",
                        as.character(df[[cv]]), perl = TRUE)
      df <- df[keep_g, ]
      df <- droplevels(df)
    }

    shiny::validate(shiny::need(nrow(df) > 0, "No data remaining after applying filters."))

    design <- svydesign(
      ids     = ~1,
      weights = as.formula(paste0("~`", wt, "`")),
      data    = df
    )

    if (cv == "__OVERALL__") {
      # Marginal distribution — no column breakdown
      tab1d <- tryCatch(
        svytable(as.formula(paste0("~`", rv, "`")), design),
        error = function(e) {
          cat("[BAS ERROR]", conditionMessage(e), "\n", file = stderr())
          stop(e)
        }
      )
      # Reshape as a 2D table with one column so all downstream code works uniformly
      as.table(matrix(as.numeric(tab1d), ncol = 1,
                      dimnames = list(names(tab1d), c("All respondents"))))
    } else {
      tryCatch(
        svytable(as.formula(paste0("~`", rv, "` + `", cv, "`")), design),
        error = function(e) {
          cat("[BAS ERROR]", conditionMessage(e), "\n", file = stderr())
          stop(e)
        }
      )
    }
  })

  # ---- Shared title string ----
  TIMES <- "×"   # ×
  DASH  <- "—"   # —

  crosstab_title <- reactive({
    req(input$row_var, col_var_actual(), input$year)
    row_lbl <- get_var_label(input$row_var, LABELS[[input$year]])
    if (col_var_actual() == "__OVERALL__") {
      paste0(row_lbl, "  ", DASH, "  BAS ", input$year)
    } else {
      col_lbl <- get_var_label(col_var_actual(), LABELS[[input$year]])
      paste0(row_lbl, " ", TIMES, " ", col_lbl, "  ", DASH, "  BAS ", input$year)
    }
  })

  output$tab_title <- renderText({
    is_overall <- isTRUE(col_var_actual() == "__OVERALL__")
    dir_word   <- if (is_overall || !isTRUE(input$pct_dir == "col")) "row" else "column"
    mode_txt <- switch(input$table_mode,
      compare  = paste0("weighted vs. unweighted ", dir_word, " %"),
      weighted = if (input$metric == "pct") {
        if (is_overall) "weighted %" else paste0("weighted ", dir_word, " %")
      } else "weighted counts"
    )
    paste0(crosstab_title(), "  (", mode_txt, ")")
  })

  # ---- Row-direction note (appears above table in weighted row % mode) ----
  output$reading_dir_note <- renderUI({
    req(input$row_var, col_var_actual())
    if (input$table_mode != "weighted") return(NULL)
    if (input$metric    != "pct")       return(NULL)
    if (col_var_actual() == "__OVERALL__") return(NULL)
    row_lbl  <- get_var_label(input$row_var,    LABELS[[input$year]])
    col_lbl  <- get_var_label(col_var_actual(), LABELS[[input$year]])
    note_txt <- if (isTRUE(input$pct_dir == "col"))
      paste0("Each column sums to 100%. For each ", col_lbl,
             " group, percentages show how ", row_lbl, " responses are distributed.")
    else
      paste0("Each row sums to 100%. Percentages show the distribution of ",
             row_lbl, " within each ", col_lbl, " group.")
    div(class = "text-muted small mb-1",
      icon("circle-info", style = "margin-right:4px;"),
      note_txt
    )
  })

  output$chart_title <- renderText({
    style_txt <- if (input$chart_style == "stack") "100% stacked" else "grouped bars"
    paste0(crosstab_title(), "  (", style_txt, ")")
  })

  # ---- Table matrix (weighted) ----
  table_matrix_r <- reactive({
    tab        <- crosstab_r()
    is_overall <- ncol(tab) == 1 && !is.null(colnames(tab)) &&
                  colnames(tab)[1] == "All respondents"
    if (input$metric == "pct") {
      if (is_overall)                           round(prop.table(tab)    * 100, 1)  # marginal
      else if (isTRUE(input$pct_dir == "col")) round(prop.table(tab, 2) * 100, 1)  # column %
      else                                      round(prop.table(tab, 1) * 100, 1)  # row %
    } else {
      round(tab)
    }
  })

  # ---- Unweighted raw-count table (for comparison mode) ----
  unweighted_r <- reactive({
    req(input$row_var, col_var_actual())
    rv <- input$row_var
    cv <- col_var_actual()
    df <- filtered_data_r()
    if (isTRUE(input$exclude_missing)) {
      keep <- !grepl("^Missing", as.character(df[[rv]]))
      if (cv != "__OVERALL__") keep <- keep & !grepl("^Missing", as.character(df[[cv]]))
      df   <- df[keep, ]
    }
    if (cv == "__OVERALL__") {
      tab1d <- table(df[[rv]])
      as.table(matrix(as.integer(tab1d), ncol = 1,
                      dimnames = list(names(tab1d), c("All respondents"))))
    } else {
      table(df[[rv]], df[[cv]])
    }
  })

  # ---- Small-cell check (TRUE/FALSE only) ----
  # Cells with N < 30 get a quiet "*" in the table; no alarming banner shown.
  has_small_cells_r <- reactive({
    uwtab <- unweighted_r()
    any(uwtab >= 1 & uwtab < 30)
  })

  # ---- Data transparency note ----
  # Shows respondent count + weighted population behind the current crosstab.
  # Goal: build trust by making the evidence visible, not alarming users.
  make_data_note_ui <- function(n_resp, pop_str, has_small, for_chart = FALSE) {
    base_note <- tagList(
      icon("circle-info", style = "color:#888; margin-right:4px;"),
      tags$b(format(n_resp, big.mark = ",")), " respondents used",
      paste0(" — weighted to represent ~", pop_str, " Baltimore-area adults.")
    )
    small_rider <- if (has_small) {
      tags$span(
        class = "small-cell-note",
        if (for_chart)
          "  † Some subgroups have fewer than 30 respondents — see Table tab for cell details."
        else
          "  † Fewer than 30 respondents — marked in table above."
      )
    }
    div(class = "bas-data-note", base_note, small_rider)
  }

  output$data_note_tab <- renderUI({
    req(input$row_var, col_var_actual())
    n_resp  <- sum(unweighted_r())
    pop     <- sum(crosstab_r())
    pop_str <- if (pop >= 1e6) paste0(round(pop / 1e6, 1), "M")
               else            paste0(format(round(pop / 1e3), big.mark = ","), "K")
    make_data_note_ui(n_resp, pop_str, has_small_cells_r(), for_chart = FALSE)
  })

  output$data_note_chart <- renderUI({
    req(input$row_var, col_var_actual())
    n_resp  <- sum(unweighted_r())
    pop     <- sum(crosstab_r())
    pop_str <- if (pop >= 1e6) paste0(round(pop / 1e6, 1), "M")
               else            paste0(format(round(pop / 1e3), big.mark = ","), "K")
    make_data_note_ui(n_resp, pop_str, has_small_cells_r(), for_chart = TRUE)
  })

  # ---- Table render ----
  output$tab <- renderDT({
    if (input$table_mode == "compare") {
      # Side-by-side: weighted % vs unweighted %
      # Overall mode: marginal % (prop.table with no margin)
      # Cross-tab mode: row % (prop.table margin = 1)
      wtab   <- crosstab_r()
      uwtab  <- unweighted_r()
      is_ov  <- ncol(wtab) == 1 && !is.null(colnames(wtab)) &&
                 colnames(wtab)[1] == "All respondents"
      # margin: 0 = overall (marginal), 1 = row %, 2 = column %
      pct_margin <- if (is_ov) 0 else if (isTRUE(input$pct_dir == "col")) 2 else 1
      w_pct  <- round(if (pct_margin == 0) prop.table(wtab)  * 100 else prop.table(wtab,  pct_margin) * 100, 1)
      uw_pct <- round(if (pct_margin == 0) prop.table(uwtab) * 100 else prop.table(uwtab, pct_margin) * 100, 1)

      rn  <- rownames(w_pct)
      cn  <- colnames(w_pct)

      rows <- lapply(rn, function(r) {
        w_row  <- if (r %in% rownames(w_pct))  as.numeric(w_pct[r, ])  else rep(NA, length(cn))
        uw_row <- if (r %in% rownames(uw_pct)) as.numeric(uw_pct[r, ]) else rep(NA, length(cn))
        c(setNames(w_row,  paste0("Wtd%: ", cn)),
          setNames(uw_row, paste0("Raw%: ", cn)))
      })
      df_cmp <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      rownames(df_cmp) <- rn

      datatable(
        df_cmp,
        rownames = TRUE,
        caption  = htmltools::tags$caption(
          style = "caption-side:top; font-size:0.8em; color:#666;",
          paste0("Wtd% = weighted ",
                 if (isTRUE(input$pct_dir == "col")) "column" else "row",
                 "% (population estimate) | Raw% = unweighted ",
                 if (isTRUE(input$pct_dir == "col")) "column" else "row",
                 "% (respondents only)")
        ),
        options = list(dom = "t", scrollX = TRUE, pageLength = nrow(df_cmp)),
        class   = "compact stripe hover"
      )
    } else {
      m      <- table_matrix_r()
      uwtab  <- unweighted_r()
      df_mat <- as.data.frame.matrix(m)

      # Mark cells with fewer than 30 respondents with "†" — quiet, standard
      # research practice. Converts numeric cells to character for those cells.
      any_small <- FALSE
      for (rn in rownames(df_mat)) {
        r_i <- match(rn, rownames(uwtab))
        if (is.na(r_i)) next
        for (cn in colnames(df_mat)) {
          c_i <- match(cn, colnames(uwtab))
          if (is.na(c_i)) next
          n_val <- uwtab[r_i, c_i]
          if (n_val >= 1 && n_val < 30) {
            df_mat[rn, cn] <- paste0(df_mat[rn, cn], "†")
            any_small <- TRUE
          }
        }
      }

      # Build caption: only show the asterisk key when small cells exist
      cap <- if (any_small) {
        htmltools::tags$caption(
          style = "caption-side:bottom; font-size:0.75em; color:#7a5500; text-align:left; padding-top:4px;",
          "† Fewer than 30 respondents — weighted estimate may be less stable."
        )
      }

      datatable(
        df_mat,
        rownames = TRUE,
        caption  = cap,
        options  = list(dom = "t", scrollX = TRUE, pageLength = nrow(df_mat)),
        class    = "compact stripe hover"
      )
    }
  })

  # ---- Chart reactive ----
  chart_r <- reactive({
    tab <- crosstab_r()
    df  <- as.data.frame(tab)
    names(df) <- c("row", "col", "Freq")

    # Shorten "Missing: Item non-response" → "Missing" for cleaner labels
    df$row <- gsub("^Missing:.*", "Missing", as.character(df$row))
    df$col <- gsub("^Missing:.*", "Missing", as.character(df$col))

    is_overall_mode <- col_var_actual() == "__OVERALL__"

    row_lbl <- get_var_label(input$row_var, LABELS[[input$year]])
    col_lbl <- if (is_overall_mode) "" else get_var_label(col_var_actual(), LABELS[[input$year]])

    # Color palette — enough distinct colors for up to 12 categories
    pal <- c("#1d4e89", "#2e86ab", "#a8dadc", "#e9c46a",
             "#f4a261", "#e63946", "#2a9d8f", "#8338ec",
             "#fb5607", "#06d6a0", "#118ab2", "#ffd166")

    # In stacked column % mode the fill variable is the row (response), not the column.
    # Everywhere else the fill variable is the column (demographic group).
    is_col_pct_stack <- !is_overall_mode && isTRUE(input$pct_dir == "col") &&
                        input$chart_style == "stack"
    n_fill  <- if (is_col_pct_stack) length(unique(df$row)) else length(unique(df$col))
    pal_use <- if (n_fill <= length(pal)) pal[seq_len(n_fill)] else
                 colorRampPalette(c("#1d4e89", "#a8dadc", "#e63946"))(n_fill)

    # Dynamic bottom margin: prevents x-axis labels from being clipped.
    # In stacked column % mode the x-axis is df$col (demographic groups),
    # otherwise it's df$row (response options).
    x_labels        <- if (is_col_pct_stack) unique(df$col) else unique(df$row)
    max_label_chars <- max(nchar(as.character(x_labels)), 0)
    bottom_margin   <- max(30, min(120, max_label_chars * 3.5))

    base_theme <- theme_minimal(base_size = 13) +
      theme(
        axis.text.x      = element_text(angle = 30, hjust = 1, vjust = 1),
        legend.position  = if (is_overall_mode) "none" else "bottom",
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        legend.title     = element_text(face = "bold", size = 11),
        axis.title       = element_text(size = 11),
        plot.margin      = margin(t = 10, r = 10, b = bottom_margin, l = 10, unit = "pt")
      )

    if (input$chart_style == "stack") {
      if (is_col_pct_stack) {
        # Column % stacked: x = demographic group, fill = response option
        # Each bar shows how that group's responses are distributed (sums to 100%)
        ggplot(df, aes(x = col, y = Freq, fill = row)) +
          geom_col(position = "fill", width = 0.7) +
          scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
          scale_fill_manual(values = pal_use) +
          labs(x = col_lbl, y = "Share within group", fill = row_lbl) +
          base_theme
      } else {
        # Row % stacked (default): x = response, fill = demographic group
        ggplot(df, aes(x = row, y = Freq, fill = col)) +
          geom_col(position = "fill", width = 0.7) +
          scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
          scale_fill_manual(values = pal_use) +
          labs(x = row_lbl, y = "Share of respondents", fill = col_lbl) +
          base_theme
      }
    } else {
      if (input$metric == "pct") {
        if (is_overall_mode) {
          df$value <- df$Freq / sum(df$Freq) * 100
          ylab     <- "Weighted %"
        } else if (isTRUE(input$pct_dir == "col")) {
          df$value <- ave(df$Freq, df$col, FUN = function(x) x / sum(x) * 100)
          ylab     <- "Weighted column %"
        } else {
          df$value <- ave(df$Freq, df$row, FUN = function(x) x / sum(x) * 100)
          ylab     <- "Weighted row %"
        }
      } else {
        df$value <- df$Freq
        ylab     <- "Estimated residents"
      }
      ggplot(df, aes(x = row, y = value, fill = col)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = pal_use) +
        labs(x = row_lbl, y = ylab, fill = col_lbl) +
        base_theme
    }
  })

  output$plot_container <- renderUI({
    plotOutput("plot", height = "480px")
  })

  output$plot <- renderPlot({ chart_r() })

  # ===========================================================
  # MAP TAB — ggplot2 + geom_sf choropleth
  # No leaflet / raster / terra — deploys cleanly on shinyapps.io.
  # ===========================================================

  # ---- Join year-band CSV to sf census tract shapefiles ----
  # merge() on an sf object works like a regular data-frame merge;
  # the geometry column is carried along automatically.
  map_data_r <- reactive({
    req(input$map_band)
    if (is.null(map_tracts)) return(NULL)

    csv_path <- BAND_FILES[input$map_band]
    if (!file.exists(csv_path)) return(NULL)

    df <- read.csv(csv_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    df$svy_geoid <- as.character(df$svy_geoid)

    # all.x = FALSE keeps only tracts that appear in the BAS CSV
    merge(map_tracts, df, by.x = "GEOID", by.y = "svy_geoid", all.x = FALSE)
  })

  # ---- Choropleth — native plotly choropleth trace (coordinate-aware zoom/pan) ----
  # ggplotly(geom_sf) converts polygons to SVG layout shapes, which are NOT
  # coordinate-aware, so zoom/pan does nothing.  plot_ly(type="choropleth") with
  # a real GeoJSON object IS coordinate-aware and gives working zoom/pan.
  output$bas_map <- renderPlotly({
    data <- map_data_r()
    shiny::validate(
      shiny::need(!is.null(data),
                  "Map data could not be loaded. Check that map_tracts.rds is in the data/ folder.")
    )
    req(input$map_measure)

    measure_col <- input$map_measure

    measure_titles <- c(
      tsi6_mean      = "Transportation Insecurity",
      fsi6_mean      = "Food Insecurity",
      nhd_cohes_mean = "Neighborhood Cohesion"
    )

    # Build hover tooltip — one string per tract row
    income_fmt <- ifelse(
      is.na(data$median_income_2024), "N/A",
      paste0("$", formatC(data$median_income_2024, format = "d", big.mark = ","))
    )
    hover_txt <- paste0(
      "<b>", data$region, " — Tract ", data$GEOID, "</b>",
      "<br>", measure_titles[measure_col], ": <b>",
      round(data[[measure_col]], 2), "</b>",
      "<br>Poverty rate: ",  round(data$poverty_rate_2024  * 100, 1), "%",
      "<br>% Black: ",       round(data$prop_black_2024    * 100, 1), "%",
      "<br>Median income: ", income_fmt
    )

    # Reproject to WGS84 — required by plotly's geo engine
    data_wgs <- sf::st_transform(data, 4326)

    # Write to temp GeoJSON and read back as a parsed list for plotly.
    # sf::st_write uses GDAL's GeoJSON driver; jsonlite is already loaded by the app.
    tmp_geo <- tempfile(fileext = ".geojson")
    sf::st_write(data_wgs, tmp_geo, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
    geojson_obj <- jsonlite::read_json(tmp_geo)

    # YlOrRd: yellow (low) → red (high)  — good for insecurity (high = bad = red)
    # Blues:  light (low) → dark (high)  — good for cohesion  (high = strong = dark)
    colorscale <- if (measure_col == "nhd_cohes_mean") "Blues" else "YlOrRd"

    plot_ly(
      type         = "choropleth",
      geojson      = geojson_obj,
      featureidkey = "properties.GEOID",   # match locations to GeoJSON feature property
      locations    = data_wgs$GEOID,
      z            = data_wgs[[measure_col]],
      text         = hover_txt,
      hoverinfo    = "text",
      colorscale   = colorscale,
      reversescale = FALSE,
      marker       = list(line = list(color = "white", width = 0.5)),
      colorbar     = list(title = list(text = measure_titles[measure_col]))
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>Baltimore Area: ", measure_titles[measure_col], "</b>",
            "<br><sup>BAS weighted estimates by census tract — ",
            input$map_band, "</sup>"
          ),
          x    = 0.02,
          font = list(size = 14, color = "#1c2331")
        ),
        geo = list(
          showframe      = FALSE,
          showcoastlines = FALSE,
          showland       = FALSE,
          fitbounds      = "locations",  # auto-zoom to Baltimore bbox on first render
          visible        = FALSE         # hide the base-map background
        ),
        paper_bgcolor = "white",
        margin        = list(t = 70, r = 10, b = 20, l = 10)
      )
  })

  # ---- Downloads ----
  dl_stem <- reactive({
    req(input$row_var, col_var_actual(), input$year)
    rv <- sub("^bas[0-9]+_", "", input$row_var)
    cv <- if (col_var_actual() == "__OVERALL__") "overall" else
            sub("^bas[0-9]+_", "", col_var_actual())
    paste0("bas", input$year, "_", rv, "_x_", cv)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0(dl_stem(), ".csv"),
    content  = function(file) {
      m       <- table_matrix_r()
      df      <- as.data.frame.matrix(m)
      # Ensure no blank column headers (empty factor levels produce "" in colnames)
      cnames  <- colnames(df)
      cnames[is.na(cnames) | !nzchar(trimws(cnames))] <- "Unknown"
      colnames(df) <- cnames
      row_lbl <- get_var_label(input$row_var, LABELS[[input$year]])
      out <- cbind(
        setNames(data.frame(rownames(df), stringsAsFactors = FALSE), row_lbl),
        df
      )
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$download_png <- downloadHandler(
    filename = function() paste0(dl_stem(), ".png"),
    content  = function(file) {
      ggsave(file, plot = chart_r(), device = "png",
             width = 10, height = 5, dpi = 150, bg = "white")
    }
  )
}

shinyApp(ui, server)

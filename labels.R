# ============================================================
# labels.R — Human-readable variable labels from the codebook
# ============================================================
# Parses docs/bas-YYYY/search_index.json for each survey year.
# Extracts: a short display label per variable AND the topic group.
#
# HOW THE LABELS ARE DERIVED
# Each variable in the codebook appears as:
#   SHORT ALL-CAPS HEADING   basYY_var_name   Question: ...
# We split the codebook text at every variable name, then look at the
# text immediately BEFORE it to find the last all-caps phrase —
# that phrase IS the short label (e.g. "YEARS IN NEIGHBORHOOD").
# We title-case it for display ("Years in Neighborhood").
#
# HOW GROUPS ARE DERIVED
# The second segment of the variable name encodes the topic:
#   bas23_NHD_nyrs  →  nhd  →  "Neighborhoods"
# This mapping is consistent across all years.
# ============================================================

library(jsonlite)

# ---- Topic group mapping (variable prefix → display name) ----
TOPIC_MAP <- c(
  nhd = "Neighborhoods",
  con = "Connectivity",
  hlt = "Health",
  fin = "Finance & Employment",
  env = "Environment",
  dem = "Demographics",
  svy = "Survey Variables"
)

# ---- Manual label overrides (variable suffix → display label) ----
# Used when the codebook assigns the same heading to two different variables.
# Keys are WITHOUT the basYY_ prefix so the override applies across all years.
LABEL_OVERRIDES <- c(
  "con_mdtrust" = "Trust in Maryland State Government",
  "con_dctrust" = "Trust in Federal Government"
)

# Return the display group for a variable like "bas23_nhd_nyrs"
var_topic <- function(varname) {
  parts <- strsplit(varname, "_", fixed = TRUE)[[1]]
  if (length(parts) < 2) return("Other")
  code  <- parts[2]
  # Food insecurity variables are coded under "fin" in some years but belong
  # in Health (e.g. bas24_fin_fsirunout).
  if (code == "fin" && length(parts) >= 3 && startsWith(parts[3], "fsi")) {
    return("Health")
  }
  grp   <- TOPIC_MAP[code]
  if (is.na(grp)) "Other" else unname(grp)
}

# ---- Locate the codebook body text inside the parsed JSON ----
# The JSON is an array of [url, toc, body] arrays.
# fromJSON() returns a character matrix (rows = pages, cols = [url,toc,body]).
extract_codebook_body <- function(raw) {
  if (is.matrix(raw)) {
    for (i in seq_len(nrow(raw))) {
      if (grepl("codebook", raw[i, 1], ignore.case = TRUE))
        return(raw[i, 3])
    }
  } else if (is.list(raw)) {
    for (entry in raw) {
      url  <- if (is.list(entry)) entry[[1]] else entry[1]
      body <- if (is.list(entry)) entry[[3]] else entry[3]
      if (!is.null(url) && grepl("codebook", url, ignore.case = TRUE))
        return(body)
    }
  }
  NULL
}

# ---- Extract the short label from the text preceding a variable name ----
# Strategy: the all-caps phrase immediately before the variable name IS the label.
extract_label_from_text <- function(pre_text) {
  # We do NOT pre-strip "Source: DCAS 2018 Q6" citations because a greedy
  # [^.]+ regex also eats the label that immediately follows (e.g.
  # "NEIGHBORHOOD SATISFACTION" right after "Q6" with no period between them).
  # The multi-word pattern below handles this correctly: codes like "DCAS",
  # "Q6", "GSS" don't form a multi-word all-caps match because the token that
  # follows them ("2018", a digit) doesn't start with an uppercase letter.

  # Look for multi-word all-caps phrases (the codebook short headings).
  # Character class allows: letters, &, /, (, ), ', -, : for labels like
  # "FOOD INSECURITY: FOOD RUNS OUT" or "RACE (BLACK, WHITE, OTHER RACE)".
  multi_pat <- "[A-Z][A-Z&/()'\\-:]*(\\s+[A-Z][A-Z0-9&/()'\\-:,]*)+"
  found <- regmatches(pre_text, gregexpr(multi_pat, pre_text, perl = TRUE))[[1]]
  found <- trimws(found[nchar(trimws(found)) >= 4])

  if (length(found) == 0) {
    # Fallback: single all-caps word ≥ 5 letters (e.g. "ENTREPRENEUR")
    single_pat <- "[A-Z]{5,}"
    found <- regmatches(pre_text, gregexpr(single_pat, pre_text, perl = TRUE))[[1]]
  }

  if (length(found) == 0) return(NA_character_)

  # The LAST match is closest to the variable name = the actual label.
  # Strip trailing punctuation that can appear when a label ends with a
  # closing parenthesis, e.g. "OTHER RACE)" from "RACE (BLACK, WHITE, OTHER RACE)".
  raw <- trimws(tail(found, 1))
  raw <- trimws(gsub("[).,;:]+$", "", raw))
  if (!nzchar(raw)) return(NA_character_)

  # Strip a leading single-letter token that crept in from unit suffixes in the
  # codebook (e.g. "K" from "$50K INCOME" → matched as "K INCOME").
  # A real label word always has ≥ 2 letters.
  raw <- trimws(sub("^[A-Z]\\s+", "", raw))
  if (!nzchar(raw)) return(NA_character_)

  lbl <- tools::toTitleCase(tolower(raw))

  # Strip leading articles (a, an, the) that toTitleCase keeps lowercase —
  # these creep in when a codebook sentence ends with "...activity a WELL-RESTED".
  lbl <- trimws(sub("^(a|an|the)\\s+", "", lbl))
  lbl
}

# ---- Main parser: one year → named character vector (varname → label) ----
parse_codebook_labels <- function(year) {
  path <- file.path("docs", paste0("bas-", year), "search_index.json")
  if (!file.exists(path)) {
    message("labels.R: no codebook index found for year ", year, " — raw names will be used.")
    return(character(0))
  }

  raw <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  if (is.null(raw)) return(character(0))

  cb_body <- extract_codebook_body(raw)
  if (is.null(cb_body) || !nzchar(cb_body)) return(character(0))

  yy          <- substr(as.character(year), 3, 4)
  full_prefix <- paste0("bas", yy, "_")

  # CODEBOOK FORMAT CHANGED BETWEEN YEARS:
  #   2023: full names appear directly, e.g. "bas23_nhd_nyrs"
  #   2024+: uses "Variable name: nhd_nyrs" (short form, no year prefix)
  # Check for the explicit "Variable name:" tag FIRST because the 2024 codebook
  # also contains a handful of bas24_xxx cross-references in notes, which would
  # confuse a check-for-full-prefix-first approach.
  if (grepl("Variable name:", cb_body)) {
    # 2024+ format: strip "Variable name: " and prepend basYY_
    anchor_pat <- "Variable name:\\s+[a-z][a-z0-9_]+"
    to_varname <- function(m) paste0(full_prefix, trimws(sub("Variable name:\\s*", "", m)))
  } else if (grepl(paste0(full_prefix, "[a-z]"), cb_body)) {
    # 2023 format
    anchor_pat <- paste0(full_prefix, "[a-z][a-z0-9_]+")
    to_varname <- function(m) m
  } else {
    message("labels.R: unrecognised codebook format for year ", year)
    return(character(0))
  }

  # Split the codebook body at every anchor occurrence.
  # chunks[i]  = text BEFORE anchor i (contains the short all-caps label)
  # anchors[i] = matched anchor text (used to derive the full variable name)
  chunks   <- strsplit(cb_body, anchor_pat)[[1]]
  anchors  <- regmatches(cb_body, gregexpr(anchor_pat, cb_body))[[1]]
  varnames <- sapply(anchors, to_varname, USE.NAMES = FALSE)

  out        <- character(length(varnames))
  names(out) <- varnames

  for (i in seq_along(varnames)) {
    if (i > length(chunks)) break
    lbl    <- extract_label_from_text(chunks[i])
    out[i] <- if (!is.na(lbl) && nzchar(lbl)) lbl else varnames[i]
  }

  # Variable names appearing more than once (cross-referenced in notes) produce
  # duplicates; R's named-vector lookup returns the FIRST match, which is the
  # actual definition, so no explicit deduplication is needed.

  # Apply manual overrides — fixes variables where the codebook gives the same
  # heading to two distinct questions (e.g. mdtrust and dctrust in 2025).
  for (suffix in names(LABEL_OVERRIDES)) {
    full_name <- paste0(full_prefix, suffix)
    if (full_name %in% names(out)) out[full_name] <- LABEL_OVERRIDES[suffix]
  }

  out
}

# ---- Build grouped choices list for Shiny's selectInput() ----
# varnames: character vector of variables to include
# labels  : named character vector from parse_codebook_labels()
# Returns : list(GroupName = c("Display label" = "varname", ...), ...)
make_grouped_choices <- function(varnames, labels) {
  groups <- list()
  for (v in varnames) {
    grp <- var_topic(v)
    lbl <- labels[v]
    if (is.na(lbl) || !nzchar(lbl)) lbl <- v   # fallback to raw name
    groups[[grp]] <- c(groups[[grp]], setNames(v, lbl))
  }

  # Return groups in a meaningful canonical order
  canonical <- c(unname(TOPIC_MAP), "Other")
  result <- list()
  for (g in canonical) {
    if (!is.null(groups[[g]])) result[[g]] <- groups[[g]]
  }
  result
}

# ---- Convenience: look up one variable's display label ----
get_var_label <- function(varname, year_labels) {
  lbl <- year_labels[[varname]]
  if (is.null(lbl) || is.na(lbl) || !nzchar(lbl)) varname else lbl
}

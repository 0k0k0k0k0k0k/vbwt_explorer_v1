# build_site_species_month_outputs.R

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# FILE PATHS
# -----------------------------
project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

checklists_file <- file.path(processed_dir, "checklists_processed.rds")
observations_file <- file.path(processed_dir, "observations_processed.rds")

# -----------------------------
# CHECK FILES EXIST
# -----------------------------
required_files <- c(checklists_file, observations_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

# -----------------------------
# HELPERS
# -----------------------------
first_non_missing <- function(x) {
  y <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(y) == 0) {
    if (is.character(x)) return(NA_character_)
    if (is.integer(x)) return(NA_integer_)
    if (is.numeric(x)) return(NA_real_)
    if (inherits(x, "IDate")) return(as.IDate(NA))
    if (is.logical(x)) return(NA)
    return(NA)
  }
  y[1]
}

safe_divide <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

write_rds <- function(x, filename) {
  saveRDS(x, file.path(processed_dir, filename))
}

# -----------------------------
# READ INPUTS
# -----------------------------
message("Reading processed inputs...")
checklists_processed <- as.data.table(readRDS(checklists_file))
observations_processed <- as.data.table(readRDS(observations_file))

# -----------------------------
# VALIDATE REQUIRED COLUMNS
# -----------------------------
required_checklist_cols <- c(
  "effort_id", "locality_id", "locality", "observer_id",
  "observation_date", "year", "month",
  "complete_checklist", "vabba2_flag"
)

required_observation_cols <- c(
  "effort_id", "locality_id", "locality", "observer_id",
  "observation_date", "year", "month",
  "common_name", "scientific_name", "species_code",
  "category", "vabba2_flag"
)

missing_checklist_cols <- setdiff(required_checklist_cols, names(checklists_processed))
missing_observation_cols <- setdiff(required_observation_cols, names(observations_processed))

if (length(missing_checklist_cols) > 0) {
  stop(
    "checklists_processed.rds is missing required columns: ",
    paste(missing_checklist_cols, collapse = ", ")
  )
}

if (length(missing_observation_cols) > 0) {
  stop(
    "observations_processed.rds is missing required columns: ",
    paste(missing_observation_cols, collapse = ", ")
  )
}

# -----------------------------
# CLEAN INPUTS
# -----------------------------
checklists_processed[, effort_id := trimws(as.character(effort_id))]
checklists_processed[, locality_id := trimws(as.character(locality_id))]
checklists_processed[, locality := as.character(locality)]
checklists_processed[, observer_id := trimws(as.character(observer_id))]

observations_processed[, effort_id := trimws(as.character(effort_id))]
observations_processed[, locality_id := trimws(as.character(locality_id))]
observations_processed[, locality := as.character(locality)]
observations_processed[, observer_id := trimws(as.character(observer_id))]
observations_processed[, common_name := as.character(common_name)]
observations_processed[, scientific_name := as.character(scientific_name)]
observations_processed[, species_code := as.character(species_code)]
observations_processed[, category := as.character(category)]

if (!inherits(checklists_processed$observation_date, "IDate")) {
  checklists_processed[, observation_date := as.IDate(observation_date)]
}

if (!inherits(observations_processed$observation_date, "IDate")) {
  observations_processed[, observation_date := as.IDate(observation_date)]
}

# -----------------------------
# FILTER OBSERVATIONS ONCE
# KEEP: species, issf, form, hybrid, domestic
# DROP: spuh, slash
# -----------------------------
obs_keep <- observations_processed[
  !is.na(common_name) &
    trimws(common_name) != "" &
    category %in% c("species", "issf", "form", "hybrid", "domestic")
]

# -----------------------------
# SITE-MONTH DENOMINATORS
# -----------------------------
site_month_denoms <- checklists_processed[, .(
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_unique_observers = uniqueN(observer_id),
  first_date = {
    d <- observation_date[!is.na(observation_date)]
    if (length(d) == 0) as.IDate(NA) else min(d)
  },
  last_date = {
    d <- observation_date[!is.na(observation_date)]
    if (length(d) == 0) as.IDate(NA) else max(d)
  }
), by = .(locality_id, locality, year, month)]

setorder(site_month_denoms, locality, year, month)
write_rds(site_month_denoms, "site_month_denoms.rds")

# -----------------------------
# SITE-SPECIES-MONTH METRICS
# -----------------------------
site_species_month_metrics <- obs_keep[, .(
  scientific_name = first_non_missing(scientific_name),
  species_code = first_non_missing(species_code),
  category = first_non_missing(category),
  n_records = .N,
  n_checklists_with_species = uniqueN(effort_id),
  n_observers = uniqueN(observer_id),
  n_vabba2_records = sum(vabba2_flag, na.rm = TRUE),
  first_date = {
    d <- observation_date[!is.na(observation_date)]
    if (length(d) == 0) as.IDate(NA) else min(d)
  },
  last_date = {
    d <- observation_date[!is.na(observation_date)]
    if (length(d) == 0) as.IDate(NA) else max(d)
  }
), by = .(locality_id, locality, year, month, common_name)]

# -----------------------------
# CORRECT COMPLETE CHECKLIST NUMERATOR
# -----------------------------
complete_with_species <- obs_keep[
  checklists_processed[complete_checklist %in% TRUE, .(effort_id, locality_id, year, month)],
  on = .(effort_id, locality_id, year, month),
  nomatch = 0
][, .(
  n_complete_checklists_with_species = uniqueN(effort_id)
), by = .(locality_id, locality, year, month, common_name)]

site_species_month_metrics <- merge(
  site_species_month_metrics,
  complete_with_species,
  by = c("locality_id", "locality", "year", "month", "common_name"),
  all.x = TRUE,
  sort = FALSE
)

site_species_month_metrics[is.na(n_complete_checklists_with_species),
                           n_complete_checklists_with_species := 0]

site_species_month_metrics <- merge(
  site_species_month_metrics,
  site_month_denoms,
  by = c("locality_id", "locality", "year", "month"),
  all.x = TRUE,
  sort = FALSE
)

site_species_month_metrics[, detection_rate_all := safe_divide(n_checklists_with_species, n_checklists)]
site_species_month_metrics[, detection_rate_complete := safe_divide(
  n_complete_checklists_with_species,
  n_complete_checklists
)]
site_species_month_metrics[, detection_rate_vabba2 := safe_divide(n_vabba2_records, n_vabba2_checklists)]

setorder(
  site_species_month_metrics,
  common_name, locality, year, month
)

write_rds(site_species_month_metrics, "site_species_month_metrics.rds")

# -----------------------------
# OPTIONAL QA SUMMARY
# -----------------------------
qa_summary <- data.table(
  metric = c(
    "n_site_month_rows",
    "n_site_species_month_rows",
    "n_obs_rows_kept",
    "n_unique_sites",
    "n_unique_taxa"
  ),
  value = c(
    nrow(site_month_denoms),
    nrow(site_species_month_metrics),
    nrow(obs_keep),
    uniqueN(site_species_month_metrics$locality_id),
    uniqueN(site_species_month_metrics$common_name)
  )
)

write_rds(qa_summary, "site_species_month_outputs_qa_summary.rds")

message("Done.")
message("Site-month denominator rows: ", nrow(site_month_denoms))
message("Site-species-month rows: ", nrow(site_species_month_metrics))
message("Observation rows kept: ", nrow(obs_keep))
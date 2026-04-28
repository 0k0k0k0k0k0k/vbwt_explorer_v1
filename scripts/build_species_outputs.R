# build_species_outputs.R

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# FILE PATHS
# -----------------------------
project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

vbwt_checklists_file <- file.path(processed_dir, "vbwt_checklists.rds")
observations_file <- file.path(processed_dir, "observations_processed.rds")
vbwt_sites_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean_with_urls.csv"

# -----------------------------
# CHECK FILES EXIST
# -----------------------------
required_files <- c(vbwt_checklists_file, observations_file, vbwt_sites_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# HELPERS
# -----------------------------
remove_duplicate_cols <- function(dt, label) {
  if (anyDuplicated(names(dt))) {
    dupes <- names(dt)[duplicated(names(dt))]
    message("Removing duplicate ", label, " columns: ", paste(unique(dupes), collapse = ", "))
    
    dt <- dt[
      ,
      !duplicated(names(dt)),
      with = FALSE
    ]
  }
  
  dt
}

clean_names_dt <- function(dt) {
  old_names <- names(dt)
  new_names <- toupper(trimws(old_names))
  new_names <- gsub("[^A-Z0-9]+", "_", new_names)
  setnames(dt, old = old_names, new = new_names)
  remove_duplicate_cols(dt, "cleaned")
}

first_non_missing <- function(x) {
  y <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(y) == 0) return(NA_character_)
  as.character(y[1])
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

checklists_processed <- as.data.table(readRDS(vbwt_checklists_file))
observations_processed <- as.data.table(readRDS(observations_file))
vbwt_sites <- fread(vbwt_sites_file)

checklists_processed <- remove_duplicate_cols(checklists_processed, "checklist")
observations_processed <- remove_duplicate_cols(observations_processed, "observation")
vbwt_sites <- clean_names_dt(vbwt_sites)

# -----------------------------
# VALIDATE REQUIRED COLUMNS
# -----------------------------
required_checklist_cols <- c(
  "effort_id", "checklist_id", "observer_id", "locality_id", "locality",
  "observation_date", "year", "month", "complete_checklist", "vabba2_flag"
)

required_observation_cols <- c(
  "effort_id", "locality_id", "locality", "observer_id",
  "observation_date", "year", "month",
  "common_name", "scientific_name", "species_code",
  "category", "vabba2_flag"
)

required_site_cols <- c("HOTSPOT_ID", "SITE_NAME", "LATITUDE", "LONGITUDE")

missing_checklist_cols <- setdiff(required_checklist_cols, names(checklists_processed))
missing_observation_cols <- setdiff(required_observation_cols, names(observations_processed))
missing_site_cols <- setdiff(required_site_cols, names(vbwt_sites))

if (length(missing_checklist_cols) > 0) {
  stop(
    "vbwt_checklists.rds is missing required columns: ",
    paste(missing_checklist_cols, collapse = ", ")
  )
}

if (length(missing_observation_cols) > 0) {
  stop(
    "observations_processed.rds is missing required columns: ",
    paste(missing_observation_cols, collapse = ", ")
  )
}

if (length(missing_site_cols) > 0) {
  stop(
    "vbwt_sites_clean_with_urls.csv is missing required columns: ",
    paste(missing_site_cols, collapse = ", ")
  )
}

# -----------------------------
# CLEAN INPUTS
# -----------------------------
checklists_processed[, locality_id := trimws(as.character(locality_id))]
checklists_processed[, checklist_id := trimws(as.character(checklist_id))]
checklists_processed[, effort_id := trimws(as.character(effort_id))]
checklists_processed[, observer_id := trimws(as.character(observer_id))]
checklists_processed[, locality := as.character(locality)]

observations_processed[, locality_id := trimws(as.character(locality_id))]
observations_processed[, effort_id := trimws(as.character(effort_id))]
observations_processed[, observer_id := trimws(as.character(observer_id))]
observations_processed[, locality := as.character(locality)]
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

vbwt_sites[, HOTSPOT_ID := trimws(as.character(HOTSPOT_ID))]
vbwt_sites[, SITE_NAME := trimws(as.character(SITE_NAME))]
vbwt_sites[, LATITUDE := suppressWarnings(as.numeric(LATITUDE))]
vbwt_sites[, LONGITUDE := suppressWarnings(as.numeric(LONGITUDE))]

if ("VBWT_URL" %in% names(vbwt_sites)) {
  vbwt_sites[, VBWT_URL := trimws(as.character(VBWT_URL))]
} else {
  vbwt_sites[, VBWT_URL := NA_character_]
}

vbwt_sites <- vbwt_sites[
  !is.na(HOTSPOT_ID) & HOTSPOT_ID != ""
]

vbwt_sites <- unique(vbwt_sites[, .(
  locality_id = HOTSPOT_ID,
  site_name = SITE_NAME,
  site_latitude = LATITUDE,
  site_longitude = LONGITUDE,
  vbwt_url = VBWT_URL
)], by = "locality_id")

# -----------------------------
# FILTER OBSERVATIONS TO VBWT CHECKLISTS ONLY
# -----------------------------
vbwt_efforts <- unique(checklists_processed[, .(effort_id, locality_id)])

observations_processed <- merge(
  observations_processed,
  vbwt_efforts,
  by = c("effort_id", "locality_id"),
  all = FALSE,
  sort = FALSE
)

# -----------------------------
# FILTER OBSERVATIONS ONCE
# KEEP ALL NORMAL SPECIES
# PLUS TARGET WARBLERS
# EXCLUDE TEALS
# -----------------------------
target_warbler_taxa <- c(
  "Blue-winged Warbler",
  "Brewster's Warbler (hybrid)",
  "Golden-winged Warbler",
  "Golden-winged/Blue-winged Warbler",
  "Lawrence's Warbler (hybrid)",
  "Golden-winged x Blue-winged Warbler (hybrid)"
)

exclude_species <- c(
  "Blue-winged Teal",
  "Blue-winged/Cinnamon Teal",
  "Blue-winged x Cinnamon Teal (hybrid)"
)

obs_keep <- observations_processed[
  !is.na(common_name) &
    trimws(common_name) != "" &
    (
      category %in% c("species", "issf", "form", "hybrid", "domestic") |
        common_name %in% target_warbler_taxa
    ) &
    !common_name %in% exclude_species
]

# -----------------------------
# SITE LOOKUP
# -----------------------------
site_lookup <- checklists_processed[, .(
  locality = first_non_missing(locality),
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
), by = locality_id]

site_lookup <- merge(
  vbwt_sites,
  site_lookup,
  by = "locality_id",
  all.x = TRUE,
  sort = FALSE
)

setorder(site_lookup, -n_checklists, site_name)
write_rds(site_lookup, "site_lookup.rds")

# -----------------------------
# CHECKLIST DENOMINATORS
# -----------------------------
site_denoms <- checklists_processed[, .(
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_unique_observers = uniqueN(observer_id)
), by = locality_id]

year_denoms <- checklists_processed[, .(
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_sites = uniqueN(locality_id),
  n_unique_observers = uniqueN(observer_id)
), by = year]

month_denoms <- checklists_processed[, .(
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_sites = uniqueN(locality_id),
  n_unique_observers = uniqueN(observer_id)
), by = .(year, month)]

site_year_denoms <- checklists_processed[, .(
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_unique_observers = uniqueN(observer_id)
), by = .(locality_id, year)]

# -----------------------------
# SPECIES LOOKUP
# -----------------------------
species_lookup <- obs_keep[, .(
  scientific_name = first_non_missing(scientific_name),
  species_code = first_non_missing(species_code),
  category = first_non_missing(category),
  n_records = .N,
  n_checklists = uniqueN(effort_id),
  n_sites = uniqueN(locality_id),
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
), by = common_name]

setorder(species_lookup, -n_checklists, common_name)
write_rds(species_lookup, "species_lookup.rds")

# -----------------------------
# SITE-SPECIES METRICS
# -----------------------------
site_species_metrics <- obs_keep[, .(
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
), by = .(locality_id, locality, common_name)]

site_species_metrics <- merge(
  site_species_metrics,
  site_denoms,
  by = "locality_id",
  all.x = TRUE,
  sort = FALSE
)

site_species_metrics[, detection_rate_all := safe_divide(n_checklists_with_species, n_checklists)]
site_species_metrics[, detection_rate_complete := safe_divide(n_checklists_with_species, n_complete_checklists)]
site_species_metrics[, detection_rate_vabba2 := safe_divide(n_vabba2_records, n_vabba2_checklists)]

setorder(site_species_metrics, locality, -detection_rate_complete, -n_checklists_with_species, common_name)
write_rds(site_species_metrics, "site_species_metrics.rds")

# -----------------------------
# SPECIES-YEAR METRICS
# -----------------------------
species_year_metrics <- obs_keep[, .(
  scientific_name = first_non_missing(scientific_name),
  species_code = first_non_missing(species_code),
  category = first_non_missing(category),
  n_records = .N,
  n_checklists_with_species = uniqueN(effort_id),
  n_sites_with_species = uniqueN(locality_id),
  n_observers = uniqueN(observer_id),
  n_vabba2_records = sum(vabba2_flag, na.rm = TRUE)
), by = .(year, common_name)]

species_year_metrics <- merge(
  species_year_metrics,
  year_denoms,
  by = "year",
  all.x = TRUE,
  sort = FALSE
)

species_year_metrics[, detection_rate_all := safe_divide(n_checklists_with_species, n_checklists)]
species_year_metrics[, detection_rate_complete := safe_divide(n_checklists_with_species, n_complete_checklists)]
species_year_metrics[, occupancy_rate_sites := safe_divide(n_sites_with_species, n_sites)]

setorder(species_year_metrics, common_name, year)
write_rds(species_year_metrics, "species_year_metrics.rds")

# -----------------------------
# SPECIES-MONTH METRICS
# -----------------------------
species_month_metrics <- obs_keep[, .(
  scientific_name = first_non_missing(scientific_name),
  species_code = first_non_missing(species_code),
  category = first_non_missing(category),
  n_records = .N,
  n_checklists_with_species = uniqueN(effort_id),
  n_sites_with_species = uniqueN(locality_id),
  n_observers = uniqueN(observer_id),
  n_vabba2_records = sum(vabba2_flag, na.rm = TRUE)
), by = .(year, month, common_name)]

species_month_metrics <- merge(
  species_month_metrics,
  month_denoms,
  by = c("year", "month"),
  all.x = TRUE,
  sort = FALSE
)

species_month_metrics[, detection_rate_all := safe_divide(n_checklists_with_species, n_checklists)]
species_month_metrics[, detection_rate_complete := safe_divide(n_checklists_with_species, n_complete_checklists)]
species_month_metrics[, occupancy_rate_sites := safe_divide(n_sites_with_species, n_sites)]

setorder(species_month_metrics, common_name, year, month)
write_rds(species_month_metrics, "species_month_metrics.rds")

# -----------------------------
# SITE-YEAR METRICS
# -----------------------------
site_year_metrics <- obs_keep[, .(
  n_taxa = uniqueN(common_name),
  n_records = .N,
  n_observers = uniqueN(observer_id)
), by = .(locality_id, locality, year)]

site_year_metrics <- merge(
  site_year_metrics,
  site_year_denoms,
  by = c("locality_id", "year"),
  all.x = TRUE,
  sort = FALSE
)

site_year_metrics[, avg_taxa_per_checklist := safe_divide(n_records, n_checklists)]
setorder(site_year_metrics, locality, year)
write_rds(site_year_metrics, "site_year_metrics.rds")

# -----------------------------
# SPECIES-SITE RANKINGS
# -----------------------------
species_site_rankings <- site_species_metrics[, .(
  locality_id,
  locality,
  common_name,
  scientific_name,
  species_code,
  category,
  n_checklists_with_species,
  n_checklists,
  n_complete_checklists,
  detection_rate_all,
  detection_rate_complete,
  first_date,
  last_date
)]

setorder(
  species_site_rankings,
  common_name,
  -detection_rate_complete,
  -n_checklists_with_species,
  locality
)

species_site_rankings[, rank_within_species := seq_len(.N), by = common_name]
write_rds(species_site_rankings, "species_site_rankings.rds")

# -----------------------------
# OPTIONAL QA SUMMARY
# -----------------------------
species_outputs_qa_summary <- data.table(
  metric = c(
    "n_sites_in_lookup",
    "n_species_in_lookup",
    "n_site_species_rows",
    "n_species_year_rows",
    "n_species_month_rows",
    "n_site_year_rows",
    "n_obs_rows_kept"
  ),
  value = c(
    nrow(site_lookup),
    nrow(species_lookup),
    nrow(site_species_metrics),
    nrow(species_year_metrics),
    nrow(species_month_metrics),
    nrow(site_year_metrics),
    nrow(obs_keep)
  )
)

write_rds(species_outputs_qa_summary, "species_outputs_qa_summary.rds")

message("Done.")
message("Sites in lookup: ", nrow(site_lookup))
message("Species in lookup: ", nrow(species_lookup))
message("Site-species rows: ", nrow(site_species_metrics))
message("Species-year rows: ", nrow(species_year_metrics))
message("Species-month rows: ", nrow(species_month_metrics))
message("Observation rows kept: ", nrow(obs_keep))
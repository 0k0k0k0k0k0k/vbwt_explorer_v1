# build_vbwt_data.R

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# FILE PATHS
# -----------------------------
processed_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_explorer_v1/data_processed"
vbwt_sites_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean_with_urls.csv"

checklists_file <- file.path(processed_dir, "checklists_processed.rds")

# -----------------------------
# CHECK FILES EXIST
# -----------------------------
if (!file.exists(checklists_file)) {
  stop("Missing file: ", checklists_file)
}

if (!file.exists(vbwt_sites_file)) {
  stop("Missing file: ", vbwt_sites_file)
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# READ INPUTS
# -----------------------------
checklists_processed <- as.data.table(readRDS(checklists_file))
vbwt_sites <- fread(vbwt_sites_file)

# -----------------------------
# FIX DUPLICATED COLUMN NAMES
# -----------------------------
if (anyDuplicated(names(checklists_processed))) {
  dupes <- names(checklists_processed)[duplicated(names(checklists_processed))]
  message("Removing duplicate checklist columns: ", paste(unique(dupes), collapse = ", "))
  
  checklists_processed <- checklists_processed[
    ,
    !duplicated(names(checklists_processed)),
    with = FALSE
  ]
}

if (anyDuplicated(names(vbwt_sites))) {
  dupes <- names(vbwt_sites)[duplicated(names(vbwt_sites))]
  message("Removing duplicate VBWT site columns: ", paste(unique(dupes), collapse = ", "))
  
  vbwt_sites <- vbwt_sites[
    ,
    !duplicated(names(vbwt_sites)),
    with = FALSE
  ]
}

# -----------------------------
# VALIDATE REQUIRED COLUMNS
# -----------------------------
required_checklist_cols <- c(
  "locality_id",
  "checklist_id",
  "observer_id",
  "observation_date",
  "complete_checklist",
  "vabba2_flag"
)

missing_checklist_cols <- setdiff(required_checklist_cols, names(checklists_processed))
if (length(missing_checklist_cols) > 0) {
  stop(
    "checklists_processed is missing required columns: ",
    paste(missing_checklist_cols, collapse = ", ")
  )
}

required_site_cols <- c("hotspot_id", "site_name", "latitude", "longitude")
missing_site_cols <- setdiff(required_site_cols, names(vbwt_sites))
if (length(missing_site_cols) > 0) {
  stop(
    "vbwt_sites_clean_with_urls.csv is missing required columns: ",
    paste(missing_site_cols, collapse = ", ")
  )
}

# -----------------------------
# ADD effort_id IF MISSING
# -----------------------------
if (!"effort_id" %in% names(checklists_processed)) {
  checklists_processed[, effort_id := as.character(checklist_id)]
}

# -----------------------------
# CLEAN VBWT SITES
# -----------------------------
vbwt_sites[, hotspot_id := trimws(as.character(hotspot_id))]
vbwt_sites[, site_name := trimws(as.character(site_name))]
vbwt_sites[, latitude := suppressWarnings(as.numeric(latitude))]
vbwt_sites[, longitude := suppressWarnings(as.numeric(longitude))]

vbwt_sites <- vbwt_sites[
  !is.na(hotspot_id) & hotspot_id != ""
]

vbwt_sites <- unique(vbwt_sites, by = "hotspot_id")

# -----------------------------
# CLEAN CHECKLIST KEYS
# -----------------------------
checklists_processed[, locality_id := trimws(as.character(locality_id))]
checklists_processed[, checklist_id := trimws(as.character(checklist_id))]
checklists_processed[, effort_id := trimws(as.character(effort_id))]
checklists_processed[, observer_id := trimws(as.character(observer_id))]

# -----------------------------
# FILTER DATE (>= 2002-01-01)
# -----------------------------
cutoff_date <- as.IDate("2002-01-01")

if (!inherits(checklists_processed$observation_date, "IDate")) {
  checklists_processed[, observation_date := as.IDate(observation_date)]
}

checklists_processed <- checklists_processed[
  !is.na(observation_date) & observation_date >= cutoff_date
]

# -----------------------------
# SUBSET TO VBWT CHECKLISTS
# -----------------------------
vbwt_sites_join <- vbwt_sites[, .(
  locality_id = hotspot_id,
  vbwt_site_name = site_name,
  vbwt_latitude = latitude,
  vbwt_longitude = longitude
)]

vbwt_checklists <- merge(
  checklists_processed,
  vbwt_sites_join,
  by = "locality_id",
  all = FALSE,
  sort = FALSE
)

# -----------------------------
# DATE SAFETY
# -----------------------------
if (!inherits(vbwt_checklists$observation_date, "IDate")) {
  vbwt_checklists[, observation_date := as.IDate(observation_date)]
}

# -----------------------------
# SAVE VBWT CHECKLISTS
# -----------------------------
setorder(vbwt_checklists, observation_date, vbwt_site_name, checklist_id)

saveRDS(vbwt_checklists, file.path(processed_dir, "vbwt_checklists.rds"))
fwrite(vbwt_checklists, file.path(processed_dir, "vbwt_checklists.csv"))

# -----------------------------
# HELPERS
# -----------------------------
first_non_missing <- function(x) {
  y <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(y) == 0) return(NA_character_)
  as.character(y[1])
}

# -----------------------------
# BUILD VBWT SITE SUMMARY
# -----------------------------
vbwt_site_summary <- vbwt_checklists[, .(
  site_name = first_non_missing(vbwt_site_name),
  latitude = suppressWarnings(as.numeric(first_non_missing(vbwt_latitude))),
  longitude = suppressWarnings(as.numeric(first_non_missing(vbwt_longitude))),
  n_checklists = uniqueN(effort_id),
  n_complete_checklists = uniqueN(effort_id[complete_checklist %in% TRUE]),
  n_vabba2_checklists = uniqueN(effort_id[vabba2_flag %in% TRUE]),
  n_unique_observers = uniqueN(observer_id),
  first_date = suppressWarnings(min(observation_date, na.rm = TRUE)),
  last_date = suppressWarnings(max(observation_date, na.rm = TRUE))
), by = locality_id]

vbwt_site_summary[is.infinite(first_date), first_date := as.IDate(NA)]
vbwt_site_summary[is.infinite(last_date), last_date := as.IDate(NA)]

setorder(vbwt_site_summary, -n_checklists, site_name)

saveRDS(vbwt_site_summary, file.path(processed_dir, "vbwt_site_summary.rds"))
fwrite(vbwt_site_summary, file.path(processed_dir, "vbwt_site_summary.csv"))

# -----------------------------
# OPTIONAL QA SUMMARY
# -----------------------------
qa_summary <- data.table(
  metric = c(
    "n_vbwt_sites",
    "n_vbwt_checklists",
    "n_unique_vbwt_hotspots",
    "n_complete_checklists",
    "n_vabba2_checklists"
  ),
  value = c(
    nrow(vbwt_sites),
    uniqueN(vbwt_checklists$effort_id),
    uniqueN(vbwt_checklists$locality_id),
    uniqueN(vbwt_checklists$effort_id[vbwt_checklists$complete_checklist %in% TRUE]),
    uniqueN(vbwt_checklists$effort_id[vbwt_checklists$vabba2_flag %in% TRUE])
  )
)

saveRDS(qa_summary, file.path(processed_dir, "vbwt_qa_summary.rds"))
fwrite(qa_summary, file.path(processed_dir, "vbwt_qa_summary.csv"))

message("Done.")
message("VBWT checklists: ", uniqueN(vbwt_checklists$effort_id))
message("VBWT hotspots matched: ", uniqueN(vbwt_checklists$locality_id))
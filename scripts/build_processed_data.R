# build_processed_data_vbwt_early_filter.R

suppressPackageStartupMessages({
  library(data.table)
})

options(datatable.fread.datatable = TRUE)

# -----------------------------
# FILE PATHS
# -----------------------------
project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"

sampling_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/EBD_Mar2026/ebd_US-VA_200201_202604_unv_smp_relMar-2026_sampling.txt"

observations_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/EBD_Mar2026/ebd_US-VA_200201_202604_unv_smp_relMar-2026.txt"

vbwt_sites_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean.csv"
output_dir <- file.path(project_dir, "data_processed")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# SETTINGS
# -----------------------------
cutoff_date <- as.IDate("2002-01-01")

# -----------------------------
# HELPERS
# -----------------------------
clean_names_dt <- function(dt) {
  old_names <- names(dt)
  new_names <- toupper(trimws(old_names))
  new_names <- gsub("[^A-Z0-9]+", "_", new_names)
  setnames(dt, old = old_names, new = new_names)
  dt
}

pick_first_existing <- function(dt, candidates, new_name, default = NA_character_) {
  hit <- candidates[candidates %in% names(dt)][1]
  if (!is.na(hit)) {
    dt[, (new_name) := get(hit)]
  } else {
    dt[, (new_name) := default]
  }
}

safe_date <- function(x) {
  as.IDate(x, format = "%Y-%m-%d")
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

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

safe_max_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num <- x_num[!is.na(x_num)]
  if (length(x_num) == 0) return(NA_real_)
  max(x_num)
}

write_rds <- function(x, filename) {
  saveRDS(x, file.path(output_dir, filename))
}

find_col_index <- function(file_path, col_name) {
  header <- readLines(file_path, n = 1, warn = FALSE)
  cols <- strsplit(header, "\t", fixed = TRUE)[[1]]
  idx <- match(col_name, cols)
  if (is.na(idx)) {
    stop("Column '", col_name, "' not found in file header: ", file_path)
  }
  idx
}

fread_filter_ids <- function(file_path, id_values, id_col_name) {
  if (!file.exists(file_path)) {
    stop("Missing file: ", file_path)
  }
  
  id_values <- unique(trimws(as.character(id_values)))
  id_values <- id_values[!is.na(id_values) & id_values != ""]
  
  if (length(id_values) == 0) {
    stop("No non-missing ID values supplied for filtering: ", id_col_name)
  }
  
  col_idx <- find_col_index(file_path, id_col_name)
  
  ids_file <- tempfile(fileext = ".txt")
  on.exit(unlink(ids_file), add = TRUE)
  writeLines(id_values, ids_file, useBytes = TRUE)
  
  awk_cmd <- sprintf(
    "awk -F'\\t' 'BEGIN{OFS=\"\\t\"} NR==FNR {ids[$1]=1; next} FNR==1 || ($%d in ids)' %s %s",
    col_idx,
    shQuote(ids_file),
    shQuote(file_path)
  )
  
  fread(
    cmd = awk_cmd,
    sep = "\t",
    quote = "",
    showProgress = TRUE
  )
}

# -----------------------------
# READ + CLEAN VBWT SITES
# -----------------------------
message("Reading VBWT sites...")
vbwt_sites <- fread(vbwt_sites_file)
vbwt_sites <- clean_names_dt(vbwt_sites)

required_site_cols <- c("HOTSPOT_ID", "SITE_NAME", "LATITUDE", "LONGITUDE")
missing_site_cols <- setdiff(required_site_cols, names(vbwt_sites))
if (length(missing_site_cols) > 0) {
  stop(
    "vbwt_sites_clean.csv is missing required columns: ",
    paste(missing_site_cols, collapse = ", ")
  )
}

vbwt_sites[, HOTSPOT_ID := trimws(as.character(HOTSPOT_ID))]
vbwt_sites[, SITE_NAME := trimws(as.character(SITE_NAME))]
vbwt_sites[, LATITUDE := safe_numeric(LATITUDE)]
vbwt_sites[, LONGITUDE := safe_numeric(LONGITUDE)]

vbwt_sites <- vbwt_sites[
  !is.na(HOTSPOT_ID) & HOTSPOT_ID != ""
]

vbwt_sites <- unique(vbwt_sites, by = "HOTSPOT_ID")
vbwt_hotspot_ids <- vbwt_sites$HOTSPOT_ID

if (length(vbwt_hotspot_ids) == 0) {
  stop("No valid VBWT hotspot IDs found in: ", vbwt_sites_file)
}

# -----------------------------
# READ SAMPLING FILE
# EARLY FILTER TO VBWT HOTSPOTS
# -----------------------------
message("Reading sampling file filtered to VBWT hotspots...")
chk <- fread_filter_ids(
  file_path = sampling_file,
  id_values = vbwt_hotspot_ids,
  id_col_name = "LOCALITY ID"
)
chk <- clean_names_dt(chk)

# -----------------------------
# STANDARDIZE CHECKLIST FIELDS
# -----------------------------
pick_first_existing(chk, c("SAMPLING_EVENT_IDENTIFIER"), "checklist_id")
pick_first_existing(chk, c("GLOBAL_UNIQUE_IDENTIFIER"), "global_unique_id")
pick_first_existing(chk, c("OBSERVER_ID"), "observer_id")
pick_first_existing(chk, c("GROUP_IDENTIFIER"), "group_id")
pick_first_existing(chk, c("PROJECT_NAMES"), "project_names")
pick_first_existing(chk, c("LOCALITY_ID"), "locality_id")
pick_first_existing(chk, c("LOCALITY"), "locality")
pick_first_existing(chk, c("COUNTY"), "county")
pick_first_existing(chk, c("STATE"), "state")
pick_first_existing(chk, c("COUNTRY"), "country")
pick_first_existing(chk, c("LATITUDE"), "latitude")
pick_first_existing(chk, c("LONGITUDE"), "longitude")
pick_first_existing(chk, c("OBSERVATION_DATE"), "observation_date")
pick_first_existing(chk, c("TIME_OBSERVATIONS_STARTED"), "time_started")
pick_first_existing(chk, c("DURATION_MINUTES"), "duration_minutes")
pick_first_existing(chk, c("EFFORT_DISTANCE_KM"), "effort_distance_km")
pick_first_existing(chk, c("ALL_SPECIES_REPORTED"), "all_species_reported")
pick_first_existing(chk, c("NUMBER_OBSERVERS"), "number_observers")
pick_first_existing(chk, c("APPROVED"), "approved")
pick_first_existing(chk, c("REVIEWED"), "reviewed")

chk[, checklist_id := trimws(as.character(checklist_id))]
chk[, global_unique_id := as.character(global_unique_id)]
chk[, observer_id := as.character(observer_id)]
chk[, group_id := trimws(as.character(group_id))]
chk[, project_names := as.character(project_names)]
chk[, locality_id := trimws(as.character(locality_id))]
chk[, locality := as.character(locality)]
chk[, county := as.character(county)]
chk[, state := as.character(state)]
chk[, country := as.character(country)]
chk[, time_started := as.character(time_started)]
chk[, all_species_reported := as.character(all_species_reported)]
chk[, approved := as.character(approved)]
chk[, reviewed := as.character(reviewed)]

chk[group_id == "", group_id := NA_character_]

chk[, observation_date := safe_date(observation_date)]
chk <- chk[!is.na(observation_date) & observation_date >= cutoff_date]

chk[, year := as.integer(format(observation_date, "%Y"))]
chk[, month := as.integer(format(observation_date, "%m"))]
chk[, latitude := safe_numeric(latitude)]
chk[, longitude := safe_numeric(longitude)]
chk[, duration_minutes := safe_numeric(duration_minutes)]
chk[, effort_distance_km := safe_numeric(effort_distance_km)]
chk[, number_observers := safe_numeric(number_observers)]

chk[, vabba2_flag := !is.na(project_names) & grepl("Virginia Breeding Bird Atlas", project_names, fixed = TRUE)]

chk[, all_species_reported := fifelse(
  is.na(all_species_reported),
  NA_character_,
  toupper(trimws(as.character(all_species_reported)))
)]

chk[, complete_checklist := all_species_reported %in% c("1", "Y", "YES", "TRUE", "T")]

chk[, effort_id := fifelse(
  is.na(group_id),
  as.character(checklist_id),
  as.character(group_id)
)]

checklists_processed <- chk[, .(
  checklist_id,
  global_unique_id,
  observer_id,
  group_id,
  effort_id,
  project_names,
  vabba2_flag,
  locality_id,
  locality,
  county,
  state,
  country,
  latitude,
  longitude,
  observation_date,
  year,
  month,
  time_started,
  duration_minutes,
  effort_distance_km,
  number_observers,
  all_species_reported,
  complete_checklist,
  approved,
  reviewed
)]

# -----------------------------
# COLLAPSE GROUP CHECKLISTS
# -----------------------------
setorder(checklists_processed, effort_id, observation_date, checklist_id)

checklists_processed <- checklists_processed[
  ,
  .(
    checklist_id = first_non_missing(checklist_id),
    global_unique_id = first_non_missing(global_unique_id),
    observer_id = first_non_missing(observer_id),
    group_id = first_non_missing(group_id),
    effort_id = first_non_missing(effort_id),
    project_names = first_non_missing(project_names),
    vabba2_flag = any(vabba2_flag, na.rm = TRUE),
    locality_id = first_non_missing(locality_id),
    locality = first_non_missing(locality),
    county = first_non_missing(county),
    state = first_non_missing(state),
    country = first_non_missing(country),
    latitude = suppressWarnings(as.numeric(first_non_missing(latitude))),
    longitude = suppressWarnings(as.numeric(first_non_missing(longitude))),
    observation_date = {
      d <- observation_date[!is.na(observation_date)]
      if (length(d) == 0) as.IDate(NA) else min(d)
    },
    year = first_non_missing(year),
    month = first_non_missing(month),
    time_started = first_non_missing(time_started),
    duration_minutes = safe_max_numeric(duration_minutes),
    effort_distance_km = safe_max_numeric(effort_distance_km),
    number_observers = safe_max_numeric(number_observers),
    all_species_reported = first_non_missing(all_species_reported),
    complete_checklist = any(complete_checklist %in% TRUE, na.rm = TRUE),
    approved = first_non_missing(approved),
    reviewed = first_non_missing(reviewed)
  ),
  by = effort_id
]

setorder(checklists_processed, observation_date, locality, checklist_id)
write_rds(checklists_processed, "checklists_processed.rds")

# -----------------------------
# READ OBSERVATIONS FILE
# EARLY FILTER TO VBWT HOTSPOTS
# -----------------------------
message("Reading observations file filtered to VBWT hotspots...")
obs <- fread_filter_ids(
  file_path = observations_file,
  id_values = vbwt_hotspot_ids,
  id_col_name = "LOCALITY ID"
)
obs <- clean_names_dt(obs)

# -----------------------------
# STANDARDIZE OBSERVATION FIELDS
# -----------------------------
pick_first_existing(obs, c("SAMPLING_EVENT_IDENTIFIER"), "checklist_id")
pick_first_existing(obs, c("GLOBAL_UNIQUE_IDENTIFIER"), "global_unique_id")
pick_first_existing(obs, c("OBSERVER_ID"), "observer_id")
pick_first_existing(obs, c("GROUP_IDENTIFIER"), "group_id")
pick_first_existing(obs, c("PROJECT_NAMES"), "project_names")
pick_first_existing(obs, c("LOCALITY_ID"), "locality_id")
pick_first_existing(obs, c("LOCALITY"), "locality")
pick_first_existing(obs, c("COUNTY"), "county")
pick_first_existing(obs, c("STATE"), "state")
pick_first_existing(obs, c("COUNTRY"), "country")
pick_first_existing(obs, c("LATITUDE"), "latitude")
pick_first_existing(obs, c("LONGITUDE"), "longitude")
pick_first_existing(obs, c("OBSERVATION_DATE"), "observation_date")
pick_first_existing(obs, c("COMMON_NAME"), "common_name")
pick_first_existing(obs, c("SCIENTIFIC_NAME"), "scientific_name")
pick_first_existing(obs, c("SPECIES_CODE"), "species_code")
pick_first_existing(obs, c("CATEGORY"), "category")
pick_first_existing(obs, c("OBSERVATION_COUNT"), "observation_count")
pick_first_existing(obs, c("BREEDING_CODE"), "breeding_code")
pick_first_existing(obs, c("BREEDING_CATEGORY"), "breeding_category")
pick_first_existing(obs, c("EXOTIC_CODE"), "exotic_code")
pick_first_existing(obs, c("APPROVED"), "approved")
pick_first_existing(obs, c("REVIEWED"), "reviewed")

obs[, checklist_id := trimws(as.character(checklist_id))]
obs[, global_unique_id := as.character(global_unique_id)]
obs[, observer_id := as.character(observer_id)]
obs[, group_id := trimws(as.character(group_id))]
obs[, project_names := as.character(project_names)]
obs[, locality_id := trimws(as.character(locality_id))]
obs[, locality := as.character(locality)]
obs[, county := as.character(county)]
obs[, state := as.character(state)]
obs[, country := as.character(country)]
obs[, common_name := as.character(common_name)]
obs[, scientific_name := as.character(scientific_name)]
obs[, species_code := as.character(species_code)]
obs[, category := as.character(category)]
obs[, observation_count := as.character(observation_count)]
obs[, breeding_code := as.character(breeding_code)]
obs[, breeding_category := as.character(breeding_category)]
obs[, exotic_code := as.character(exotic_code)]
obs[, approved := as.character(approved)]
obs[, reviewed := as.character(reviewed)]

obs[group_id == "", group_id := NA_character_]

obs[, observation_date := safe_date(observation_date)]
obs <- obs[!is.na(observation_date) & observation_date >= cutoff_date]

obs[, year := as.integer(format(observation_date, "%Y"))]
obs[, month := as.integer(format(observation_date, "%m"))]
obs[, latitude := safe_numeric(latitude)]
obs[, longitude := safe_numeric(longitude)]
obs[, observation_count_num := suppressWarnings(as.numeric(observation_count))]
obs[, vabba2_flag := !is.na(project_names) & grepl("Virginia Breeding Bird Atlas", project_names, fixed = TRUE)]

obs[, effort_id := fifelse(
  is.na(group_id),
  as.character(checklist_id),
  as.character(group_id)
)]

observations_processed <- obs[, .(
  checklist_id,
  global_unique_id,
  observer_id,
  group_id,
  effort_id,
  project_names,
  vabba2_flag,
  locality_id,
  locality,
  county,
  state,
  country,
  latitude,
  longitude,
  observation_date,
  year,
  month,
  common_name,
  scientific_name,
  species_code,
  category,
  observation_count,
  observation_count_num,
  breeding_code,
  breeding_category,
  exotic_code,
  approved,
  reviewed
)]

# -----------------------------
# COLLAPSE SHARED CHECKLIST DUPLICATES
# ONE SPECIES PER EFFORT_ID PER SITE
# -----------------------------
setorder(
  observations_processed,
  effort_id, locality_id, common_name, scientific_name, species_code, checklist_id
)

observations_processed <- observations_processed[
  !is.na(common_name) & trimws(common_name) != "",
  .(
    checklist_id = first_non_missing(checklist_id),
    global_unique_id = first_non_missing(global_unique_id),
    observer_id = first_non_missing(observer_id),
    group_id = first_non_missing(group_id),
    effort_id = first_non_missing(effort_id),
    project_names = first_non_missing(project_names),
    vabba2_flag = any(vabba2_flag, na.rm = TRUE),
    locality_id = first_non_missing(locality_id),
    locality = first_non_missing(locality),
    county = first_non_missing(county),
    state = first_non_missing(state),
    country = first_non_missing(country),
    latitude = suppressWarnings(as.numeric(first_non_missing(latitude))),
    longitude = suppressWarnings(as.numeric(first_non_missing(longitude))),
    observation_date = {
      d <- observation_date[!is.na(observation_date)]
      if (length(d) == 0) as.IDate(NA) else min(d)
    },
    year = first_non_missing(year),
    month = first_non_missing(month),
    common_name = first_non_missing(common_name),
    scientific_name = first_non_missing(scientific_name),
    species_code = first_non_missing(species_code),
    category = first_non_missing(category),
    observation_count = first_non_missing(observation_count),
    observation_count_num = safe_max_numeric(observation_count_num),
    breeding_code = first_non_missing(breeding_code),
    breeding_category = first_non_missing(breeding_category),
    exotic_code = first_non_missing(exotic_code),
    approved = first_non_missing(approved),
    reviewed = first_non_missing(reviewed)
  ),
  by = .(effort_id, locality_id, common_name, scientific_name, species_code)
]

setorder(observations_processed, observation_date, common_name, checklist_id)
write_rds(observations_processed, "observations_processed.rds")

# -----------------------------
# QA SUMMARY
# -----------------------------
qa_summary <- data.table(
  metric = c(
    "n_vbwt_sites",
    "n_checklists_processed",
    "n_complete_checklists",
    "n_vabba2_checklists",
    "n_observation_rows_processed",
    "n_unique_species",
    "date_min",
    "date_max"
  ),
  value = c(
    length(vbwt_hotspot_ids),
    uniqueN(checklists_processed$effort_id),
    uniqueN(checklists_processed$effort_id[checklists_processed$complete_checklist %in% TRUE]),
    uniqueN(checklists_processed$effort_id[checklists_processed$vabba2_flag %in% TRUE]),
    nrow(observations_processed),
    uniqueN(observations_processed$common_name),
    as.character(min(checklists_processed$observation_date, na.rm = TRUE)),
    as.character(max(checklists_processed$observation_date, na.rm = TRUE))
  )
)

write_rds(qa_summary, "processed_data_qa_summary.rds")

message("Done.")
message("VBWT hotspots: ", length(vbwt_hotspot_ids))
message("Checklists processed: ", uniqueN(checklists_processed$effort_id))
message("Observation rows processed: ", nrow(observations_processed))
message("Unique species: ", uniqueN(observations_processed$common_name))
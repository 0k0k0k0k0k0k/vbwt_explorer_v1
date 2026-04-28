# append_missing_va_hotspots.R

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# FILE PATHS
# -----------------------------
project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

va_ebd_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/EBD_Mar2026/ebd_US-VA_200201_202604_unv_smp_relMar-2026.txt"

checklists_file <- file.path(processed_dir, "checklists_processed.rds")
observations_file <- file.path(processed_dir, "observations_processed.rds")

# -----------------------------
# TARGET VIRGINIA HOTSPOTS
# -----------------------------
target_hotspots <- c(
  "L718393", # Appomattox Community Park
  "L718635", # Northern Virginia 4-H Educational and Conference Center
  "L718640"  # Town of Occoquan
)

# -----------------------------
# FILTER SETTINGS
# -----------------------------
min_species_per_checklist <- 3
cutoff_date <- as.IDate("2002-01-01")

backup_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

checklists_backup <- file.path(
  processed_dir,
  paste0("checklists_processed_backup_before_missing_va_", backup_stamp, ".rds")
)

observations_backup <- file.path(
  processed_dir,
  paste0("observations_processed_backup_before_missing_va_", backup_stamp, ".rds")
)

# -----------------------------
# HELPERS
# -----------------------------
remove_duplicate_cols <- function(dt, label) {
  if (anyDuplicated(names(dt))) {
    dupes <- names(dt)[duplicated(names(dt))]
    message("Removing duplicate ", label, " columns: ", paste(unique(dupes), collapse = ", "))
    
    dt <- dt[, !duplicated(names(dt)), with = FALSE]
  }
  
  dt
}

find_col <- function(nms, options, required = TRUE) {
  hit <- options[options %in% nms]
  
  if (length(hit) == 0 && required) {
    stop("Could not find required column. Tried: ", paste(options, collapse = ", "))
  }
  
  if (length(hit) == 0) return(NA_character_)
  
  hit[1]
}

as_logical_ebird <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x %in% c("1", "TRUE", "T", "YES", "Y")
}

add_missing_cols <- function(dt, template_names) {
  missing <- setdiff(template_names, names(dt))
  
  for (col in missing) {
    dt[, (col) := NA]
  }
  
  dt[, ..template_names]
}

# -----------------------------
# CHECK FILES EXIST
# -----------------------------
required_files <- c(va_ebd_file, checklists_file, observations_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

# -----------------------------
# READ EXISTING PROCESSED FILES
# -----------------------------
message("Reading existing processed files...")

checklists_existing <- as.data.table(readRDS(checklists_file))
observations_existing <- as.data.table(readRDS(observations_file))

checklists_existing <- remove_duplicate_cols(checklists_existing, "checklist")
observations_existing <- remove_duplicate_cols(observations_existing, "observation")

message("Backing up existing processed files...")
saveRDS(checklists_existing, checklists_backup)
saveRDS(observations_existing, observations_backup)

message("Checklist backup:")
message(checklists_backup)

message("Observation backup:")
message(observations_backup)

# -----------------------------
# READ VA EBD
# -----------------------------
message("Reading VA EBD header...")

header <- fread(
  va_ebd_file,
  sep = "\t",
  quote = "",
  nrows = 0,
  showProgress = FALSE
)

nms <- names(header)

locality_id_col <- find_col(nms, c("LOCALITY ID"))
locality_col <- find_col(nms, c("LOCALITY"))
observer_id_col <- find_col(nms, c("OBSERVER ID"))
date_col <- find_col(nms, c("OBSERVATION DATE"))
effort_col <- find_col(nms, c("SAMPLING EVENT IDENTIFIER"))
complete_col <- find_col(nms, c("ALL SPECIES REPORTED"))
common_col <- find_col(nms, c("COMMON NAME"))
scientific_col <- find_col(nms, c("SCIENTIFIC NAME"))
species_code_col <- find_col(nms, c("SPECIES CODE"), required = FALSE)
category_col <- find_col(nms, c("CATEGORY"))
protocol_col <- find_col(nms, c("PROTOCOL TYPE"), required = FALSE)
project_col <- find_col(nms, c("PROJECT NAMES"), required = FALSE)

read_cols <- unique(na.omit(c(
  locality_id_col,
  locality_col,
  observer_id_col,
  date_col,
  effort_col,
  complete_col,
  common_col,
  scientific_col,
  species_code_col,
  category_col,
  protocol_col,
  project_col
)))

message("Reading selected VA EBD columns...")
va <- fread(
  va_ebd_file,
  sep = "\t",
  quote = "",
  select = read_cols,
  showProgress = TRUE
)

message("Filtering to missing Virginia hotspots...")
va_sites <- va[get(locality_id_col) %in% target_hotspots]

message("Raw VA rows for target hotspots: ", nrow(va_sites))
message("Raw VA checklists for target hotspots: ", uniqueN(va_sites[[effort_col]]))

if (nrow(va_sites) == 0) {
  stop("No VA rows found for target hotspots.")
}

message("Rows by hotspot before filtering:")
print(
  va_sites[, .(
    rows = .N,
    checklists = uniqueN(get(effort_col))
  ), by = get(locality_id_col)]
)

# -----------------------------
# STANDARDIZE VA SITE DATA
# -----------------------------
va_sites[, effort_id := trimws(as.character(get(effort_col)))]
va_sites[, checklist_id := trimws(as.character(get(effort_col)))]
va_sites[, locality_id := trimws(as.character(get(locality_id_col)))]
va_sites[, locality := as.character(get(locality_col))]
va_sites[, observer_id := trimws(as.character(get(observer_id_col)))]
va_sites[, observation_date := as.IDate(get(date_col))]
va_sites[, year := as.integer(format(observation_date, "%Y"))]
va_sites[, month := as.integer(format(observation_date, "%m"))]
va_sites[, complete_checklist := as_logical_ebird(get(complete_col))]
va_sites[, common_name := as.character(get(common_col))]
va_sites[, scientific_name := as.character(get(scientific_col))]
va_sites[, category := as.character(get(category_col))]

if (!is.na(species_code_col)) {
  va_sites[, species_code := as.character(get(species_code_col))]
} else {
  va_sites[, species_code := NA_character_]
}

if (!is.na(project_col)) {
  va_sites[, project_names := as.character(get(project_col))]
  va_sites[, vabba2_flag := grepl("Virginia Breeding Bird Atlas", project_names, fixed = TRUE)]
} else {
  va_sites[, project_names := NA_character_]
  va_sites[, vabba2_flag := FALSE]
}

if (!is.na(protocol_col)) {
  va_sites[, protocol_type := as.character(get(protocol_col))]
} else {
  va_sites[, protocol_type := NA_character_]
}

# -----------------------------
# APPLY BASIC QUALITY FILTERS
# -----------------------------
message("Applying quality filters...")

va_sites <- va_sites[
  !is.na(observation_date) &
    observation_date >= cutoff_date &
    complete_checklist %in% TRUE
]

if (!all(is.na(va_sites$protocol_type))) {
  va_sites <- va_sites[
    !tolower(trimws(protocol_type)) %in% c("incidental")
  ]
}

checklist_species_counts <- va_sites[
  !is.na(common_name) & trimws(common_name) != "",
  .(n_species_checklist = uniqueN(common_name)),
  by = effort_id
]

valid_efforts <- checklist_species_counts[
  n_species_checklist >= min_species_per_checklist,
  effort_id
]

va_sites <- va_sites[effort_id %in% valid_efforts]

message("Filtered VA rows kept: ", nrow(va_sites))
message("Filtered VA checklists kept: ", uniqueN(va_sites$effort_id))

if (nrow(va_sites) == 0) {
  stop("No VA rows remained after filtering.")
}

message("Rows by hotspot after filtering:")
print(
  va_sites[, .(
    rows = .N,
    checklists = uniqueN(effort_id)
  ), by = locality_id]
)

# -----------------------------
# BUILD CHECKLIST ROWS
# -----------------------------
va_checklists <- unique(
  va_sites[, .(
    effort_id,
    checklist_id,
    observer_id,
    locality_id,
    locality,
    observation_date,
    year,
    month,
    complete_checklist,
    vabba2_flag
  )],
  by = "effort_id"
)

# -----------------------------
# BUILD OBSERVATION ROWS
# -----------------------------
va_observations <- va_sites[, .(
  effort_id,
  checklist_id,
  observer_id,
  locality_id,
  locality,
  observation_date,
  year,
  month,
  complete_checklist,
  vabba2_flag,
  common_name,
  scientific_name,
  species_code,
  category
)]

# -----------------------------
# REMOVE OLD TARGET HOTSPOT ROWS IF PRESENT
# -----------------------------
message("Removing any existing rows for target hotspots before append...")

checklists_existing <- checklists_existing[!locality_id %in% target_hotspots]
observations_existing <- observations_existing[!locality_id %in% target_hotspots]

# -----------------------------
# ALIGN COLUMNS TO EXISTING FILES
# -----------------------------
message("Aligning columns...")

va_checklists <- add_missing_cols(va_checklists, names(checklists_existing))
va_observations <- add_missing_cols(va_observations, names(observations_existing))

# -----------------------------
# APPEND AND SAVE
# -----------------------------
message("Appending VA rows...")

checklists_combined <- rbindlist(
  list(checklists_existing, va_checklists),
  use.names = TRUE,
  fill = TRUE
)

observations_combined <- rbindlist(
  list(observations_existing, va_observations),
  use.names = TRUE,
  fill = TRUE
)

message("Saving combined processed files...")

saveRDS(checklists_combined, checklists_file)
saveRDS(observations_combined, observations_file)

message("Done.")
message("Added VA checklists: ", nrow(va_checklists))
message("Added VA observation rows: ", nrow(va_observations))
message("Combined checklist rows: ", nrow(checklists_combined))
message("Combined observation rows: ", nrow(observations_combined))
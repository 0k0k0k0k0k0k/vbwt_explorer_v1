# append_theodore_roosevelt_dc.R

suppressPackageStartupMessages({
  library(data.table)
})

project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

dc_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/EBD_Mar2026/ebd_US-DC_unv_smp_relMar-2026.txt"

checklists_file <- file.path(processed_dir, "checklists_processed.rds")
observations_file <- file.path(processed_dir, "observations_processed.rds")

target_hotspot <- "L718768"

min_species_per_checklist <- 3
cutoff_date <- as.IDate("2002-01-01")

backup_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

checklists_backup <- file.path(processed_dir, paste0("checklists_processed_backup_before_dc_", backup_stamp, ".rds"))
observations_backup <- file.path(processed_dir, paste0("observations_processed_backup_before_dc_", backup_stamp, ".rds"))

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
# READ DC EBD
# -----------------------------
message("Reading DC EBD header...")

header <- fread(
  dc_file,
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

message("Reading selected DC EBD columns...")
dc <- fread(
  dc_file,
  sep = "\t",
  quote = "",
  select = read_cols,
  showProgress = TRUE
)

message("Filtering to Theodore Roosevelt Island...")
dc_site <- dc[get(locality_id_col) == target_hotspot]

message("Raw DC rows for target hotspot: ", nrow(dc_site))
message("Raw DC checklists for target hotspot: ", uniqueN(dc_site[[effort_col]]))

if (nrow(dc_site) == 0) {
  stop("No DC rows found for hotspot: ", target_hotspot)
}

# -----------------------------
# STANDARDIZE DC SITE DATA
# -----------------------------
dc_site[, effort_id := trimws(as.character(get(effort_col)))]
dc_site[, checklist_id := trimws(as.character(get(effort_col)))]
dc_site[, locality_id := trimws(as.character(get(locality_id_col)))]
dc_site[, locality := as.character(get(locality_col))]
dc_site[, observer_id := trimws(as.character(get(observer_id_col)))]
dc_site[, observation_date := as.IDate(get(date_col))]
dc_site[, year := as.integer(format(observation_date, "%Y"))]
dc_site[, month := as.integer(format(observation_date, "%m"))]
dc_site[, complete_checklist := as_logical_ebird(get(complete_col))]
dc_site[, common_name := as.character(get(common_col))]
dc_site[, scientific_name := as.character(get(scientific_col))]
dc_site[, category := as.character(get(category_col))]
dc_site[, vabba2_flag := FALSE]

if (!is.na(species_code_col)) {
  dc_site[, species_code := as.character(get(species_code_col))]
} else {
  dc_site[, species_code := NA_character_]
}

if (!is.na(project_col)) {
  dc_site[, project_names := as.character(get(project_col))]
} else {
  dc_site[, project_names := NA_character_]
}

if (!is.na(protocol_col)) {
  dc_site[, protocol_type := as.character(get(protocol_col))]
} else {
  dc_site[, protocol_type := NA_character_]
}

# -----------------------------
# APPLY BASIC QUALITY FILTERS
# -----------------------------
message("Applying quality filters...")

dc_site <- dc_site[
  !is.na(observation_date) &
    observation_date >= cutoff_date &
    complete_checklist %in% TRUE
]

if (!all(is.na(dc_site$protocol_type))) {
  dc_site <- dc_site[
    !tolower(trimws(protocol_type)) %in% c("incidental")
  ]
}

checklist_species_counts <- dc_site[
  !is.na(common_name) & trimws(common_name) != "",
  .(n_species_checklist = uniqueN(common_name)),
  by = effort_id
]

valid_efforts <- checklist_species_counts[
  n_species_checklist >= min_species_per_checklist,
  effort_id
]

dc_site <- dc_site[effort_id %in% valid_efforts]

message("Filtered DC rows kept: ", nrow(dc_site))
message("Filtered DC checklists kept: ", uniqueN(dc_site$effort_id))

if (nrow(dc_site) == 0) {
  stop("No DC rows remained after filtering.")
}

# -----------------------------
# BUILD CHECKLIST ROWS
# -----------------------------
dc_checklists <- unique(
  dc_site[, .(
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
dc_observations <- dc_site[, .(
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
message("Removing any existing rows for target hotspot before append...")

checklists_existing <- checklists_existing[locality_id != target_hotspot]
observations_existing <- observations_existing[locality_id != target_hotspot]

# -----------------------------
# ALIGN COLUMNS TO EXISTING FILES
# -----------------------------
message("Aligning columns...")

dc_checklists <- add_missing_cols(dc_checklists, names(checklists_existing))
dc_observations <- add_missing_cols(dc_observations, names(observations_existing))

# -----------------------------
# APPEND AND SAVE
# -----------------------------
message("Appending DC rows...")

checklists_combined <- rbindlist(
  list(checklists_existing, dc_checklists),
  use.names = TRUE,
  fill = TRUE
)

observations_combined <- rbindlist(
  list(observations_existing, dc_observations),
  use.names = TRUE,
  fill = TRUE
)

message("Saving combined processed files...")

saveRDS(checklists_combined, checklists_file)
saveRDS(observations_combined, observations_file)

message("Done.")
message("Added DC checklists: ", nrow(dc_checklists))
message("Added DC observation rows: ", nrow(dc_observations))
message("Combined checklist rows: ", nrow(checklists_combined))
message("Combined observation rows: ", nrow(observations_combined))
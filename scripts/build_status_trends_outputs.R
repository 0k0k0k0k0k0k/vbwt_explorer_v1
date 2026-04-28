# build_status_trends_outputs.R

suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# FILE PATHS
# -----------------------------
project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

species_lookup_file <- file.path(processed_dir, "species_lookup.rds")
status_csv_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/VA_CSV/VA_regional_status_2023.csv"
trends_csv_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/VA_CSV/VA_regional_trends_2023.csv"

# -----------------------------
# CHECK FILES EXIST
# -----------------------------
required_files <- c(species_lookup_file, status_csv_file, trends_csv_file)
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
clean_names_dt <- function(dt) {
  old_names <- names(dt)
  new_names <- toupper(trimws(old_names))
  new_names <- gsub("[^A-Z0-9]+", "_", new_names)
  setnames(dt, old = old_names, new = new_names)
  dt
}

write_rds <- function(x, filename) {
  saveRDS(x, file.path(processed_dir, filename))
}

# -----------------------------
# READ INPUTS
# -----------------------------
message("Reading species lookup...")
species_lookup <- as.data.table(readRDS(species_lookup_file))

message("Reading regional status...")
status_dt <- fread(status_csv_file)
status_dt <- clean_names_dt(status_dt)

message("Reading regional trends...")
trends_dt <- fread(trends_csv_file)
trends_dt <- clean_names_dt(trends_dt)

# -----------------------------
# STANDARDIZE KEYS
# -----------------------------
species_lookup[, species_code := trimws(tolower(as.character(species_code)))]
species_lookup[, common_name := trimws(as.character(common_name))]
species_lookup[, scientific_name := trimws(as.character(scientific_name))]

status_dt[, SPECIES_CODE := trimws(tolower(as.character(SPECIES_CODE)))]
status_dt[, COMMON_NAME := trimws(as.character(COMMON_NAME))]
status_dt[, SCIENTIFIC_NAME := trimws(as.character(SCIENTIFIC_NAME))]
status_dt[, STATE_NAME := trimws(as.character(STATE_NAME))]

trends_dt[, SPECIES_CODE := trimws(tolower(as.character(SPECIES_CODE)))]
trends_dt[, COMMON_NAME := trimws(as.character(COMMON_NAME))]
trends_dt[, SCIENTIFIC_NAME := trimws(as.character(SCIENTIFIC_NAME))]
trends_dt[, STATE_NAME := trimws(as.character(STATE_NAME))]
trends_dt[, SEASON_CODE := trimws(as.character(SEASON_CODE))]

# -----------------------------
# KEEP VIRGINIA ONLY
# -----------------------------
status_dt <- status_dt[STATE_NAME == "Virginia"]
trends_dt <- trends_dt[STATE_NAME == "Virginia"]

# -----------------------------
# BUILD STATUS LOOKUP
# -----------------------------
species_status_lookup <- status_dt[, .(
  species_code = SPECIES_CODE,
  common_name = COMMON_NAME,
  scientific_name = SCIENTIFIC_NAME,
  percent_pop_breeding = PERCENT_POP_BREEDING,
  percent_pop_postbreeding_migration = PERCENT_POP_POSTBREEDING_MIGRATION,
  percent_pop_nonbreeding = PERCENT_POP_NONBREEDING,
  percent_pop_prebreeding_migration = PERCENT_POP_PREBREEDING_MIGRATION,
  max_week_season = MAX_WEEK_SEASON,
  max_week = MAX_WEEK,
  max_week_percent_pop = MAX_WEEK_PERCENT_POP,
  state_rank_breeding = STATE_RANK_BREEDING,
  breeding_habitat_primary = BREEDING_HABITAT_PRIMARY,
  breeding_habitat_secondary = BREEDING_HABITAT_SECONDARY
)]

setorder(species_status_lookup, common_name)
write_rds(species_status_lookup, "species_status_lookup.rds")

# -----------------------------
# BUILD TRENDS LOOKUP
# -----------------------------
species_trends_lookup <- trends_dt[, .(
  species_code = SPECIES_CODE,
  common_name = COMMON_NAME,
  scientific_name = SCIENTIFIC_NAME,
  trend_period = TREND_PERIOD,
  season_code = SEASON_CODE,
  state_trend_median = STATE_TREND_MEDIAN,
  state_trend_lowerci = STATE_TREND_LOWERCI,
  state_trend_upperci = STATE_TREND_UPPERCI,
  rangewide_trend_median = RANGEWIDE_TREND_MEDIAN,
  rangewide_trend_lowerci = RANGEWIDE_TREND_LOWERCI,
  rangewide_trend_upperci = RANGEWIDE_TREND_UPPERCI,
  breeding_habitat_primary = BREEDING_HABITAT_PRIMARY,
  breeding_habitat_secondary = BREEDING_HABITAT_SECONDARY
)]

setorder(species_trends_lookup, common_name, season_code)
write_rds(species_trends_lookup, "species_trends_lookup.rds")

# -----------------------------
# BUILD SPECIES PROFILE LOOKUP
# ONE ROW PER SPECIES
# -----------------------------
# Pivot seasonal trend fields wider so you can display them easily in the app
trends_wide <- dcast(
  species_trends_lookup,
  species_code + common_name + scientific_name ~ season_code,
  value.var = c(
    "state_trend_median",
    "state_trend_lowerci",
    "state_trend_upperci",
    "rangewide_trend_median",
    "rangewide_trend_lowerci",
    "rangewide_trend_upperci"
  )
)

species_profile_lookup <- merge(
  species_lookup,
  species_status_lookup,
  by = c("common_name", "scientific_name"),
  all.x = TRUE,
  sort = FALSE
)

species_profile_lookup <- merge(
  species_profile_lookup,
  trends_wide,
  by = c("common_name", "scientific_name"),
  all.x = TRUE,
  sort = FALSE
)

setorder(species_profile_lookup, common_name)
write_rds(species_profile_lookup, "species_profile_lookup.rds")

# -----------------------------
# OPTIONAL: SIMPLE BROAD-SEASON SCORE LABEL
# -----------------------------
species_profile_lookup[, strongest_va_season := fifelse(
  !is.na(max_week_season), as.character(max_week_season), NA_character_
)]

write_rds(species_profile_lookup, "species_profile_lookup.rds")

# -----------------------------
# QA SUMMARY
# -----------------------------
qa_summary <- data.table(
  metric = c(
    "n_species_lookup",
    "n_status_species",
    "n_trend_rows",
    "n_species_profile_rows"
  ),
  value = c(
    nrow(species_lookup),
    nrow(species_status_lookup),
    nrow(species_trends_lookup),
    nrow(species_profile_lookup)
  )
)

write_rds(qa_summary, "status_trends_outputs_qa_summary.rds")

message("Done.")
message("Status species: ", nrow(species_status_lookup))
message("Trend rows: ", nrow(species_trends_lookup))
message("Species profiles: ", nrow(species_profile_lookup))
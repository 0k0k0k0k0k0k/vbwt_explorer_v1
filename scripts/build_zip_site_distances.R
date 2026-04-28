# build_zip_site_distances.R

suppressPackageStartupMessages({
  library(data.table)
  library(geosphere)
})

project_dir <- "/Users/lisamease/Documents/Shiny App Folder/VBWT_Explorer_v1"
processed_dir <- file.path(project_dir, "data_processed")

zip_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/zip_va_centroids.rds"
site_file <- file.path(processed_dir, "site_lookup.rds")

output_rds <- file.path(processed_dir, "zip_site_distances.rds")
output_csv <- file.path(processed_dir, "zip_site_distances.csv")

message("Reading ZIP centroids...")
zip_dt <- as.data.table(readRDS(zip_file))

message("Reading VBWT site lookup...")
sites <- as.data.table(readRDS(site_file))

required_zip_cols <- c("zip", "latitude", "longitude")
required_site_cols <- c("locality_id", "site_name", "site_latitude", "site_longitude")

missing_zip <- setdiff(required_zip_cols, names(zip_dt))
missing_site <- setdiff(required_site_cols, names(sites))

if (length(missing_zip) > 0) {
  stop("ZIP file is missing columns: ", paste(missing_zip, collapse = ", "))
}

if (length(missing_site) > 0) {
  stop("Site lookup is missing columns: ", paste(missing_site, collapse = ", "))
}

zip_dt[, zip := trimws(as.character(zip))]
zip_dt[, latitude := as.numeric(latitude)]
zip_dt[, longitude := as.numeric(longitude)]

sites[, locality_id := trimws(as.character(locality_id))]
sites[, site_name := trimws(as.character(site_name))]
sites[, site_latitude := as.numeric(site_latitude)]
sites[, site_longitude := as.numeric(site_longitude)]

zip_dt <- zip_dt[
  !is.na(zip) &
    zip != "" &
    !is.na(latitude) &
    !is.na(longitude)
]

sites <- sites[
  !is.na(locality_id) &
    locality_id != "" &
    !is.na(site_latitude) &
    !is.na(site_longitude)
]

message("ZIPs available: ", nrow(zip_dt))
message("VBWT sites available: ", nrow(sites))

message("Building ZIP x site table...")

zip_dt[, join_key := 1L]
sites[, join_key := 1L]

distances <- merge(
  zip_dt,
  sites,
  by = "join_key",
  allow.cartesian = TRUE
)

distances[, join_key := NULL]

message("Calculating distances...")

distances[, distance_miles := geosphere::distHaversine(
  cbind(longitude, latitude),
  cbind(site_longitude, site_latitude)
) / 1609.344]

distances[, distance_miles := round(distance_miles, 2)]

keep_cols <- c(
  "zip",
  "latitude",
  "longitude",
  "locality_id",
  "site_name",
  "site_latitude",
  "site_longitude",
  "distance_miles"
)

if ("vbwt_url" %in% names(distances)) {
  keep_cols <- c(keep_cols, "vbwt_url")
}

distances <- distances[, ..keep_cols]

setorder(distances, zip, distance_miles, site_name)

message("Rows created: ", nrow(distances))

saveRDS(distances, output_rds)
fwrite(distances, output_csv)

message("Saved RDS to:")
message(output_rds)

message("Saved CSV to:")
message(output_csv)

message("Done.")
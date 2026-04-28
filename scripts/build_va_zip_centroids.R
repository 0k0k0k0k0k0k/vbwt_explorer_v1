# build_va_zip_centroids.R

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

zcta_dir <- "/Users/lisamease/Documents/Shiny App Folder/Data/tl_2025_us_zcta520"
zip_va_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/zip_va.csv"

output_csv <- "/Users/lisamease/Documents/Shiny App Folder/Data/zip_va_centroids.csv"
output_rds <- "/Users/lisamease/Documents/Shiny App Folder/Data/zip_va_centroids.rds"

message("Finding ZCTA shapefile...")
shp_files <- list.files(zcta_dir, pattern = "\\.shp$", full.names = TRUE)

if (length(shp_files) == 0) {
  stop("No .shp file found in: ", zcta_dir)
}

zcta_shp <- shp_files[1]
message("Using shapefile: ", zcta_shp)

message("Reading Virginia ZIP list...")
zip_va <- fread(zip_va_file)

if (!"DELIVERY ZIPCODE" %in% names(zip_va)) {
  stop("Missing expected column in zip_va.csv: DELIVERY ZIPCODE")
}

zip_va[, zip := sprintf("%05s", trimws(as.character(`DELIVERY ZIPCODE`)))]
zip_va <- unique(zip_va[, .(zip)])

message("Virginia ZIPs in USPS file: ", nrow(zip_va))

message("Reading Census ZCTA polygons...")
zcta <- st_read(zcta_shp, quiet = TRUE)

message("ZCTA columns found:")
print(names(zcta))

zip_col_candidates <- c("ZCTA5CE20", "ZCTA5CE10", "GEOID20", "GEOID10", "GEOID", "ZIP", "ZCTA")
zip_col <- zip_col_candidates[zip_col_candidates %in% names(zcta)]

if (length(zip_col) == 0) {
  stop("Could not find a ZCTA ZIP column. Check printed column names.")
}

zip_col <- zip_col[1]
message("Using ZCTA ZIP column: ", zip_col)

zcta$zip <- sprintf("%05s", trimws(as.character(zcta[[zip_col]])))

message("Filtering ZCTAs to Virginia ZIP list...")
zcta_va <- zcta[zcta$zip %in% zip_va$zip, ]

message("Virginia ZCTA polygons matched: ", nrow(zcta_va))

if (nrow(zcta_va) == 0) {
  stop("No matching ZCTAs found. Check ZIP formatting.")
}

# -----------------------------
# GET LAT/LON
# Prefer Census internal point columns when available.
# Fall back to geometry-based point on surface.
# -----------------------------
lat_col_candidates <- c("INTPTLAT20", "INTPTLAT10", "INTPTLAT")
lon_col_candidates <- c("INTPTLON20", "INTPTLON10", "INTPTLON")

lat_col <- lat_col_candidates[lat_col_candidates %in% names(zcta_va)]
lon_col <- lon_col_candidates[lon_col_candidates %in% names(zcta_va)]

if (length(lat_col) > 0 && length(lon_col) > 0) {
  lat_col <- lat_col[1]
  lon_col <- lon_col[1]
  
  message("Using Census internal point columns:")
  message("Latitude column: ", lat_col)
  message("Longitude column: ", lon_col)
  
  zip_coords <- data.table(
    zip = zcta_va$zip,
    latitude = suppressWarnings(as.numeric(zcta_va[[lat_col]])),
    longitude = suppressWarnings(as.numeric(zcta_va[[lon_col]]))
  )
  
} else {
  message("Census internal point columns not found.")
  message("Falling back to st_point_on_surface().")
  
  zcta_va_projected <- st_transform(zcta_va, 5070)
  zcta_points <- st_point_on_surface(zcta_va_projected)
  zcta_points <- st_transform(zcta_points, 4326)
  
  coords <- st_coordinates(zcta_points)
  
  zip_coords <- data.table(
    zip = zcta_points$zip,
    latitude = coords[, "Y"],
    longitude = coords[, "X"]
  )
}

zip_coords <- zip_coords[
  !is.na(zip) &
    zip != "" &
    !is.na(latitude) &
    !is.na(longitude)
]

zip_coords <- unique(zip_coords, by = "zip")
setorder(zip_coords, zip)

message("ZIP centroid rows created: ", nrow(zip_coords))

missing_zips <- setdiff(zip_va$zip, zip_coords$zip)

message("Virginia ZIPs without matched ZCTA point: ", length(missing_zips))

if (length(missing_zips) > 0) {
  message("First missing ZIPs:")
  print(head(missing_zips, 25))
}

fwrite(zip_coords, output_csv)
saveRDS(zip_coords, output_rds)

message("Saved CSV to:")
message(output_csv)

message("Saved RDS to:")
message(output_rds)

message("Done.")
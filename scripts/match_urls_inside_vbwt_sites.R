# match_urls_inside_vbwt_sites.R

library(data.table)
library(readxl)

# -----------------------------
# FILE PATHS
# -----------------------------
input_xlsx <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean.xlsx"
output_csv <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean_with_urls.csv"

# -----------------------------
# READ FILE
# -----------------------------
vbwt <- as.data.table(read_excel(input_xlsx))

# -----------------------------
# CHECK REQUIRED COLUMNS
# -----------------------------
required_cols <- c("site_name", "vbwt_url")
missing_cols <- setdiff(required_cols, names(vbwt))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# CLEAN URL VALUES
# -----------------------------
vbwt[, site_name := trimws(as.character(site_name))]
vbwt[, vbwt_url := trimws(as.character(vbwt_url))]
vbwt[vbwt_url == "", vbwt_url := NA_character_]

# -----------------------------
# BASIC QA CHECKS
# -----------------------------
qa <- data.table(
  metric = c(
    "total_rows",
    "rows_with_site_name",
    "rows_with_url",
    "rows_missing_url",
    "duplicate_site_names"
  ),
  value = c(
    nrow(vbwt),
    sum(!is.na(vbwt$site_name) & vbwt$site_name != ""),
    sum(!is.na(vbwt$vbwt_url) & vbwt$vbwt_url != ""),
    sum(is.na(vbwt$vbwt_url) | vbwt$vbwt_url == ""),
    sum(duplicated(vbwt$site_name[!is.na(vbwt$site_name) & vbwt$site_name != ""]))
  )
)

print(qa)

# -----------------------------
# SHOW SITES STILL MISSING URLS
# -----------------------------
missing_urls <- vbwt[
  is.na(vbwt_url) | vbwt_url == "",
  .(site_name)
]

if (nrow(missing_urls) > 0) {
  message("Sites missing URLs:")
  print(missing_urls)
}

# -----------------------------
# WRITE CLEAN CSV
# -----------------------------
fwrite(vbwt, output_csv)

message("Done.")
message("Wrote: ", output_csv)
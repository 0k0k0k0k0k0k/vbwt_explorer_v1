# filter_zip.R

library(data.table)
library(readxl)

zip_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/ZIP_Locale_Detail.xls"
output_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/zip_va.csv"

message("Reading ZIP file...")
zip_dt <- as.data.table(read_excel(zip_file))

message("Columns found:")
print(names(zip_dt))

message("First few rows:")
print(head(zip_dt))

state_col <- "PHYSICAL STATE"

if (!state_col %in% names(zip_dt)) {
  stop("Missing expected column: ", state_col)
}

message("Using state column: ", state_col)

zip_va <- zip_dt[
  toupper(trimws(as.character(get(state_col)))) == "VA"
]

message("Virginia ZIP rows found: ", nrow(zip_va))

fwrite(zip_va, output_file)

message("Saved Virginia ZIP file to:")
message(output_file)
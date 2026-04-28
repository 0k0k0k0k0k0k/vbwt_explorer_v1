# extract_vbwt_urls_from_xlsx.R

library(data.table)
library(readxl)
library(xml2)

xlsx_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean.xlsx"
output_file <- "/Users/lisamease/Documents/Shiny App Folder/Data/vbwt_sites_clean_with_urls.csv"

# Read visible spreadsheet data
sites <- as.data.table(read_excel(xlsx_file))

# Unzip xlsx to temp folder
tmp_dir <- tempfile()
dir.create(tmp_dir)
unzip(xlsx_file, exdir = tmp_dir)

# Assume first worksheet
sheet_xml <- file.path(tmp_dir, "xl", "worksheets", "sheet1.xml")
rels_xml <- file.path(tmp_dir, "xl", "worksheets", "_rels", "sheet1.xml.rels")

if (!file.exists(sheet_xml)) {
  stop("Could not find sheet1.xml inside xlsx.")
}

if (!file.exists(rels_xml)) {
  stop("Could not find hyperlink relationship file for sheet1.")
}

# Read hyperlink refs from worksheet XML
sheet_doc <- read_xml(sheet_xml)
rels_doc <- read_xml(rels_xml)

hyperlinks <- xml_find_all(sheet_doc, ".//*[local-name()='hyperlink']")

link_refs <- data.table(
  cell_ref = xml_attr(hyperlinks, "ref"),
  rid = xml_attr(hyperlinks, "id")
)

# Read URL targets from relationships XML
rels <- xml_find_all(rels_doc, ".//*[local-name()='Relationship']")

rel_lookup <- data.table(
  rid = xml_attr(rels, "Id"),
  vbwt_url = xml_attr(rels, "Target")
)

# Join cell refs to URLs
links <- merge(link_refs, rel_lookup, by = "rid", all.x = TRUE)

# Extract row number from cell ref, e.g. A2 -> 2
links[, row := as.integer(gsub("[^0-9]", "", cell_ref))]

# Remove header row
links <- links[row > 1]

# Match Excel row to data row
sites[, row := .I + 1]

sites <- merge(
  sites,
  links[, .(row, vbwt_url)],
  by = "row",
  all.x = TRUE,
  sort = FALSE
)

sites[, row := NULL]

# Write new CSV
fwrite(sites, output_file)

message("Done.")
message("Wrote: ", output_file)
message("Rows with URLs: ", sum(!is.na(sites$vbwt_url)))
# VBWT Explorer v1 - Phase 1 Core App
# Option 5: Combined Species Guide polish: profile card, recent card, and Seasonal Pattern graph card.
# Trial polish: Best-version Species tab with guide chips, chart note, and best nearby site card.
# Recent eBird sightings modify existing VBWT dots: larger dot, orange ring, dark outer outline, and popup details.
# Site rating colors: Best #2A5235, Better #4F7A45, Good #8FAF5A, Low #F1E8B8.
# Recent sightings draw above non-recent dots; recent eBird date links to checklist when available.

library(shiny)
library(qs2)
library(data.table)
library(leaflet)
library(ggplot2)
library(scales)
library(DT)
library(httr)
library(jsonlite)


# ======================
# LOAD DATA -- SETUP
# ======================

site_lookup <- as.data.table(readRDS("data_processed/site_lookup.rds"))
species_lookup <- as.data.table(readRDS("data_processed/species_lookup.rds"))
ebird_species_codes <- as.data.table(
  readRDS("data_processed/ebird_species_codes.rds")
)

# site_species_month is not currently used by the app.
# site_species_month <- as.data.table(readRDS("data_processed/site_species_month_all_year.rds"))

site_species_month_yearly <- as.data.table(qs_read("data_processed/slim/site_species_month_metrics_slim.qs2"))
site_species_date <- as.data.table(qs_read("data_processed/slim/site_species_date_metrics_slim.qs2"))
site_date_denoms <- as.data.table(readRDS("data_processed/site_date_denoms.rds"))

site_species_metrics <- as.data.table(readRDS("data_processed/site_species_metrics.rds"))
zip_site_distances <- as.data.table(qs_read("data_processed/slim/zip_site_distances_slim.qs2"))
profile <- as.data.table(readRDS("data_processed/species_profile_lookup.rds"))

iconic_species_edited_file <- "outputs/iconic_species_candidates_edited.csv"

read_iconic_species_edited <- function(file) {
  
  if (!file.exists(file)) {
    return(character(0))
  }
  
  x <- readLines(file, warn = FALSE)
  x <- trimws(x)
  x <- x[nzchar(x)]
  
  x <- gsub('^"|"$', "", x)
  x <- gsub("^common_name$", "", x, ignore.case = TRUE)
  x <- trimws(x)
  x <- x[nzchar(x)]
  
  # Handles accidental row-number/index exports like:
  # 1,American Barn Owl
  # "1","American Barn Owl"
  x <- sub('^"?[0-9]+"?,', "", x)
  x <- gsub('^"|"$', "", x)
  x <- trimws(x)
  
  unique(x[nzchar(x)])
}

iconic_species_approved <- read_iconic_species_edited(
  iconic_species_edited_file
)

iconic_rarity_flags_file <- "outputs/iconic_species_candidates_with_rarity_chase_flags.csv"

iconic_rarity_excludes <- character(0)

if (file.exists(iconic_rarity_flags_file)) {
  iconic_rarity_flags <- data.table::fread(iconic_rarity_flags_file)
  
  iconic_rarity_excludes <- iconic_rarity_flags[
    suggested_exclude_from_iconic == "yes",
    unique(common_name)
  ]
}

# ======================
# HELPER FUNCTIONS -- SETUP
# ======================

format_peak_week <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  if (grepl("resident", x, ignore.case = TRUE)) return("Year-round")
  
  parts <- strsplit(x, "-")[[1]]
  if (length(parts) != 2) return(x)
  
  month_num <- as.integer(parts[1])
  week_num <- as.integer(parts[2])
  
  month_name <- month.name[month_num]
  week_of_month <- ((week_num - 1) %% 4) + 1
  week_label <- c("Early", "Early", "Mid", "Late")[week_of_month]
  
  paste(week_label, month_name)
}

format_peak_season <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  
  season_labels <- c(
    "prebreeding_migration" = "Spring migration",
    "breeding" = "Breeding season",
    "postbreeding_migration" = "Fall migration",
    "nonbreeding" = "Winter",
    "resident" = "Year-round"
  )
  
  if (x %in% names(season_labels)) return(season_labels[[x]])
  x
}

format_month_as_season <- function(month_num) {
  if (is.na(month_num)) return(NA_character_)
  
  if (month_num %in% c(3, 4, 5)) {
    return("Spring migration")
  }
  
  if (month_num %in% c(6, 7)) {
    return("Breeding season")
  }
  
  if (month_num %in% c(8, 9, 10)) {
    return("Fall migration")
  }
  
  if (month_num %in% c(11, 12, 1, 2)) {
    return("Winter")
  }
  
  NA_character_
}

format_ordinal_week <- function(month_num, day_num) {
  if (
    is.na(month_num) || month_num < 1 || month_num > 12 ||
    is.na(day_num) || day_num < 1 || day_num > 31
  ) {
    return(NA_character_)
  }
  
  week_of_month <- ceiling(day_num / 7)
  
  week_label <- c(
    "1st week of",
    "2nd week of",
    "3rd week of",
    "4th week of",
    "5th week of"
  )[week_of_month]
  
  paste(week_label, month.name[month_num])
}

pluralize_species_for_title <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(x)
  }
  
  irregular_species_plurals <- c(
    "Canada Goose" = "Canada Geese",
    "Snow Goose" = "Snow Geese",
    "Ross's Goose" = "Ross's Geese",
    "Cackling Goose" = "Cackling Geese",
    "Greater White-fronted Goose" = "Greater White-fronted Geese",
    "Brant" = "Brant",
    "Dunlin" = "Dunlin",
    "Sanderling" = "Sanderlings"
  )
  
  if (x %in% names(irregular_species_plurals)) {
    return(irregular_species_plurals[[x]])
  }
  
  last_word <- sub("^.*\\s", "", x)
  
  if (grepl("[^aeiou]y$", last_word, ignore.case = TRUE)) {
    return(sub("y$", "ies", x, ignore.case = TRUE))
  }
  
  if (grepl("(s|x|z|ch|sh)$", last_word, ignore.case = TRUE)) {
    return(paste0(x, "es"))
  }
  
  paste0(x, "s")
}

filter_by_month_day_range <- function(dt, date_col, start_date, end_date) {
  
  start_md <- as.integer(format(as.Date(start_date), "%m%d"))
  end_md <- as.integer(format(as.Date(end_date), "%m%d"))
  
  dt[, month_day_filter_value := as.integer(format(as.Date(get(date_col)), "%m%d"))]
  
  if (start_md <= end_md) {
    out <- dt[
      month_day_filter_value >= start_md &
        month_day_filter_value <= end_md
    ]
  } else {
    out <- dt[
      month_day_filter_value >= start_md |
        month_day_filter_value <= end_md
    ]
  }
  
  out[, month_day_filter_value := NULL]
  
  out
}


# ======================
# RECENT EBIRD API -- HELPERS
# ======================

get_ebird_species_code <- function(selected_common_name) {
  
  if (
    is.null(selected_common_name) ||
    selected_common_name == ""
  ) {
    return(NA_character_)
  }
  
  out <- ebird_species_codes[
    common_name == selected_common_name,
    unique(ebird_species_code)
  ]
  
  out <- out[!is.na(out) & out != ""]
  
  if (length(out) == 0) {
    return(NA_character_)
  }
  
  out[1]
}
fetch_recent_ebird_observations <- function(
    species_code,
    lat,
    lng,
    radius_miles,
    days_back,
    vbwt_locality_ids = character(0)
) {
  
  ebird_api_key <- Sys.getenv("EBIRD_API_KEY")
  
  if (ebird_api_key == "") {
    return(list(
      data = data.table(),
      message = "Set EBIRD_API_KEY to show recent eBird sightings."
    ))
  }
  
  if (
    is.na(species_code) ||
    species_code == "" ||
    is.na(lat) ||
    is.na(lng)
  ) {
    return(list(
      data = data.table(),
      message = "Recent sightings are unavailable for this species or ZIP code."
    ))
  }
  
  radius_km <- min(radius_miles * 1.609344, 50)
  days_back <- max(1, min(as.integer(days_back), 30))
  
  response <- tryCatch(
    httr::GET(
      url = paste0(
        "https://api.ebird.org/v2/data/obs/geo/recent/",
        species_code
      ),
      httr::add_headers("X-eBirdApiToken" = ebird_api_key),
      query = list(
        lat = round(lat, 2),
        lng = round(lng, 2),
        dist = round(radius_km, 1),
        back = days_back,
        hotspot = "true",
        includeProvisional = "false",
        maxResults = 200
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(response)) {
    return(list(
      data = data.table(),
      message = "Recent eBird sightings could not be loaded."
    ))
  }
  
  if (httr::status_code(response) != 200) {
    return(list(
      data = data.table(),
      message = paste0(
        "Recent eBird sightings could not be loaded. API status: ",
        httr::status_code(response),
        "."
      )
    ))
  }
  
  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, flatten = TRUE),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    !is.data.frame(parsed) ||
    nrow(parsed) == 0
  ) {
    return(list(
      data = data.table(),
      message = paste0(
        "No recent eBird sightings found in the last ",
        days_back,
        " days."
      )
    ))
  }
  
  dt <- as.data.table(parsed)
  
  required_cols <- c(
    "locId",
    "locName",
    "obsDt",
    "lat",
    "lng",
    "howMany",
    "subId"
  )
  
  for (col in required_cols) {
    if (!col %in% names(dt)) {
      dt[, (col) := NA]
    }
  }
  
  vbwt_locality_ids <- unique(as.character(vbwt_locality_ids))
  vbwt_locality_ids <- vbwt_locality_ids[
    !is.na(vbwt_locality_ids) &
      vbwt_locality_ids != ""
  ]
  
  dt[, locId := as.character(locId)]
  
  if (length(vbwt_locality_ids) == 0) {
    return(list(
      data = data.table(),
      message = "No VBWT sites were found in this search area."
    ))
  }
  
  dt <- dt[
    !is.na(locId) &
      locId %in% vbwt_locality_ids
  ]
  
  if (nrow(dt) == 0) {
    return(list(
      data = data.table(),
      message = paste0(
        "No recent eBird sightings at VBWT sites found in the last ",
        days_back,
        " days."
      )
    ))
  }
  
  dt <- dt[
    !is.na(lat) &
      !is.na(lng)
  ]
  
  if (nrow(dt) == 0) {
    return(list(
      data = data.table(),
      message = "No mappable recent eBird sightings were returned."
    ))
  }
  
  dt[, obs_date := as.Date(substr(obsDt, 1, 10))]
  dt[, checklist_url := ifelse(
    !is.na(subId) & subId != "",
    paste0("https://ebird.org/checklist/", subId),
    NA_character_
  )]
  
  dt <- dt[
    order(
      -obs_date,
      locName
    )
  ]
  
  list(
    data = dt,
    message = NULL
  )
}

# ======================
# UI -- UI INPUT PREP -- SETUP
# ======================

valid_species <- profile[
  !is.na(percent_pop_breeding) &
    percent_pop_breeding >= 0.01,
  unique(common_name)
]

species_choices <- c("Select a species..." = "", sort(valid_species))
month_choices <- c("Select a month..." = "", setNames(1:12, month.name))
year_choices <- sort(unique(site_species_month_yearly$year))

date_min <- as.Date("2000-01-01")
date_max <- as.Date("2000-12-31")

current_month <- as.integer(format(Sys.Date(), "%m"))

date_default_start <- as.Date(paste0(
  "2000-",
  sprintf("%02d", current_month),
  "-01"
))

date_default_end <- if (current_month == 12) {
  as.Date("2000-12-31")
} else {
  as.Date(paste0(
    "2000-",
    sprintf("%02d", current_month + 1),
    "-01"
  )) - 1
}

# ======================
# UI -- UI
# ======================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .app-wrap {
        display: flex;
        gap: 20px;
        align-items: flex-start;
        width: 100%;
      }

      .selectize-control .selectize-input {
        font-size: 13px;
      }

      #map .leaflet-tile {
        filter: saturate(150%) contrast(110%) hue-rotate(-15deg);
      }

      #map-message {
        position: absolute;
        top: 50%;
        left: 50%;
        text-align: center;
        transform: translate(-50%, -50%);
        background: rgba(255,255,255,0.9);
        padding: 12px 18px;
        border-radius: 6px;
        font-size: 14px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.2);
        z-index: 1000;
      }

      .selectize-dropdown {
        font-size: 13px;
      }

      .fixed-sidebar {
        flex: 0 0 280px !important;
        width: 280px !important;
        min-width: 280px !important;
        max-width: 280px !important;
      }

      .legend-toggle {
        cursor: pointer;
      }


      .recent-ebird-ring-legend i {
        background: transparent !important;
        border: 3px solid #F36C21 !important;
        border-radius: 50% !important;
        box-sizing: border-box !important;
        opacity: 0.8 !important;
      }

      #rank_table table.dataTable th,
      #rank_table table.dataTable td,
      #iconic_table table.dataTable th,
      #iconic_table table.dataTable td {
        text-align: left !important;
      }

      .datepicker .datepicker-switch {
        pointer-events: none !important;
        cursor: default !important;
      }

      .datepicker-years,
      .datepicker-months,
      .datepicker-decades,
      .datepicker-centuries {
        display: none !important;
      }

      .selectize-control {
        width: 100% !important;
      }

      .selectize-control.single .selectize-input {
        padding-right: 8px !important;
      }

      #zip + .selectize-control.single .selectize-input:after {
        display: none !important;
      }

      .main-content {
        flex: 1 1 auto;
        min-width: 0;
      }

      @media (max-width: 900px) {
        .app-wrap {
          display: block;
        }

        #map-message {
          background: rgba(255,255,255,0.95);
          padding: 14px 20px;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.15);
          max-width: 260px;
          line-height: 1.4;
        }

        .fixed-sidebar {
          width: 100% !important;
          min-width: 100% !important;
          max-width: 100% !important;
          margin-bottom: 15px;
        }
      }
      ")),
    
    tags$script(HTML("
      function cleanDatepickerHeader() {
        $('.datepicker-days .datepicker-switch').each(function() {
          var txt = $(this).text().trim();
          var cleaned = txt.replace(/\\s+\\d{4}$/, '');
          if (txt !== cleaned) {
            $(this).text(cleaned);
          }
          });
        }

        $(document).on('focus click', '#date_range input', function() {
          setTimeout(cleanDatepickerHeader, 0);
          setTimeout(cleanDatepickerHeader, 50);
          });

          $(document).on('click', '.datepicker .prev, .datepicker .next', function() {
            setTimeout(cleanDatepickerHeader, 0);
            setTimeout(cleanDatepickerHeader, 50);
            });

            $(document).ready(function() {
              var observer = new MutationObserver(function() {
                cleanDatepickerHeader();
                });

                observer.observe(document.body, {
                  childList: true,
                  subtree: true
                  });
                  });
                  "))
  ),
  
  titlePanel("VBWT Explorer v1"),
  
  div(
    class = "app-wrap",
    
    # ======================
    # SIDEBAR -- UI
    # ======================
    
    div(
      class = "fixed-sidebar",
      wellPanel(
        
        selectizeInput(
          "zip",
          "ZIP Code",
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Type ZIP code and press Enter",
            create = TRUE,
            persist = FALSE,
            onInitialize = I("
          function() {
            var self = this;
            this.$control_input.on('keydown', function(e) {
              if (e.keyCode === 13) {
                e.preventDefault();
                var val = self.$control_input.val();
                if (val.length > 0) {
                  self.addOption({value: val, text: val});
                  self.setValue(val);
                }
              }
              });
            }
            ")
          )
        ),
        
        selectizeInput(
          "species",
          "Select Species",
          choices = species_choices,
          selected = "",
          options = list(placeholder = "Type species name")
        ),
        
        selectInput(
          "month",
          "Month",
          choices = month_choices,
          selected = as.character(current_month)
        ),
        
        dateRangeInput(
          "date_range",
          "Date Range",
          start = date_default_start,
          end = date_default_end,
          min = date_min,
          max = date_max,
          format = "M d",
          separator = " to ",
          startview = "month",
          autoclose = FALSE
        ),
        
        sliderInput("radius", "Search Radius (miles)", 5, 150, 25, 5),
        
        tags$div(
          style = "font-weight: 700; margin-bottom: 6px;",
          "Recent eBird Sightings"
        ),
        
        checkboxInput(
          "show_recent_sightings",
          "Show recent eBird sightings",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.show_recent_sightings == true",
          sliderInput(
            "recent_days",
            "Days Back",
            min = 1,
            max = 14,
            value = 14,
            step = 1
          ),
          uiOutput("recent_sightings_status")
        ),
        
        actionButton("reset", "Reset Filters", width = "100%")
      )
    ),
    
    # ======================
    # MAIN PANEL -- UI
    # ======================
    
    div(
      class = "main-content",
      tabsetPanel(
        tabPanel(
          "Map",
          div(
            style = "position: relative;",
            leafletOutput("map", height = 550),
            uiOutput("map_message")
          )
        ),
        
        tabPanel(
          "Species Guide",
          div(
            style = "max-width: 900px; margin: 0 auto;",
            uiOutput("profile_box")
          )
        ),
        
        tabPanel(
          "Recommended Sites",
          uiOutput("best_site_box"),
          
          uiOutput("recent_sightings_box"),
          
          tags$hr(),
          
          uiOutput("rank_table_title"),
          DTOutput("rank_table")
        ),
        
        tabPanel(
          "Notable Species",
          uiOutput("iconic_table_title"),
          DTOutput("iconic_table")
        )
      )
    )
  )
)

# ======================
# SERVER -- SERVER
# ======================

server <- function(input, output, session) {
  
  total_checks <- sum(site_lookup$n_complete_checklists, na.rm = TRUE)
  
  species_baseline <- site_species_metrics[
    ,
    .(
      vbwt_detection_rate_complete =
        sum(n_checklists_with_species, na.rm = TRUE) / total_checks
    ),
    by = common_name
  ]
  
  observeEvent(input$month, {
    
    if (is.null(input$month) || input$month == "") {
      return()
    }
    
    selected_month <- as.integer(input$month)
    
    new_start <- as.Date(paste0(
      "2000-",
      sprintf("%02d", selected_month),
      "-01"
    ))
    
    new_end <- if (selected_month == 12) {
      as.Date("2000-12-31")
    } else {
      as.Date(paste0(
        "2000-",
        sprintf("%02d", selected_month + 1),
        "-01"
      )) - 1
    }
    
    updateDateRangeInput(
      session,
      "date_range",
      start = new_start,
      end = new_end
    )
  })
  
  observeEvent(input$date_range, {
    
    req(input$date_range)
    
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    
    if (
      is.na(start_date) ||
      is.na(end_date)
    ) {
      return()
    }
    
    if (start_date > end_date) {
      updateDateRangeInput(
        session,
        "date_range",
        start = end_date,
        end = start_date
      )
    }
  }, ignoreInit = TRUE)
  
  # ======================
  # FILTERED DATA -- SERVER
  # ======================
  
  filtered_sites <- reactive({
    
    req(input$zip, input$species, input$date_range)
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) {
      return(data.table())
    }
    
    date_start <- as.Date(input$date_range[1])
    date_end <- as.Date(input$date_range[2])
    
    denom_dt <- site_date_denoms[
      locality_id %in% nearby$locality_id
    ]
    
    denom_dt <- filter_by_month_day_range(
      denom_dt,
      "observation_date",
      date_start,
      date_end
    )
    
    denom_dt <- denom_dt[
      ,
      .(
        n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE)
      ),
      by = locality_id
    ]
    
    species_dt <- site_species_date[
      common_name == input$species &
        locality_id %in% nearby$locality_id
    ]
    
    species_dt <- filter_by_month_day_range(
      species_dt,
      "observation_date",
      date_start,
      date_end
    )
    
    species_dt <- species_dt[
      ,
      .(
        n_checklists_with_species =
          sum(n_complete_checklists_with_species, na.rm = TRUE)
      ),
      by = .(locality_id, common_name)
    ]
    
    dt_counts <- merge(
      denom_dt,
      species_dt,
      by = "locality_id",
      all.x = TRUE,
      sort = FALSE
    )
    
    dt_counts[is.na(common_name), common_name := input$species]
    dt_counts[is.na(n_checklists_with_species), n_checklists_with_species := 0]
    
    dt_counts[
      ,
      detection_rate_complete :=
        n_checklists_with_species / pmax(n_complete_checklists, 1)
    ]
    
    dt_counts <- dt_counts[
      n_complete_checklists >= 50 &
        n_checklists_with_species >= 10
    ]
    
    if (nrow(dt_counts) == 0) {
      return(data.table())
    }
    
    dt <- merge(
      nearby,
      dt_counts,
      by = "locality_id",
      all.x = FALSE,
      sort = FALSE
    )
    
    if (nrow(dt) == 0) {
      return(data.table())
    }
    
    dt <- merge(
      dt,
      species_baseline,
      by = "common_name",
      all.x = TRUE,
      sort = FALSE
    )
    
    dt[, app_detection_rate := detection_rate_complete]
    dt[, app_complete_checklists := n_complete_checklists]
    dt[, app_complete_checklists_with_species := n_checklists_with_species]
    dt[, recommendation_score :=
         (app_complete_checklists_with_species + 2) /
         (app_complete_checklists + 20)]
    
    dt[, relative_detection :=
         ifelse(
           vbwt_detection_rate_complete > 0,
           app_detection_rate / vbwt_detection_rate_complete,
           NA_real_
         )]
    
    dt[, display_name := site_name]
    dt[is.na(display_name) | display_name == "", display_name := locality_id]
    
    dt
  })
  
  # ======================
  # RECENT EBIRD SIGHTINGS -- SERVER
  # ======================
  
  recent_sightings <- reactive({
    
    req(input$zip, input$species, input$recent_days)
    
    zip_point <- zip_site_distances[
      zip == input$zip
    ][1]
    
    if (
      nrow(zip_point) == 0 ||
      is.na(zip_point$latitude) ||
      is.na(zip_point$longitude)
    ) {
      return(list(
        data = data.table(),
        message = "Recent sightings need a valid ZIP code."
      ))
    }
    
    nearby_vbwt_sites <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    species_code <- get_ebird_species_code(input$species)
    
    fetch_recent_ebird_observations(
      species_code = species_code,
      lat = zip_point$latitude,
      lng = zip_point$longitude,
      radius_miles = input$radius,
      days_back = input$recent_days,
      vbwt_locality_ids = nearby_vbwt_sites$locality_id
    )
  })
  
  output$recent_sightings_status <- renderUI({
    
    if (!isTRUE(input$show_recent_sightings)) {
      return(NULL)
    }
    
    recent <- recent_sightings()
    
    if (!is.null(recent$message)) {
      return(tags$p(
        style = "font-size: 12px; color: #666666; margin-top: -4px;",
        recent$message
      ))
    }
    
    radius_note <- if (input$radius > 31) {
      " eBird limits recent sightings to about 31 miles."
    } else {
      ""
    }
    
    tags$p(
      style = "font-size: 12px; color: #666666; margin-top: -4px;",
      paste0(
        nrow(recent$data),
        " recent sightings found.",
        radius_note
      )
    )
  })
  
  # ======================
  # ICONIC SPECIES -- SERVER
  # ======================
  
  iconic_species <- reactive({
    
    req(input$zip, input$date_range)
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$date_range)
    ) {
      return(data.table())
    }
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) {
      return(data.table())
    }
    
    date_start <- as.Date(input$date_range[1])
    date_end <- as.Date(input$date_range[2])
    
    denom_dt <- site_date_denoms[
      locality_id %in% nearby$locality_id
    ]
    
    denom_dt <- filter_by_month_day_range(
      denom_dt,
      "observation_date",
      date_start,
      date_end
    )
    
    site_denoms <- denom_dt[
      ,
      .(
        site_complete_checklists =
          sum(n_complete_checklists, na.rm = TRUE)
      ),
      by = locality_id
    ]
    
    if (nrow(site_denoms) == 0) {
      return(data.table())
    }
    
    dt <- site_species_date[
      locality_id %in% nearby$locality_id
    ]
    
    if (length(iconic_species_approved) > 0) {
      dt <- dt[
        common_name %in% iconic_species_approved
      ]
    }
    
    dt <- dt[
      !grepl("\\bsp\\.$", common_name, ignore.case = TRUE) &
        !grepl("/", common_name) &
        !grepl("domestic", common_name, ignore.case = TRUE) &
        !common_name %in% c(
          "House Sparrow",
          "Rock Pigeon",
          "Muscovy Duck",
          iconic_rarity_excludes
        )
    ]
    
    dt <- filter_by_month_day_range(
      dt,
      "observation_date",
      date_start,
      date_end
    )
    
    if (nrow(dt) == 0) {
      return(data.table())
    }
    
    site_species <- dt[
      ,
      .(
        site_species_checklists =
          sum(n_complete_checklists_with_species, na.rm = TRUE)
      ),
      by = .(locality_id, common_name)
    ]
    
    site_species <- merge(
      site_species,
      site_denoms,
      by = "locality_id",
      all.x = TRUE,
      sort = FALSE
    )
    
    site_species[
      ,
      site_detection_rate :=
        site_species_checklists / pmax(site_complete_checklists, 1)
    ]
    
    nearby_complete_total <- sum(site_denoms$site_complete_checklists, na.rm = TRUE)
    
    nearby_baseline <- site_species[
      ,
      .(
        nearby_complete_checklists = nearby_complete_total,
        nearby_species_checklists =
          sum(site_species_checklists, na.rm = TRUE)
      ),
      by = common_name
    ]
    
    nearby_baseline[
      ,
      nearby_vbwt_detection_rate :=
        nearby_species_checklists / pmax(nearby_complete_checklists, 1)
    ]
    
    iconic_dt <- merge(
      site_species,
      nearby_baseline[
        ,
        .(
          common_name,
          nearby_complete_checklists,
          nearby_species_checklists,
          nearby_vbwt_detection_rate
        )
      ],
      by = "common_name",
      all.x = TRUE,
      sort = FALSE
    )
    
    iconic_dt[
      ,
      iconic_score :=
        site_detection_rate / nearby_vbwt_detection_rate
    ]
    
    local_common_threshold <- 0.25
    strong_exception_threshold <- 4
    
    iconic_dt <- iconic_dt[
      !is.na(iconic_score) &
        is.finite(iconic_score) &
        iconic_score >= 2 &
        site_detection_rate >= 0.05 &
        site_complete_checklists >= 10 &
        site_species_checklists >= 3 &
        nearby_vbwt_detection_rate > 0 &
        (
          nearby_vbwt_detection_rate < local_common_threshold |
            iconic_score >= strong_exception_threshold
        )
    ]
    
    if (nrow(iconic_dt) == 0) {
      return(data.table())
    }
    
    nearby_sites <- unique(
      nearby[
        ,
        .(
          locality_id,
          site_name,
          distance_miles,
          vbwt_url
        )
      ],
      by = "locality_id"
    )
    
    iconic_dt <- merge(
      iconic_dt,
      nearby_sites,
      by = "locality_id",
      all.x = TRUE,
      sort = FALSE
    )
    
    iconic_dt[
      is.na(site_name) | site_name == "",
      site_name := locality_id
    ]
    
    iconic_dt <- iconic_dt[
      order(site_name, -iconic_score, -site_detection_rate, common_name)
    ]
    
    iconic_dt[
      ,
      head(.SD, 5),
      by = locality_id
    ]  })
  
  # ======================
  # MAP -- SERVER
  # ======================
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(
        "Stadia.AlidadeSmooth",
        options = providerTileOptions(opacity = 0.85)
      ) |>
      setView(lng = -79.5, lat = 37.8, zoom = 5.7)
  })
  
  # ======================
  # MAP -- SERVER MARKERS
  # ======================
  
  observe({
    
    proxy <- leafletProxy("map") |>
      clearGroup("VBWT Sites") |>
      clearGroup("Recent eBird Sightings") |>
      removeControl("site_rating_legend") |>
      removeControl("recent_ebird_legend")
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$species) || input$species == "" ||
      is.null(input$date_range)
    ) {
      return()
    }
    
    dt <- filtered_sites()
    
    if (nrow(dt) == 0) {
      return()
    }
    
    dt[, recommendation_score :=
         (app_complete_checklists_with_species + 5) /
         (app_complete_checklists + 50)
    ]
    
    dt[
      ,
      Recommendation := fifelse(
        recommendation_score < 0.05,
        "Possible",
        fifelse(
          recommendation_score < 0.20,
          "Good",
          fifelse(
            recommendation_score < 0.50,
            "Better",
            "Best"
          )
        )
      )
    ]
    
    dt[
      ,
      recommendation_rank := fifelse(
        Recommendation == "Best",
        4L,
        fifelse(
          Recommendation == "Better",
          3L,
          fifelse(
            Recommendation == "Good",
            2L,
            1L
          )
        )
      )
    ]
    
    dt <- dt[
      order(
        -recommendation_rank,
        -recommendation_score,
        -app_detection_rate,
        -app_complete_checklists_with_species,
        -app_complete_checklists,
        distance_miles
      )
    ]
    
    if (any(dt$Recommendation != "Low")) {
      dt <- dt[
        Recommendation != "Low"
      ]
    } else {
      dt <- dt[1]
    }
    
    dt[
      ,
      rating_color := fifelse(
        Recommendation == "Best",
        "#2A5235",
        fifelse(
          Recommendation == "Better",
          "#4F7A45",
          fifelse(
            Recommendation == "Good",
            "#8FAF5A",
            "#F1E8B8"
          )
        )
      )
    ]
    
    dt[
      ,
      `:=`(
        recent_reported_label = NA_character_,
        recent_obsDt = NA_character_,
        recent_checklist_url = NA_character_,
        recent_count = NA_real_
      )
    ]
    
    if (isTRUE(input$show_recent_sightings)) {
      recent <- recent_sightings()
      recent_dt <- recent$data
      
      if (nrow(recent_dt) > 0) {
        recent_dt <- copy(recent_dt)
        recent_dt[, locId := as.character(locId)]
        
        recent_dt[, obs_datetime := as.POSIXct(
          obsDt,
          format = "%Y-%m-%d %H:%M",
          tz = Sys.timezone()
        )]
        
        recent_dt[
          is.na(obs_datetime),
          obs_datetime := as.POSIXct(
            obsDt,
            format = "%Y-%m-%d",
            tz = Sys.timezone()
          )
        ]
        
        recent_dt[
          ,
          sighting_age_hours := as.numeric(
            difftime(
              Sys.time(),
              obs_datetime,
              units = "hours"
            )
          )
        ]
        
        recent_dt[
          ,
          reported_label := fifelse(
            !is.na(sighting_age_hours) & sighting_age_hours <= 24,
            "within the last 24 hours",
            fifelse(
              !is.na(sighting_age_hours) & sighting_age_hours <= 72,
              "within the last 3 days",
              fifelse(
                !is.na(sighting_age_hours) & sighting_age_hours <= 168,
                "within the last 7 days",
                "within the last 14 days"
              )
            )
          )
        ]
        
        recent_dt <- recent_dt[
          order(
            sighting_age_hours,
            obsDt
          )
        ]
        
        recent_summary <- recent_dt[
          ,
          .(
            recent_reported_label = reported_label[1],
            recent_obsDt = obsDt[1],
            recent_checklist_url = checklist_url[1],
            recent_count = suppressWarnings(as.numeric(howMany[1]))
          ),
          by = .(locality_id = locId)
        ]
        
        dt[, locality_id := as.character(locality_id)]
        dt <- merge(
          dt[
            ,
            !c(
              "recent_reported_label",
              "recent_obsDt",
              "recent_checklist_url",
              "recent_count"
            )
          ],
          recent_summary,
          by = "locality_id",
          all.x = TRUE,
          sort = FALSE
        )
      }
    }
    
    dt[, has_recent_sighting := !is.na(recent_reported_label)]
    
    dt[
      ,
      recent_obsDt_display := format(
        as.POSIXct(
          recent_obsDt,
          format = "%Y-%m-%d %H:%M",
          tz = Sys.timezone()
        ),
        "%m/%d/%Y at %I:%M %p"
      )
    ]
    
    dt[
      is.na(recent_obsDt_display),
      recent_obsDt_display := format(
        as.Date(substr(recent_obsDt, 1, 10)),
        "%m/%d/%Y"
      )
    ]
    
    dt[
      ,
      recent_obsDt_display := gsub(
        "^0",
        "",
        recent_obsDt_display
      )
    ]
    
    dt[
      ,
      recent_obsDt_display := gsub(
        "/0",
        "/",
        recent_obsDt_display
      )
    ]
    
    dt[
      ,
      recent_obsDt_display := gsub(
        " at 0([0-9]):",
        " at \\1:",
        recent_obsDt_display
      )
    ]
    
    dt[
      ,
      recent_popup_text := ifelse(
        has_recent_sighting,
        paste0(
          "Most recent eBird sighting: ",
          ifelse(
            !is.na(recent_count),
            paste0(recent_count, " on "),
            ""
          ),
          ifelse(
            !is.na(recent_checklist_url) & recent_checklist_url != "",
            paste0(
              "<a href='",
              recent_checklist_url,
              "' target='_blank'>",
              recent_obsDt_display,
              "</a>"
            ),
            recent_obsDt_display
          ),
          "<br>"
        ),
        ""
      )
    ]
    
    dt[, recent_radius := ifelse(has_recent_sighting, 9, 6)]
    dt[, recent_outline_color := ifelse(has_recent_sighting, "#F36C21", "#333333")]
    dt[, recent_outline_weight := ifelse(has_recent_sighting, 4, 1)]
    dt[, recent_marker_opacity := ifelse(has_recent_sighting, 1.0, 0.9)]
    
    dt[
      ,
      popup_site_name := ifelse(
        !is.na(vbwt_url) & vbwt_url != "",
        paste0(
          "<a href='",
          vbwt_url,
          "' target='_blank'><b>",
          display_name,
          "</b></a>"
        ),
        paste0("<b>", display_name, "</b>")
      )
    ]
    
    dt[
      ,
      popup_text := paste0(
        popup_site_name, "<br>",
        "Rating: ", Recommendation, "<br>",
        "Distance: ", round(distance_miles, 1), " miles<br>",
        recent_popup_text
      )
    ]
    
    non_recent_dt <- dt[has_recent_sighting == FALSE]
    recent_marker_dt <- dt[has_recent_sighting == TRUE]
    
    proxy <- proxy
    
    if (nrow(non_recent_dt) > 0) {
      proxy <- proxy |>
        addCircleMarkers(
          data = non_recent_dt,
          lng = ~site_longitude,
          lat = ~site_latitude,
          group = "VBWT Sites",
          radius = ~recent_radius,
          fillColor = ~rating_color,
          color = ~recent_outline_color,
          stroke = TRUE,
          weight = ~recent_outline_weight,
          opacity = ~recent_marker_opacity,
          fillOpacity = 0.9,
          popup = ~popup_text
        )
    }
    
    if (nrow(recent_marker_dt) > 0) {
      proxy <- proxy |>
        addCircleMarkers(
          data = recent_marker_dt,
          lng = ~site_longitude,
          lat = ~site_latitude,
          group = "VBWT Sites",
          radius = 11,
          fillColor = "#F36C21",
          color = "#F36C21",
          stroke = TRUE,
          weight = 6,
          opacity = 0.8,
          fillOpacity = 0,
          popup = ~popup_text
        ) |>
        addCircleMarkers(
          data = recent_marker_dt,
          lng = ~site_longitude,
          lat = ~site_latitude,
          group = "VBWT Sites",
          radius = ~recent_radius,
          fillColor = ~rating_color,
          color = "#333333",
          stroke = TRUE,
          weight = 1,
          opacity = ~recent_marker_opacity,
          fillOpacity = 0.9,
          popup = ~popup_text
        )
    }
    
    proxy <- proxy |>
      addLegend(
        position = "bottomleft",
        colors = c("#2A5235", "#4F7A45", "#8FAF5A", "#F1E8B8"),
        labels = c(
          "Best",
          "Better",
          "Good",
          "Low"
        ),
        title = "Site rating",
        opacity = 0.9,
        layerId = "site_rating_legend"
      )
    if (any(dt$has_recent_sighting)) {
      proxy |>
        addLegend(
          position = "bottomright",
          colors = c("#F36C21"),
          labels = c("Recent eBird Sightings"),
          opacity = 0.9,
          layerId = "recent_ebird_legend",
          className = "info legend recent-ebird-ring-legend"
        )
    }
  })
  
  # ======================
  # MAP -- RECENT EBIRD SIGHTINGS
  # Recent eBird sightings are handled by changing existing site dots in this option.
  # ======================
  
  # ======================
  # MAP -- SERVER ZOOM
  # ======================
  
  observeEvent(
    list(input$zip, input$radius),
    {
      if (is.null(input$zip) || input$zip == "") {
        leafletProxy("map") |>
          setView(lng = -79.5, lat = 37.8, zoom = 5.7)
        return()
      }
      
      radius_sites <- zip_site_distances[
        zip == input$zip &
          distance_miles <= input$radius
      ]
      
      if (nrow(radius_sites) == 0) {
        
        zip_point <- zip_site_distances[zip == input$zip][1]
        
        if (
          nrow(zip_point) > 0 &&
          !is.na(zip_point$latitude) &&
          !is.na(zip_point$longitude)
        ) {
          leafletProxy("map") |>
            setView(
              lng = zip_point$longitude,
              lat = zip_point$latitude,
              zoom = 10
            )
        }
        
        return()
      }
      
      leafletProxy("map") |>
        fitBounds(
          lng1 = min(radius_sites$site_longitude, na.rm = TRUE),
          lat1 = min(radius_sites$site_latitude, na.rm = TRUE),
          lng2 = max(radius_sites$site_longitude, na.rm = TRUE),
          lat2 = max(radius_sites$site_latitude, na.rm = TRUE)
        )
    },
    ignoreInit = TRUE
  )
  
  # ======================
  # BAR CHART -- SERVER
  # ======================
  
  output$seasonal_bar_chart <- renderPlot({
    
    req(input$zip, input$species)
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) {
      plot.new()
      text(0.5, 0.5, "No results nearby", cex = 1.2)
      return()
    }
    
    dt_all <- site_species_month_yearly[
      locality_id %in% nearby$locality_id
    ]
    
    bar_start_year <- min(year_choices)
    bar_end_year <- max(year_choices)
    
    dt_all <- dt_all[
      year >= as.integer(bar_start_year) &
        year <= as.integer(bar_end_year)
    ]
    
    denom_dt <- unique(
      dt_all[, .(locality_id, year, month, n_complete_checklists)]
    )[
      ,
      .(n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE)),
      by = month
    ]
    
    species_dt <- dt_all[
      common_name == input$species,
      .(
        n_complete_checklists_with_species =
          sum(n_complete_checklists_with_species, na.rm = TRUE)
      ),
      by = month
    ]
    
    chart_dt <- data.table(month = 1:12)
    
    chart_dt <- merge(chart_dt, denom_dt, by = "month", all.x = TRUE)
    chart_dt <- merge(chart_dt, species_dt, by = "month", all.x = TRUE)
    
    chart_dt[is.na(n_complete_checklists), n_complete_checklists := 0]
    chart_dt[is.na(n_complete_checklists_with_species), n_complete_checklists_with_species := 0]
    
    chart_dt[
      ,
      detection_rate :=
        ifelse(
          n_complete_checklists > 0,
          n_complete_checklists_with_species / n_complete_checklists,
          0
        )
    ]
    
    if (
      all(is.na(chart_dt$detection_rate)) ||
      max(chart_dt$detection_rate, na.rm = TRUE) <= 0
    ) {
      plot.new()
      text(0.5, 0.5, "No results nearby", cex = 1.2)
      return()
    }
    
    chart_dt[, is_peak_month := detection_rate == max(detection_rate, na.rm = TRUE)]
    
    ggplot(chart_dt, aes(x = factor(month, levels = 1:12), y = detection_rate, fill = is_peak_month)) +
      geom_col(width = 0.72) +
      scale_fill_manual(
        values = c("FALSE" = "#2A5235", "TRUE" = "#2A5235"),
        guide = "none"
      ) +
      scale_x_discrete(labels = month.abb) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        title = paste0("When to Look for ", pluralize_species_for_title(input$species)),
        subtitle = paste0(
          "at VBWT sites within ",
          input$radius,
          " miles of ZIP ",
          input$zip,
          ", ",
          bar_start_year,
          "–",
          bar_end_year
        ),
        x = NULL,
        y = "Chance of seeing this species"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = "#2A5235", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "#6A6157", hjust = 0.5, margin = margin(b = 10)),
        axis.title.y = element_text(size = 12, color = "#2A5235", face = "bold", margin = margin(r = 8)),
        axis.text.x = element_text(size = 11, color = "#6A6157"),
        axis.text.y = element_text(size = 10, color = "#6A6157"),
        panel.grid.major.y = element_line(color = "#DFC9A2", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 16, 10, 10)
      )
  })
  
  # ======================
  # RECENT EBIRD SIGHTINGS BOX -- SERVER
  # ======================
  
  output$recent_sightings_box <- renderUI({
    
    recent <- recent_sightings()
    
    if (!is.null(recent$message)) {
      return(div(
        style = "
      background: #ffffff;
      border: 1px solid #dddddd;
      border-left: 5px solid #F36C21;
      border-radius: 12px;
      padding: 12px 14px;
      margin: 12px auto 0 auto;
      max-width: 900px;
      box-shadow: 0 1px 5px rgba(0,0,0,0.07);
      ",
        h4(
          style = "margin-top: 0; margin-bottom: 8px;",
          "Recent eBird Sightings"
        ),
        tags$p(
          style = "margin-bottom: 0; color: #666666;",
          recent$message
        )
      ))
    }
    
    dt <- recent$data
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    dt <- head(dt, 8)
    
    sighting_rows <- lapply(seq_len(nrow(dt)), function(i) {
      
      count_text <- if (
        !is.na(dt$howMany[i]) &&
        dt$howMany[i] != ""
      ) {
        paste0(" · Count: ", dt$howMany[i])
      } else {
        ""
      }
      
      location_name <- if (
        !is.na(dt$checklist_url[i]) &&
        dt$checklist_url[i] != ""
      ) {
        tags$a(
          href = dt$checklist_url[i],
          target = "_blank",
          dt$locName[i]
        )
      } else {
        dt$locName[i]
      }
      
      tags$li(
        style = "margin-bottom: 8px;",
        tags$div(
          style = "font-weight: 700;",
          location_name
        ),
        tags$div(
          style = "font-size: 13px; color: #555555;",
          paste0(dt$obsDt[i], count_text)
        )
      )
    })
    
    radius_note <- if (input$radius > 31) {
      tags$p(
        style = "font-size: 12px; color: #666666; margin-bottom: 8px;",
        "Note: eBird limits recent sightings searches to about 31 miles."
      )
    } else {
      NULL
    }
    
    div(
      style = "
      background: #ffffff;
      border: 1px solid #dddddd;
      border-left: 5px solid #F36C21;
      border-radius: 12px;
      padding: 12px 14px;
      margin: 12px auto 0 auto;
      max-width: 900px;
      box-shadow: 0 1px 5px rgba(0,0,0,0.07);
      ",
      h4(
        style = "margin-top: 0; margin-bottom: 8px;",
        "Recent eBird Sightings"
      ),
      radius_note,
      tags$p(
        style = "font-size: 13px; color: #555555; margin-bottom: 8px;",
        paste0(
          "Reports from the last ",
          input$recent_days,
          " days."
        )
      ),
      tags$ul(
        style = "padding-left: 18px; margin-bottom: 0;",
        sighting_rows
      )
    )
  })
  
  # ======================
  # PROFILE TABLE -- SERVER
  # ======================
  
  output$profile_box <- renderUI({
    req(input$species)
    
    p <- profile[common_name == input$species]
    
    if (nrow(p) == 0) {
      return(NULL)
    }
    
    if (
      is.null(input$zip) ||
      input$zip == ""
    ) {
      peak_season <- "Enter ZIP code"
      peak_month <- "Enter ZIP code"
      peak_week <- "Enter ZIP code"
    } else {
      peak_season <- "No results nearby"
      peak_month <- "No results nearby"
      peak_week <- "No results nearby"
    }
    
    if (
      !is.null(input$zip) &&
      input$zip != "" &&
      !is.null(input$species) &&
      input$species != ""
    ) {
      
      bar_start_year <- min(year_choices)
      bar_end_year <- max(year_choices)
      
      nearby <- zip_site_distances[
        zip == input$zip &
          distance_miles <= input$radius
      ]
      
      if (nrow(nearby) > 0) {
        
        dt_all <- site_species_month_yearly[
          locality_id %in% nearby$locality_id
        ]
        
        dt_all <- dt_all[
          year >= as.integer(bar_start_year) &
            year <= as.integer(bar_end_year)
        ]
        
        denom_month <- unique(
          dt_all[, .(locality_id, year, month, n_complete_checklists)]
        )[
          ,
          .(n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE)),
          by = month
        ]
        
        species_month <- dt_all[
          common_name == input$species,
          .(
            n_complete_checklists_with_species =
              sum(n_complete_checklists_with_species, na.rm = TRUE)
          ),
          by = month
        ]
        
        month_dt <- data.table(month = 1:12)
        
        month_dt <- merge(month_dt, denom_month, by = "month", all.x = TRUE)
        month_dt <- merge(month_dt, species_month, by = "month", all.x = TRUE)
        
        month_dt[is.na(n_complete_checklists), n_complete_checklists := 0]
        month_dt[is.na(n_complete_checklists_with_species), n_complete_checklists_with_species := 0]
        
        month_dt[
          ,
          detection_rate :=
            ifelse(
              n_complete_checklists > 0,
              n_complete_checklists_with_species / n_complete_checklists,
              0
            )
        ]
        
        if (nrow(month_dt) > 0 && max(month_dt$detection_rate, na.rm = TRUE) > 0) {
          peak_month_row <- month_dt[which.max(detection_rate)]
          peak_month_num <- peak_month_row$month
          
          peak_month <- month.name[peak_month_num]
          peak_season <- format_month_as_season(peak_month_num)
        }
        
        denom_week <- site_date_denoms[
          locality_id %in% nearby$locality_id
        ]
        
        denom_week <- denom_week[
          as.integer(format(as.Date(observation_date), "%Y")) >= as.integer(bar_start_year) &
            as.integer(format(as.Date(observation_date), "%Y")) <= as.integer(bar_end_year)
        ]
        
        denom_week[
          ,
          `:=`(
            peak_month_num = as.integer(format(as.Date(observation_date), "%m")),
            peak_week_num = ceiling(as.integer(format(as.Date(observation_date), "%d")) / 7)
          )
        ]
        
        denom_week <- denom_week[
          ,
          .(
            n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE)
          ),
          by = .(peak_month_num, peak_week_num)
        ]
        
        species_week <- site_species_date[
          common_name == input$species &
            locality_id %in% nearby$locality_id
        ]
        
        species_week <- species_week[
          as.integer(format(as.Date(observation_date), "%Y")) >= as.integer(bar_start_year) &
            as.integer(format(as.Date(observation_date), "%Y")) <= as.integer(bar_end_year)
        ]
        
        species_week[
          ,
          `:=`(
            peak_month_num = as.integer(format(as.Date(observation_date), "%m")),
            peak_week_num = ceiling(as.integer(format(as.Date(observation_date), "%d")) / 7)
          )
        ]
        
        species_week <- species_week[
          ,
          .(
            n_complete_checklists_with_species =
              sum(n_complete_checklists_with_species, na.rm = TRUE)
          ),
          by = .(peak_month_num, peak_week_num)
        ]
        
        week_dt <- merge(
          denom_week,
          species_week,
          by = c("peak_month_num", "peak_week_num"),
          all.x = TRUE,
          sort = FALSE
        )
        
        week_dt[is.na(n_complete_checklists_with_species), n_complete_checklists_with_species := 0]
        
        week_dt[
          ,
          detection_rate :=
            n_complete_checklists_with_species / pmax(n_complete_checklists, 1)
        ]
        
        if (nrow(week_dt) > 0 && max(week_dt$detection_rate, na.rm = TRUE) > 0) {
          peak_week_row <- week_dt[
            order(
              -detection_rate,
              -n_complete_checklists_with_species,
              -n_complete_checklists
            )
          ][1]
          
          peak_week_day <- ((peak_week_row$peak_week_num - 1) * 7) + 1
          
          peak_week <- format_ordinal_week(
            peak_week_row$peak_month_num,
            peak_week_day
          )
        }
      }
    }
    
    div(
      style = "background: #DFC9A2; border: 2px solid #2A5235; border-radius: 14px; padding: 22px 24px; margin-top: 8px; margin-bottom: 12px;",
      tags$div(style = "font-size: clamp(30px, 4vw, 42px); line-height: 1.08; color: #2A5235; font-weight: 800; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-bottom: 4px;", p$common_name),
      tags$div(style = "font-size: 23px; color: #6A6157; margin-bottom: 18px;", tags$em(p$scientific_name)),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 12px;",
        div(
          style = "background: #A19857; border-radius: 12px; padding: 13px 14px; min-width: 0;",
          tags$div(style = "font-size: 12px; color: #79441C; font-weight: 700; margin-bottom: 4px;", "Peak Season:"),
          tags$div(style = "font-size: 14px; color: #2A5235; font-weight: 600;", peak_season)
        ),
        div(
          style = "background: #A19857; border-radius: 12px; padding: 13px 14px; min-width: 0;",
          tags$div(style = "font-size: 12px; color: #79441C; font-weight: 700; margin-bottom: 4px;", "Peak Month:"),
          tags$div(style = "font-size: 14px; color: #2A5235; font-weight: 600;", peak_month)
        ),
        div(
          style = "background: #A19857; border-radius: 12px; padding: 13px 14px; min-width: 0;",
          tags$div(style = "font-size: 12px; color: #79441C; font-weight: 700; margin-bottom: 4px;", "Peak Week:"),
          tags$div(style = "font-size: 14px; color: #2A5235; font-weight: 600;", peak_week)
        )
      ),
      div(
        style = "margin-top: 24px; border-radius: 12px; overflow: hidden;",
        plotOutput("seasonal_bar_chart", height = 225)
      ),
    )
  })
  
  # ======================
  # SEASONAL CHART NOTE -- SERVER
  # ======================
  
  output$seasonal_chart_note <- renderUI({
    
    req(input$zip, input$species)
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) return(NULL)
    
    dt_all <- site_species_month_yearly[
      locality_id %in% nearby$locality_id
    ]
    
    bar_start_year <- min(year_choices)
    bar_end_year <- max(year_choices)
    
    dt_all <- dt_all[
      year >= as.integer(bar_start_year) &
        year <= as.integer(bar_end_year)
    ]
    
    denom_dt <- unique(
      dt_all[, .(locality_id, year, month, n_complete_checklists)]
    )[
      ,
      .(n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE)),
      by = month
    ]
    
    species_dt <- dt_all[
      common_name == input$species,
      .(
        n_complete_checklists_with_species =
          sum(n_complete_checklists_with_species, na.rm = TRUE)
      ),
      by = month
    ]
    
    chart_dt <- data.table(month = 1:12)
    
    chart_dt <- merge(chart_dt, denom_dt, by = "month", all.x = TRUE)
    chart_dt <- merge(chart_dt, species_dt, by = "month", all.x = TRUE)
    
    chart_dt[is.na(n_complete_checklists), n_complete_checklists := 0]
    chart_dt[is.na(n_complete_checklists_with_species), n_complete_checklists_with_species := 0]
    
    chart_dt[
      ,
      detection_rate :=
        ifelse(
          n_complete_checklists > 0,
          n_complete_checklists_with_species / n_complete_checklists,
          0
        )
    ]
    
    if (nrow(chart_dt) == 0 || max(chart_dt$detection_rate, na.rm = TRUE) <= 0) {
      return(NULL)
    }
    
    top_months <- chart_dt[
      order(-detection_rate)
    ][1:min(.N, 3)]
    
    top_month_labels <- paste(
      month.name[top_months$month],
      collapse = ", "
    )
    
    peak <- top_months[1]
    
    tags$p(
      style = "
    text-align: center;
    font-size: 13px;
    color: #555555;
    margin: -2px 0 2px 0;
    ",
      paste0(
        "Most likely months: ",
        top_month_labels,
        ". Peak month: ",
        month.name[peak$month],
        " (",
        percent(peak$detection_rate, accuracy = 0.1),
        ")."
      )
    )
  })
  
  # ======================
  # BEST SITE BOX -- SERVER
  # ======================
  
  output$best_site_box <- renderUI({
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$species) || input$species == "" ||
      is.null(input$date_range)
    ) {
      return(NULL)
    }
    
    dt <- filtered_sites()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    dt[, recommendation_score :=
         (app_complete_checklists_with_species + 5) /
         (app_complete_checklists + 50)
    ]
    
    best <- dt[
      order(
        -recommendation_score,
        -app_detection_rate,
        -app_complete_checklists_with_species,
        -app_complete_checklists,
        distance_miles
      )
    ][1]
    
    limited_records <- best$app_complete_checklists < 50 ||
      best$app_complete_checklists_with_species < 10
    
    limited_records <- FALSE
    
    recommendation_strength <- if (best$recommendation_score < 0.05) {
      "Low Likelihood VBWT Site"
    } else if (best$recommendation_score < 0.20) {
      "Good VBWT Site"
    } else if (best$recommendation_score < 0.50) {
      "Better VBWT Site"
    } else {
      "Best VBWT Site"
    }
    
    data_confidence <- if (
      best$app_complete_checklists < 50 ||
      best$app_complete_checklists_with_species < 10
    ) {
      "Limited"
    } else if (best$app_complete_checklists_with_species < 20) {
      "Moderate"
    } else {
      "Strong"
    }
    
    site_link <- if (
      !is.na(best$vbwt_url) &&
      best$vbwt_url != ""
    ) {
      tags$a(
        href = best$vbwt_url,
        target = "_blank",
        best$display_name
      )
    } else {
      best$display_name
    }
    
    raw_start_date <- as.Date(input$date_range[1])
    raw_end_date <- as.Date(input$date_range[2])
    
    start_date <- min(raw_start_date, raw_end_date, na.rm = TRUE)
    end_date <- max(raw_start_date, raw_end_date, na.rm = TRUE)
    
    start_label <- paste(
      month.name[as.integer(format(start_date, "%m"))],
      as.integer(format(start_date, "%d"))
    )
    
    end_label <- paste(
      month.name[as.integer(format(end_date, "%m"))],
      as.integer(format(end_date, "%d"))
    )
    
    selected_month <- if (
      !is.null(input$month) &&
      input$month != ""
    ) {
      as.integer(input$month)
    } else {
      NA_integer_
    }
    
    full_month_selected <- FALSE
    
    if (!is.na(selected_month)) {
      month_start <- as.Date(paste0(
        "2000-",
        sprintf("%02d", selected_month),
        "-01"
      ))
      
      month_end <- if (selected_month == 12) {
        as.Date("2000-12-31")
      } else {
        as.Date(paste0(
          "2000-",
          sprintf("%02d", selected_month + 1),
          "-01"
        )) - 1
      }
      
      full_month_selected <- identical(start_date, month_start) &&
        identical(end_date, month_end)
    }
    
    likelihood_label <- if (full_month_selected) {
      paste0(
        "Likelihood in ",
        month.name[selected_month],
        ": ",
        percent(best$app_detection_rate, accuracy = 0.1)
      )
    } else {
      paste0(
        "Likelihood from ",
        start_label,
        " to ",
        end_label,
        ": ",
        percent(best$app_detection_rate, accuracy = 0.1)
      )
    }
    
    limited_record_note <- if (limited_records) {
      tags$p(
        style = "margin-bottom: 4px; color: #8a5a00;",
        paste0(
          "Data note: limited records for this date range. This ranking is based on ",
          best$app_complete_checklists_with_species,
          " checklists with this species out of ",
          best$app_complete_checklists,
          " complete checklists."
        )
      )
    } else {
      NULL
    }
    
    div(
      style = "
                        background: #f7f7f7;
                        border: 1px solid #dddddd;
                        border-radius: 8px;
                        padding: 14px 16px;
                        margin-bottom: 10px;
                        ",
      
      h4(
        style = "margin-top: 0; margin-bottom: 8px;",
        "Best Bet"
      ),
      
      tags$p(
        style = "font-size: 18px; margin-bottom: 6px; font-weight: 700;",
        site_link
      ),
      
      tags$p(
        style = "margin-bottom: 0; color: #555555;",
        paste0(
          "Best VBWT site within ",
          input$radius,
          " miles of ",
          input$zip,
          " for this species between ",
          start_label,
          " and ",
          end_label,
          "."
        )
      )
    )
  })
  
  # ======================
  # ICONIC SPECIES -- SERVER TABLE
  # ======================
  
  output$iconic_table_title <- renderUI({
    
    if (
      is.null(input$zip) ||
      input$zip == "" ||
      is.null(input$date_range)
    ) {
      h4("Notable Species Nearby")
    } else {
      
      start_date <- as.Date(input$date_range[1])
      end_date <- as.Date(input$date_range[2])
      
      start_label <- paste(
        month.name[as.integer(format(start_date, "%m"))],
        as.integer(format(start_date, "%d"))
      )
      
      end_label <- paste(
        month.name[as.integer(format(end_date, "%m"))],
        as.integer(format(end_date, "%d"))
      )
      
      h4(paste0(
        "Notable Species within ",
        input$radius,
        " miles of ZIP ",
        input$zip,
        " between ",
        start_label,
        " and ",
        end_label
      ))
    }
  })
  
  output$iconic_table <- renderDT({
    
    req(input$zip, input$date_range)
    
    dt <- iconic_species()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    dt <- dt[
      order(
        site_name,
        -iconic_score,
        common_name
      )
    ]
    
    site_summary <- dt[
      ,
      .(
        Distance = min(distance_miles, na.rm = TRUE),
        `Iconic Species` = paste(
          common_name,
          collapse = ", "
        ),
        `Top Iconic Score` = max(iconic_score, na.rm = TRUE),
        `Number of Iconic Species` = .N,
        vbwt_url = vbwt_url[which.max(iconic_score)]
      ),
      by = .(site_name)
    ]
    
    site_summary <- site_summary[
      order(
        Distance,
        -`Top Iconic Score`,
        site_name
      )
    ]
    
    out <- site_summary[
      ,
      .(
        Site = ifelse(
          !is.na(vbwt_url) & vbwt_url != "",
          paste0("<a href='", vbwt_url, "' target='_blank'>", site_name, "</a>"),
          site_name
        ),
        `Distance (mi)` = round(Distance, 1),
        `Notable Species` = `Iconic Species`
      )
    ]
    
    datatable(
      out,
      escape = FALSE,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 10,
        ordering = TRUE,
        order = list(list(1, "asc")),
        language = list(
          search = "",
          searchPlaceholder = "Search site or species"
        ),
        columnDefs = list(
          list(width = "35%", targets = 0),
          list(width = "12%", targets = 1),
          list(width = "53%", targets = 2)
        )
      )
    )
  })
  
  # ======================
  # MAP -- SERVER MESSAGE
  # ======================
  
  output$map_message <- renderUI({
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$species) || input$species == "" ||
      is.null(input$date_range)
    ) {
      div(
        id = "map-message",
        tags$div(style = "font-weight: 600; margin-bottom: 8px;", "To start exploring:"),
        tags$div("1. Enter a ZIP code"),
        tags$div("2. Select a species"),
        tags$div("3. Adjust the date range if needed"),
        tags$div("4. Optionally choose a month")
      )
    } else {
      
      dt <- filtered_sites()
      
      if (nrow(dt) == 0) {
        div(
          id = "map-message",
          tags$div(
            style = "font-weight: 700; margin-bottom: 6px;",
            "No results nearby"
          ),
          tags$div(
            "Try a different date range, month, or wider search radius."
          )
        )
      } else {
        NULL
      }
    }
  })
  
  # ======================
  # RANK TABLE -- SERVER
  # ======================
  
  output$rank_table_title <- renderUI({
    
    if (
      is.null(input$species) ||
      input$species == ""
    ) {
      h4("Top Sites")
    } else {
      h4(paste0("Top Sites for ", pluralize_species_for_title(input$species)))
    }
  })
  
  output$rank_table <- renderDT({
    req(input$zip, input$species)
    
    dt <- filtered_sites()
    
    if (nrow(dt) == 0) {
      no_results <- data.table(
        Message = "No recommended sites found. Try a different date range, wider search radius, or another species."
      )
      
      return(
        datatable(
          no_results,
          escape = FALSE,
          rownames = FALSE,
          colnames = "",
          options = list(
            dom = "t",
            ordering = FALSE
          )
        )
      )
    }
    
    dt[, recommendation_score :=
         (app_complete_checklists_with_species + 5) /
         (app_complete_checklists + 50)
    ]
    
    dt[
      ,
      Recommendation := fifelse(
        app_complete_checklists < 50 |
          app_complete_checklists_with_species < 10,
        "Limited Data",
        fifelse(
          recommendation_score < 0.05,
          "Low Likelihood",
          fifelse(
            recommendation_score < 0.20,
            "Good",
            fifelse(
              recommendation_score < 0.50,
              "Better",
              "Best"
            )
          )
        )
      )
    ]
    
    dt[
      ,
      recommendation_rank := fifelse(
        Recommendation == "Best",
        5L,
        fifelse(
          Recommendation == "Better",
          4L,
          fifelse(
            Recommendation == "Good",
            3L,
            fifelse(
              Recommendation == "Low Likelihood",
              2L,
              1L
            )
          )
        )
      )
    ]
    
    dt <- dt[
      order(
        -recommendation_rank,
        -recommendation_score,
        -app_detection_rate,
        -app_complete_checklists_with_species,
        -app_complete_checklists,
        distance_miles
      )
    ]
    
    if (any(dt$Recommendation != "Low Likelihood")) {
      dt <- dt[
        Recommendation != "Low Likelihood"
      ]
    } else {
      dt <- dt[1]
    }
    
    default_order <- list()
    
    out <- dt[, .(
      Site = ifelse(
        !is.na(vbwt_url) & vbwt_url != "",
        paste0("<a href='", vbwt_url, "' target='_blank'>", display_name, "</a>"),
        display_name
      ),
      Rating = Recommendation,
      Likelihood = app_detection_rate,
      `Distance (mi)` = round(distance_miles, 1),
      Checklists = paste0(
        app_complete_checklists_with_species,
        " of ",
        app_complete_checklists
      )
    )][seq_len(min(.N, 10))]
    
    datatable(
      out,
      escape = FALSE,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        pageLength = 10,
        ordering = TRUE,
        order = default_order,
        columnDefs = list(
          list(width = "34%", targets = 0),
          list(width = "18%", targets = 1),
          list(width = "14%", targets = 2),
          list(width = "14%", targets = 3),
          list(width = "20%", targets = 4)
        )
      )
    ) |>
      formatPercentage("Likelihood", digits = 1)
  })
  
  # ======================
  # RESET FILTERS -- SERVER
  # ======================
  
  observeEvent(input$reset, {
    
    updateSelectizeInput(session, "zip", selected = "")
    updateSelectizeInput(session, "species", selected = "")
    updateSliderInput(session, "radius", value = 25)
    updateSelectInput(session, "month", selected = as.character(current_month))
    updateDateRangeInput(
      session,
      "date_range",
      start = date_default_start,
      end = date_default_end
    )
    leafletProxy("map") |>
      clearMarkers() |>
      clearControls() |>
      setView(lng = -79.5, lat = 37.8, zoom = 5.7)
  })
}

shinyApp(ui, server)


# VBWT Explorer v1 - Phase 1 Core App

library(shiny)
library(data.table)
library(leaflet)
library(ggplot2)
library(scales)
library(DT)

# ======================
# LOAD DATA
# ======================

site_lookup <- as.data.table(readRDS("data_processed/site_lookup.rds"))
species_lookup <- as.data.table(readRDS("data_processed/species_lookup.rds"))
site_species_month <- as.data.table(readRDS("data_processed/site_species_month_metrics.rds"))
site_species_metrics <- as.data.table(readRDS("data_processed/site_species_metrics.rds"))
zip_site_distances <- as.data.table(readRDS("data_processed/zip_site_distances.rds"))
profile <- as.data.table(readRDS("data_processed/species_profile_lookup.rds"))

# ======================
# HELPER FUNCTIONS
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

# ======================
# UI INPUT PREP
# ======================

valid_species <- profile[
  !is.na(percent_pop_breeding) &
    percent_pop_breeding >= 0.01,
  unique(common_name)
]

species_choices <- c("Select a species..." = "", sort(valid_species))
month_choices <- c("Select a month..." = "", setNames(1:12, month.abb))
year_choices <- sort(unique(site_species_month$year))

# ======================
# UI
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
      "))
  ),
  
  titlePanel("VBWT Explorer v1"),
  
  div(
    class = "app-wrap",
    
    # ======================
    # SIDEBAR
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
        
        selectInput("month", "Month", choices = month_choices),
        
        sliderInput("radius", "Search Radius (miles)", 5, 150, 25, 5),
        
        sliderInput("min_checklists", "Minimum Complete Checklists", 1, 100, 10),
        
        actionButton("reset", "Reset Filters", width = "100%")
      )
    ),
    
    # ======================
    # MAIN PANEL
    # ======================
    
    div(
      class = "main-content",
      tabsetPanel(
        tabPanel(
          "Map",
          div(
            style = "position: relative;",
            leafletOutput("map", height = 500),
            uiOutput("map_message")
          )
        ),
        
        tabPanel(
          "Bar Chart",
          div(
            style = "display: flex; gap: 10px; max-width: 260px;",
            
            div(
              style = "flex: 1;",
              selectInput(
                "bar_start_year",
                "Start Year",
                choices = year_choices,
                selected = min(year_choices),
                width = "100%"
              )
            ),
            
            div(
              style = "flex: 1;",
              selectInput(
                "bar_end_year",
                "End Year",
                choices = year_choices,
                selected = max(year_choices),
                width = "100%"
              )
            )
          ),
          
          plotOutput("seasonal_bar_chart", height = 425)
        ),
        
        tabPanel(
          "Details",
          tableOutput("profile_box"),
          DTOutput("rank_table")
        )
      )
    )
  )
)

# ======================
# SERVER
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
  
  # ======================
  # FILTERED DATA
  # ======================
  
  filtered_sites <- reactive({
    
    req(input$zip, input$species, input$month)
    
    if (input$month == "") {
      return(data.table())
    }
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) {
      return(data.table())
    }
    
    selected_month <- as.integer(input$month)
    
    species_dt <- site_species_month[
      common_name == input$species &
        month == selected_month
    ]
    
    species_dt <- species_dt[, .(
      n_complete_checklists = sum(n_complete_checklists, na.rm = TRUE),
      n_checklists_with_species = sum(n_complete_checklists_with_species, na.rm = TRUE)
    ), by = .(locality_id, common_name)]
    
    species_dt[, detection_rate_complete :=
                 n_checklists_with_species / pmax(n_complete_checklists, 1)]
    
    species_dt <- species_dt[n_complete_checklists >= input$min_checklists]
    
    if (nrow(species_dt) == 0) {
      return(data.table())
    }
    
    dt <- merge(
      nearby,
      species_dt,
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
  # MAP
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
  # MAP MARKERS
  # ======================
  
  observe({
    
    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearControls()
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$species) || input$species == "" ||
      is.null(input$month) || input$month == ""
    ) {
      return()
    }
    
    dt <- filtered_sites()
    
    if (nrow(dt) == 0) {
      return()
    }
    
    pal <- colorNumeric(
      palette = c("#ffffcc", "#ffcc66", "#ff9900", "#e34a33", "#b30000"),
      domain = dt$app_detection_rate,
      na.color = "#cccccc"
    )
    
    proxy |>
      addCircleMarkers(
        data = dt,
        lng = ~site_longitude,
        lat = ~site_latitude,
        radius = 6,
        fillColor = ~pal(app_detection_rate),
        color = "#333333",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>", display_name, "</b><br>",
          "Distance: ", distance_miles, " miles<br>",
          "Likelihood: ", percent(app_detection_rate, accuracy = 0.1), "<br>",
          "Relative to VBWT average: ", round(relative_detection, 1), "x<br>",
          "Complete checklists with species: ", app_complete_checklists_with_species, "<br>",
          "Complete checklists total: ", app_complete_checklists, "<br>",
          ifelse(
            !is.na(vbwt_url) & vbwt_url != "",
            paste0("<a href='", vbwt_url, "' target='_blank'>Open VBWT site webpage</a>"),
            ""
          )
        )
      ) |>
      addLegend(
        position = "bottomleft",
        colors = c("#ffffcc", "#ffcc66", "#ff9900", "#e34a33", "#b30000"),
        labels = c(
          "Lowest",
          "Low",
          "Medium",
          "High",
          "Highest"
        ),
        title = "Detection rate",
        opacity = 0.9
      )
  })
  
  # ======================
  # MAP ZOOM
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
  # BAR CHART
  # ======================
  
  output$seasonal_bar_chart <- renderPlot({
    
    req(input$zip, input$species)
    
    nearby <- zip_site_distances[
      zip == input$zip &
        distance_miles <= input$radius
    ]
    
    if (nrow(nearby) == 0) return(NULL)
    
    dt_all <- site_species_month[
      locality_id %in% nearby$locality_id
    ]
    
    req(input$bar_start_year <= input$bar_end_year)
    
    dt_all <- dt_all[
      year >= as.integer(input$bar_start_year) &
        year <= as.integer(input$bar_end_year)
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
    
    if (all(is.na(chart_dt$detection_rate))) return(NULL)
    
    ggplot(chart_dt, aes(x = factor(month, levels = 1:12), y = detection_rate)) +
      geom_col() +
      scale_x_discrete(labels = month.abb) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste0(input$species, " Seasonal Pattern"),
        x = NULL,
        y = "Percent of Complete Checklists"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ======================
  # PROFILE TABLE
  # ======================
  
  output$profile_box <- renderTable({
    req(input$species)
    
    p <- profile[common_name == input$species]
    
    if (nrow(p) == 0) {
      return(NULL)
    }
    
    p[, .(
      Species = common_name,
      Scientific = scientific_name,
      Peak_Season = format_peak_season(strongest_va_season),
      Peak_Week = format_peak_week(max_week)
    )]
  })
  
  # ======================
  # MAP MESSAGE
  # ======================
  
  output$map_message <- renderUI({
    
    if (
      is.null(input$zip) || input$zip == "" ||
      is.null(input$species) || input$species == "" ||
      is.null(input$month) || input$month == ""
    ) {
      div(
        id = "map-message",
        tags$div(style = "font-weight: 600; margin-bottom: 8px;", "To start exploring:"),
        tags$div("1. Enter a ZIP code"),
        tags$div("2. Select a species"),
        tags$div("3. Choose a month")
      )
    } else {
      
      dt <- filtered_sites()
      
      if (nrow(dt) == 0) {
        div(
          id = "map-message",
          tags$div(
            style = "font-weight: 700; margin-bottom: 6px;",
            "No results found"
          ),
          tags$div(
            "Try a different month, wider search radius, or lower checklist minimum."
          )
        )
      } else {
        NULL
      }
    }
  })
  
  # ======================
  # RANK TABLE
  # ======================
  
  output$rank_table <- renderDT({
    req(input$zip, input$species)
    
    dt <- filtered_sites()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    dt <- dt[order(distance_miles)]
    
    default_order <- list(list(1, "asc"))
    
    out <- dt[, .(
      Site = ifelse(
        !is.na(vbwt_url) & vbwt_url != "",
        paste0("<a href='", vbwt_url, "' target='_blank'>", display_name, "</a>"),
        display_name
      ),
      Distance = round(distance_miles, 1),
      Likelihood = app_detection_rate,
      Relative = relative_detection
    )][seq_len(min(.N, 20))]
    
    datatable(
      out,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        ordering = TRUE,
        order = default_order
      )
    ) |>
      formatPercentage("Likelihood", digits = 1) |>
      formatRound("Relative", digits = 1)
  })
  
  # ======================
  # RESET FILTERS
  # ======================
  
  observeEvent(input$reset, {
    
    updateSelectizeInput(session, "zip", selected = "")
    updateSelectizeInput(session, "species", selected = "")
    updateSliderInput(session, "radius", value = 25)
    updateSelectInput(session, "month", selected = "")
    updateSelectInput(session, "bar_start_year", selected = min(year_choices))
    updateSelectInput(session, "bar_end_year", selected = max(year_choices))
    updateSliderInput(session, "min_checklists", value = 10)
    
    leafletProxy("map") |>
      clearMarkers() |>
      clearControls() |>
      setView(lng = -79.5, lat = 37.8, zoom = 5.7)
  })
}

shinyApp(ui, server)
# ArcticGRO Data Explorer
# Two sections:
#  - Discharge: daily, monthly and annual river discharge, read from the latest
#    version of the ArcticGRO Discharge Dataset on Zenodo (R/data_zenodo.R)
#  - Water chemistry: ArcticGRO water quality samples, read from the master
#    Google Sheet (R/data_wq.R)
# Helper code is in R/ and loads automatically.
#
# Open a section directly with ?section=discharge or ?section=chemistry
# (add &embed=1 when embedding in the website).
#
# Run locally:  shiny::runApp()

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)

DS_START <- get_dataset()   # load once when the app starts
WQ_START <- get_wq()

WQ_RIVER_CHOICES <- c("Ob'", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")
WQ_YEARS <- range(year(WQ_START$values$date))

# Parameter menu, grouped, e.g. "DOC (mg/L)" under "Dissolved organic carbon"
wq_param_choices <- function(params) {
  lab <- ifelse(nzchar(params$unit), sprintf("%s (%s)", params$parameter, params$unit), params$parameter)
  split(setNames(params$parameter, lab), factor(params$group, levels = unique(params$group)))
}

WQ_SYMBOL_NOTE <- paste(
  "Colours: season of sampling. Filled circle: accepted value; open circle: provisional;",
  "diamond: caution; square: depth sample; open triangle at the bottom: below detection.")

station_choices <- function(st) {
  core <- st$core_arcticgro_river == "yes"
  list(`Core ArcticGRO rivers` = setNames(st$station_id[core], st$label[core]),
       `Additional rivers`     = setNames(st$station_id[!core], st$label[!core]))
}

# Dataset citation for the loaded version, e.g.
# Arctic Great Rivers Observatory. (2026). ArcticGRO Discharge Dataset (Version v20261104)
#   [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.23957361
cite_text <- function(info) {
  year <- substr(info$published %||% "", 1, 4)
  if (!nzchar(year)) year <- format(Sys.Date(), "%Y")
  ver <- info$version %||% ""
  doi <- if (nzchar(info$doi %||% "")) info$doi else CONCEPT_DOI
  url <- paste0("https://doi.org/", doi)
  txt <- paste0("Arctic Great Rivers Observatory. (", year, "). ArcticGRO Discharge Dataset",
                if (nzchar(ver)) paste0(" (Version ", ver, ")"), " [Dataset]. Zenodo. ")
  tagList(HTML(paste0(htmltools::htmlEscape(txt),
                      sprintf("<a href='%s' target='_blank'>%s</a>", url, url))))
}

# Holmes et al. (2011) reference with DOI link
HOLMES_2011_LINK <- function() {
  doi <- "https://doi.org/10.1007/s12237-011-9386-6"
  tagList(HTML(paste0(htmltools::htmlEscape(sub(" https://doi.org/.*$", " ", HOLMES_2011)),
                      sprintf("<a href='%s' target='_blank'>%s</a>.", doi, doi))))
}

# Link to the water quality Google Sheet, and the data source text with that link
wq_sheet_link <- function(text = "ArcticGRO water quality spreadsheet (Google Sheet)")
  a(text, href = WQ_SHEET_VIEW_URL, target = "_blank", .noWS = "after")
wq_source_ui <- function(src) {
  out <- if (grepl("Google Sheet could not be reached", src))
    tagList("bundled copy (the ", wq_sheet_link("Google Sheet"), " could not be reached)")
  else if (grepl("Google Sheet", src)) wq_sheet_link()
  else src
  tags$span(out, .noWS = "after")
}

year_label <- function(y, year_type) if (year_type == "water") paste("WY", y) else as.character(y)

# ---- UI ----------------------------------------------------------------------------

ui <- page_navbar(
  title = APP_TITLE,
  id = "nav",
  window_title = APP_TITLE,
  theme = bs_theme(
    version = 5, bg = "#ffffff", fg = SITE$text, primary = SITE$blue,
    base_font    = font_collection(FONT_BODY, "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = font_collection(FONT_HEADINGS, "Helvetica Neue", "Arial", "sans-serif"),
    "border-color" = SITE$border,
    # bslib builds colour helper classes the app doesn't use; for a few mid-tones
    # no text colour reaches 4.5:1, which only produces console warnings.
    "min-contrast-ratio" = 3
  ),
  fillable = FALSE,
  header = tagList(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(rel = "stylesheet", href = GOOGLE_FONTS_URL),
      tags$link(rel = "stylesheet", href = "site.css"),
      tags$script(src = "embed.js")
    ),
    div(id = "app-loading", class = "app-loading", role = "status",
        "Loading the ArcticGRO data explorer\u2026")
  ),
  nav_panel(
    "Discharge", value = "discharge", icon = NULL,
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
            selectInput("station", "Station", choices = station_choices(DS_START$stations),
                        selected = "Lena_Kyusyur"),
            radioButtons("year_type", "Year",
                         c("Calendar year (Jan\u2013Dec)" = "calendar", "Water year (Oct\u2013Sep)" = "water")),
            selectizeInput("years", "Years to compare (up to 4)", choices = NULL, multiple = TRUE,
                           options = list(maxItems = MAX_YEARS_SHOWN, plugins = list("remove_button"))),
            sliderInput("ref", "Reference period for statistics", min = 1900, max = 2030,
                        value = DEFAULT_REF_PERIOD, step = 1, sep = ""),
            checkboxInput("log_y", "Logarithmic scale", FALSE),
            hr(),
            uiOutput("data_info")
      ),
      navset_underline(
        id = "dis_tabs",
        nav_panel(
            "Daily hydrograph",
            card(
              full_screen = TRUE,
              card_header(textOutput("hydro_title", inline = TRUE)),
              plotlyOutput("hydro", height = "520px"),
              card_footer(
                "Grey bands: range, 10th\u201390th and 25th\u201375th percentiles of daily discharge in the ",
                "reference period; dashed line: median. Dotted line: provisional data. ",
                "29 February is not shown."
              )
            ),
            accordion(open = FALSE, accordion_panel(
              "Show values as a table",
              DTOutput("hydro_table")
            ))
          ),
        nav_panel(
            "Monthly & annual",
            layout_columns(
              col_widths = c(6, 6),
              card(
                full_screen = TRUE,
                card_header(textOutput("monthly_title", inline = TRUE)),
                plotlyOutput("monthly", height = "420px"),
                card_footer("Monthly means of the selected years against the reference period. ",
                            "Open markers: month includes provisional data. Months with less than ",
                            sprintf("%d%% of days are left out.", round(100 * MIN_COMPLETENESS)))
              ),
              card(
                full_screen = TRUE,
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  "Annual statistics",
                  div(style = "width: 290px; margin-bottom: -1rem;",
                      selectInput("metric", NULL, choices = ANNUAL_METRICS, width = "100%"))
                ),
                plotlyOutput("annual", height = "420px"),
                card_footer("Only years with at least ",
                            sprintf("%d%% of days are shown. ", round(100 * MIN_COMPLETENESS)),
                            "Open markers: year includes provisional data.")
              )
            ),
            card(
              card_header("Annual summary table"),
              DTOutput("annual_table")
            )
          ),
        nav_panel(
          "Data & download",
          card(
            card_body(
              class = "pb-0",
              layout_columns(
                col_widths = c(4, 3, 3, 2),
                selectizeInput("dl_stations", "Stations", choices = station_choices(DS_START$stations),
                               multiple = TRUE, selected = "Lena_Kyusyur", width = "100%",
                               options = list(plugins = list("remove_button"))),
                dateRangeInput("dl_dates", "Date range",
                               start = min(DS_START$daily$date), end = max(DS_START$daily$date),
                               min = min(DS_START$daily$date), max = max(DS_START$daily$date),
                               startview = "decade", width = "100%"),
                div(
                  radioButtons("dl_step", "Time step",
                               c("Daily values" = "daily", "Monthly statistics" = "monthly",
                                 "Annual statistics" = "annual")),
                  conditionalPanel(
                    "input.dl_step == 'daily'",
                    checkboxGroupInput("dl_flags", "Include",
                                       c("Approved (A)" = "A", "Provisional (P)" = "P",
                                         "Days without data (NA)" = "NA"),
                                       selected = c("A", "P", "NA"))
                  ),
                  conditionalPanel("input.dl_step == 'annual'",
                                   helpText("Annual statistics use the year type chosen in the left panel."))
                ),
                div(class = "pt-4", downloadButton("dl_csv", "Download CSV", class = "btn-primary w-100"))
              ),
              uiOutput("dl_cite")
            )
          ),
          card(
            card_header(textOutput("dl_summary", inline = TRUE)),
            DTOutput("dl_table")
          )
        )
      )
    )
  ),

  nav_panel(
    "Water chemistry", value = "chemistry",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        checkboxGroupInput("wq_rivers", "Rivers", choices = WQ_RIVER_CHOICES,
                           selected = WQ_RIVER_CHOICES),
        selectInput("wq_param", "Parameter", choices = wq_param_choices(WQ_START$params),
                    selected = WQ_DEFAULT_PARAM),
        uiOutput("wq_param_desc"),
        sliderInput("wq_years", "Years", min = WQ_YEARS[1], max = WQ_YEARS[2],
                    value = WQ_YEARS, step = 1, sep = ""),
        conditionalPanel(
          "output.wq_has_depth",
          radioButtons("wq_sample", "Samples",
                       c("Surface only" = "surface", "Surface and depth" = "all",
                         "Depth samples only" = "depth"))
        ),
        checkboxGroupInput("wq_flags", "Data quality",
                           c("Accepted (AV)" = "AV", "Provisional (PV)" = "PV",
                             "Caution (CAU)" = "CAU", "Changed (CV)" = "CV",
                             "Below detection (BD)" = "BD"),
                           selected = WQ_DEFAULT_FLAGS),
        checkboxInput("wq_log", "Logarithmic scale", FALSE),
        hr(),
        uiOutput("wq_info")
      ),
      navset_underline(
        id = "wq_tabs",
        nav_panel(
          "Over time",
          card(
            card_header(textOutput("wq_time_title", inline = TRUE)),
            uiOutput("wq_time_ui"),
            card_footer(WQ_SYMBOL_NOTE)
          )
        ),
        nav_panel(
          "Against discharge",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              textOutput("wq_cq_title", inline = TRUE),
              checkboxInput("wq_cq_logx", "Logarithmic discharge axis", TRUE)
            ),
            uiOutput("wq_cq_ui"),
            card_footer("Each point is one sample, plotted against the discharge of the water that was ",
                        "sampled: for the Lena, Yenisey and Kolyma the gauge value is shifted by the ",
                        "travel time between the sampling site and the gauge (see About; Holmes et al. 2011). ",
                        WQ_SYMBOL_NOTE)
          )
        ),
        nav_panel(
          "Two parameters",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              textOutput("wq_xy_title", inline = TRUE),
              div(class = "d-flex align-items-center gap-3 wq-header-controls",
                  div(style = "width: 260px;",
                      selectInput("wq_param_x", NULL, choices = wq_param_choices(WQ_START$params),
                                  selected = "Ca", width = "100%")),
                  checkboxInput("wq_xy_logx", "Log x axis", FALSE))
            ),
            uiOutput("wq_xy_ui"),
            card_footer("Y axis: the parameter chosen in the left panel; x axis: the parameter chosen ",
                        "above. Only samples with values for both are shown. ", WQ_SYMBOL_NOTE)
          )
        ),
        nav_panel(
          "Data & download",
          card(
            card_body(
              class = "pb-0",
              layout_columns(
                col_widths = c(5, 4, 3),
                radioButtons("wq_dl_params", "Parameters",
                             c("The parameter chosen in the left panel" = "one",
                               "All parameters" = "all")),
                radioButtons("wq_dl_shape", "Table layout",
                             c("One row per value (long)" = "long",
                               "One row per sample (wide, like the spreadsheet)" = "wide")),
                div(class = "pt-4", downloadButton("wq_dl_csv", "Download CSV", class = "btn-primary w-100"))
              ),
              helpText("Rivers, years, samples and data quality follow the left panel. ",
                       "Values below detection are given as BD; empty values are not included in the ",
                       "long layout."),
              uiOutput("wq_dl_cite")
            )
          ),
          card(
            card_header(textOutput("wq_dl_summary", inline = TRUE)),
            DTOutput("wq_dl_table")
          )
        )
      )
    )
  ),

  nav_panel(
    "Map", value = "map",
    card(
      full_screen = TRUE,
      card_header("ArcticGRO river basins and gauging stations \u2014 click a station to see its discharge"),
      plotlyOutput("map", height = "760px"),
      card_footer("Scroll to zoom, drag to pan, double-click to reset. ",
                  "Basin map: ArcticGreatRivers.org.")
    )
  ),

  nav_panel("About", value = "about", card(card_body(uiOutput("about")))),

  nav_spacer(),
  nav_item(tags$a("Discharge data on Zenodo", href = paste0("https://doi.org/", CONCEPT_DOI),
                  target = "_blank"))
)

# ---- server ------------------------------------------------------------------------

server <- function(input, output, session) {
  ds <- get_dataset()   # re-checks Zenodo if the cached copy is older than the check interval
  stations <- ds$stations

  station_row <- reactive(stations[station_id == input$station])
  d_station   <- reactive(ds$daily[station_id == input$station])
  years_avail <- reactive(station_years(d_station(), input$year_type))

  # When the station or year type changes: reset years and reference period
  observeEvent(list(input$station, input$year_type), {
    yrs <- years_avail()
    keep <- intersect(as.integer(isolate(input$years)), yrs)
    sel <- if (length(keep)) keep else head(rev(yrs), 2)
    updateSelectizeInput(session, "years",
                         choices = setNames(rev(yrs), year_label(rev(yrs), input$year_type)),
                         selected = sel, server = FALSE)
    ref <- effective_ref(yrs, DEFAULT_REF_PERIOD)
    updateSliderInput(session, "ref", min = min(yrs), max = max(yrs), value = ref)
  })

  ref <- reactive({
    req(input$ref, years_avail())
    effective_ref(years_avail(), input$ref)
  }) |> debounce(300)

  # Each selected year keeps its colour while it stays selected
  year_colors <- reactiveVal(character())
  observeEvent(input$years, {
    cur <- year_colors()
    sel <- input$years %||% character()
    keep <- cur[names(cur) %in% sel]
    for (y in setdiff(sel, names(keep))) {
      free <- setdiff(COL$series, keep)
      keep[y] <- free[1]
    }
    year_colors(keep[order(match(names(keep), sel))])
  }, ignoreNULL = FALSE)

  # ---- sidebar info ------------------------------------------------------------
  output$data_info <- renderUI({
    i <- ds$info
    tags$div(
      class = "small text-muted",
      tags$p(tags$strong("Data version: "), i$version %||% "",
             if (nzchar(i$published %||% "")) paste0(" (", i$published, ")")),
      tags$p(tags$strong("Data through: "), format(i$data_through)),
      tags$p(tags$strong("Source: "), i$source),
      tags$p(tags$strong("Cite: "), cite_text(i))
    )
  })
  output$dl_cite <- renderUI(
    helpText(tags$strong("Please cite the dataset: "), cite_text(ds$info))
  )

  # ---- daily hydrograph --------------------------------------------------------
  clim   <- reactive(daily_climatology(d_station(), ref(), input$year_type))
  series <- reactive(year_series(d_station(), as.integer(input$years %||% integer()), input$year_type))

  output$hydro_title <- renderText({
    sprintf("%s \u2014 daily discharge, %s", station_row()$label,
            if (input$year_type == "water") "water year (Oct\u2013Sep)" else "calendar year")
  })

  output$hydro <- renderPlotly({
    plot_hydrograph(clim(), series(), as.list(year_colors()), ref(), input$year_type, input$log_y)
  })

  output$hydro_table <- renderDT({
    s <- series()
    validate(need(nrow(s) > 0, "Select one or more years."))
    tab <- merge(s[, .(date, year = yr, pdate, discharge_m3s = q, flag)],
                 clim()[, .(pdate, ref_p10 = round(p10), ref_median = round(median), ref_p90 = round(p90))],
                 by = "pdate", all.x = TRUE)[order(date)][, pdate := NULL]
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
              colnames = c("Date", if (input$year_type == "water") "Water year" else "Year",
                           "Discharge (m\u00b3/s)", "Flag", "Reference 10th pct",
                           "Reference median", "Reference 90th pct"))
  })

  # ---- monthly and annual ------------------------------------------------------
  mv <- reactive(monthly_values(d_station()))
  output$monthly_title <- renderText(paste(station_row()$label, "\u2014 monthly mean discharge"))
  output$monthly <- renderPlotly({
    plot_monthly(monthly_climatology(mv(), ref(), input$year_type), mv(),
                 as.list(year_colors()), ref(), input$year_type, input$log_y)
  })

  av <- reactive(annual_values(d_station(), input$year_type))
  output$annual <- renderPlotly(plot_annual(av(), input$metric, ref(), input$year_type))

  output$annual_table <- renderDT({
    a <- av()[n_obs > 0][order(-yr)]
    tab <- a[, .(year = yr, mean = round(mean_q), volume = round(volume_km3, 1),
                 peak = peak_q, peak_date, min = min_q, min_date,
                 completeness = round(100 * completeness), provisional = pct_provisional)]
    datatable(tab, rownames = FALSE, class = "display nowrap",
              options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
              colnames = c(if (input$year_type == "water") "Water year" else "Year",
                           "Mean (m\u00b3/s)", "Volume (km\u00b3)", "Peak (m\u00b3/s)", "Peak date",
                           "Minimum (m\u00b3/s)", "Minimum date", "Days with data (%)",
                           "Provisional (%)")) |>
      formatRound(c("mean", "peak", "min"), digits = 0)
  })

  # ---- map ---------------------------------------------------------------------
  output$map <- renderPlotly(plot_station_map(stations, input$station))
  # Clicks on the map arrive as the input "plotly_click-map" (reading it directly
  # avoids plotly's "event not registered" warning before the Map tab is opened).
  map_click <- reactive({
    v <- input[["plotly_click-map"]]
    if (is.null(v)) NULL else jsonlite::fromJSON(v)
  })
  observeEvent(map_click(), {
    id <- map_click()$customdata[1]
    if (!is.null(id) && id %in% stations$station_id) {
      updateSelectInput(session, "station", selected = id)
      nav_select("nav", "discharge")
    }
  })

  # ---- data & download ---------------------------------------------------------
  observeEvent(input$station, {
    updateSelectizeInput(session, "dl_stations", selected = input$station)
  })

  # Date range follows the period covered by the selected stations
  observeEvent(input$dl_stations, {
    r <- ds$daily[station_id %in% input$dl_stations & !is.na(q), range(date)]
    updateDateRangeInput(session, "dl_dates", start = r[1], end = r[2])
  })

  dl_data <- reactive({
    req(input$dl_stations, input$dl_dates)
    tab <- download_table(ds, input$dl_stations, as.IDate(input$dl_dates[1]),
                          as.IDate(input$dl_dates[2]), input$dl_step, input$year_type)
    if (input$dl_step == "daily") {
      f <- input$dl_flags %||% character()
      tab <- tab[(flag %in% f) | (is.na(flag) & "NA" %in% f)]
    }
    tab
  })

  output$dl_summary <- renderText({
    sprintf("%s rows \u2014 %s", format(nrow(dl_data()), big.mark = ","),
            c(daily = "daily values", monthly = "monthly statistics",
              annual = "annual statistics")[[input$dl_step]])
  })

  output$dl_table <- renderDT({
    datatable(dl_data(), rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      ids <- input$dl_stations
      who <- if (length(ids) == 1) ids else sprintf("%d_stations", length(ids))
      sprintf("arcticgro_discharge_%s_%s_%s_to_%s_%s.csv", who, input$dl_step,
              input$dl_dates[1], input$dl_dates[2], ds$info$version)
    },
    content = function(file) fwrite(dl_data(), file, na = "NA")
  )


  # ---- open a section from the address: ?section=chemistry ----------------------
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    sec <- tolower(q$section %||% "")
    if (sec %in% c("discharge", "chemistry", "map", "about")) nav_select("nav", sec)
  }) |> bindEvent(session$clientData$url_search, once = TRUE)

  # ==== Water chemistry ===========================================================
  wq <- get_wq()   # re-reads the Google Sheet if the cached copy is older than an hour
  # discharge on each sampling day from the discharge dataset (R/data_wq.R)
  wq$values <- wq_attach_discharge(wq$values, ds$daily)
  wq_params <- wq$params

  # depth controls only for particulate parameters
  output$wq_has_depth <- reactive(isTRUE(wq_params[parameter == input$wq_param, has_depth]))
  outputOptions(output, "wq_has_depth", suspendWhenHidden = FALSE)

  output$wq_param_desc <- renderUI({
    d <- wq_params[parameter == input$wq_param, description]
    if (length(d) && nzchar(d)) helpText(class = "mt-n2 mb-3", d)
  })

  # values after the sidebar filters (one parameter, or all)
  wq_filter <- function(params = input$wq_param, sample = input$wq_sample) {
    v <- wq$values[parameter %in% params & river %in% input$wq_rivers &
                     year(date) >= input$wq_years[1] & year(date) <= input$wq_years[2]]
    v <- v[flag %in% input$wq_flags & (flag == "BD" | !is.na(value))]
    if (any(wq_params[parameter %in% params, has_depth])) {
      if (identical(sample, "surface")) v <- v[sample == "surface"]
      if (identical(sample, "depth")) v <- v[sample != "surface" | !(parameter %in% WQ_DEPTH_PARAMS)]
    } else v <- v[sample == "surface"]
    v[, river := factor(river, levels = WQ_RIVER_CHOICES)]
    setorder(v, river, date)
    v[, river := as.character(river)]
    v
  }
  wq_d <- reactive({
    req(input$wq_param, input$wq_rivers)
    wq_filter()
  })

  daily_by_river <- reactive({
    sapply(input$wq_rivers, function(r) ds$daily[station_id == WQ_STATION[[r]], .(date, q)],
           simplify = FALSE)
  })

  n_rivers <- reactive(length(unique(wq_d()$river)))
  plot_box <- function(id, view) renderUI(plotlyOutput(id, height = sprintf("%dpx", wq_height(view, max(1, n_rivers())))))
  output$wq_time_ui  <- plot_box("wq_time", "time")
  output$wq_cq_ui    <- plot_box("wq_cq", "grid")
  output$wq_xy_ui    <- plot_box("wq_xy", "grid")

  plabel <- reactive(param_label(input$wq_param, wq_params))
  output$wq_time_title  <- renderText(sprintf("%s over time", plabel()))
  output$wq_cq_title    <- renderText(sprintf("%s against discharge", plabel()))
  output$wq_xy_title    <- renderText(sprintf("%s against", plabel()))

  output$wq_time <- renderPlotly(
    plot_wq_time(wq_d(), daily_by_river(), input$wq_param, wq_params, input$wq_log, input$wq_years))
  output$wq_cq <- renderPlotly(
    plot_wq_cq(wq_d(), input$wq_param, wq_params, log_y = input$wq_log, log_x = input$wq_cq_logx))
  output$wq_xy <- renderPlotly({
    req(input$wq_param_x)
    y <- wq_d()
    x <- wq_filter(input$wq_param_x)
    key <- c("river", "date", "kit", "sample")
    xy <- merge(y, x[, c(key, "value", "flag"), with = FALSE], by = key, suffixes = c("_y", "_x"))
    xy[, river := factor(river, levels = WQ_RIVER_CHOICES)]
    setorder(xy, river, date); xy[, river := as.character(river)]
    plot_wq_xy(xy, input$wq_param_x, input$wq_param, wq_params,
               log_x = input$wq_xy_logx, log_y = input$wq_log)
  })

  # ---- chemistry table and download ----
  wq_dl_data <- reactive({
    params <- if (identical(input$wq_dl_params, "all")) wq_params$parameter else input$wq_param
    v <- wq_filter(params)
    v <- merge(v, wq_params[, .(parameter, unit)], by = "parameter", sort = FALSE)
    v[, value_out := ifelse(flag == "BD", "BD", format(value, trim = TRUE, drop0trailing = TRUE))]
    if (identical(input$wq_dl_shape, "wide")) {
      v[, col := ifelse(nzchar(unit), sprintf("%s (%s)", parameter, unit), parameter)]
      v[, col := factor(col, levels = unique(col[order(match(parameter, wq_params$parameter))]))]
      w <- dcast(v, river + phase + date + kit + sample + depth_m + q_sample + q_date + q_source ~ col,
                 value.var = c("value_out", "flag"), sep = "|")
      # order columns as value, flag, value, flag ...
      vals <- grep("^value_out\\|", names(w), value = TRUE)
      ord <- as.vector(rbind(vals, sub("^value_out", "flag", vals)))
      setcolorder(w, c("river", "phase", "date", "kit", "sample", "depth_m", "q_sample", "q_date", "q_source", ord))
      setnames(w, sub("^value_out\\|", "", names(w)))
      setnames(w, sub("^flag\\|(.*)$", "\\1 flag", names(w)))
      setnames(w, c("q_sample", "q_date", "q_source"),
               c("discharge_m3s", "discharge_gauge_date", "discharge_source"))
      w[order(match(river, WQ_RIVER_CHOICES), date)]
    } else {
      out <- v[, .(river, phase, date, kit, sample, depth_m, discharge_m3s = q_sample,
                   discharge_gauge_date = q_date, discharge_source = q_source,
                   parameter, value = value_out, unit, flag)]
      out[order(match(river, WQ_RIVER_CHOICES), date, sample, match(parameter, wq_params$parameter))]
    }
  })

  output$wq_dl_summary <- renderText({
    d <- wq_dl_data()
    sprintf("%s rows \u2014 %s, %d\u2013%d", format(nrow(d), big.mark = ","),
            paste(input$wq_rivers, collapse = ", "), input$wq_years[1], input$wq_years[2])
  })
  output$wq_dl_table <- renderDT({
    d <- wq_dl_data()
    datatable(head(d, 5000), rownames = FALSE, filter = "none",
              options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })
  output$wq_dl_csv <- downloadHandler(
    filename = function() {
      who <- if (length(input$wq_rivers) == 1) gsub("'", "", input$wq_rivers) else sprintf("%d_rivers", length(input$wq_rivers))
      what <- if (identical(input$wq_dl_params, "all")) "all_parameters" else gsub("[^A-Za-z0-9]+", "", input$wq_param)
      sprintf("arcticgro_water_quality_%s_%s_%d_%d_%s_v%s.csv", who, what, input$wq_years[1],
              input$wq_years[2], input$wq_dl_shape, wq$info$version)
    },
    content = function(file) fwrite(wq_dl_data(), file, na = "")
  )

  # Suggested citation for the water quality dataset, e.g.
  # The Arctic Great Rivers Observatory. 2026. Water Quality Dataset, Version 20260601.
  #   https://www.arcticgreatrivers.org/data
  wq_cite <- function() {
    v <- wq$info$version
    yr <- if (nzchar(v)) substr(v, 1, 4) else format(Sys.Date(), "%Y")
    tagList(HTML(paste0(
      htmltools::htmlEscape(sprintf(
        "The Arctic Great Rivers Observatory. %s. Water Quality Dataset, Version %s. ", yr, v)),
      sprintf("<a href='%s' target='_blank'>%s</a>", WQ_DATA_PAGE, WQ_DATA_PAGE))))
  }
  wq_metadata_link <- function() a("Metadata (PDF)", href = WQ_METADATA_URL, target = "_blank",
                                   .noWS = "after")
  output$wq_dl_cite <- renderUI(helpText(tags$strong("Please cite the dataset: "), wq_cite(),
                                          " \u00b7 ", wq_metadata_link()))
  output$wq_info <- renderUI({
    i <- wq$info
    tags$div(
      class = "small text-muted",
      tags$p(tags$strong("Data version: "), i$version),
      tags$p(tags$strong("Samples: "), paste0(format(i$n_samples, big.mark = ","),
             ", latest with results ", format(i$last_sample, "%d %b %Y"))),
      tags$p(tags$strong("Source: "), wq_source_ui(i$source)),
      tags$p(tags$strong("Cite: "), wq_cite()),
      tags$p(wq_metadata_link())
    )
  })

  # ---- about -------------------------------------------------------------------
  output$about <- renderUI({
    i <- ds$info
    w <- wq$info
    section <- function(title, ...) div(class = "about-section", h3(title), ...)
    tagList(
      p(class = "lead",
        "The ArcticGRO Data Explorer shows two datasets from the ",
        a("Arctic Great Rivers Observatory (ArcticGRO)", href = "https://www.arcticgreatrivers.org",
          target = "_blank", .noWS = "after"),
        ": daily river discharge, and the chemistry of water samples. They are separate datasets, ",
        "published, versioned and cited separately."),

      section(
        "Discharge dataset",
        h5("What it contains"),
        p("Mean daily river discharge (m\u00b3/s) for 17 gauging stations on 15 rivers draining to ",
          "the Arctic Ocean. Discharge is measured by Roshydromet (Russia), the U.S. Geological ",
          "Survey and the Water Survey of Canada, and compiled by ArcticGRO. Records combine data ",
          "certified by the agencies with provisional data, so each record is as up to date as possible."),
        h5("Data flags"),
        tags$ul(
          tags$li(tags$strong("A, approved: "), "certified by the responsible agency."),
          tags$li(tags$strong("P, provisional: "), "subject to revision; drawn as dotted lines or ",
                  "open markers.")
        ),
        h5("How the statistics are calculated"),
        tags$ul(
          tags$li("Percentile bands and medians use daily values from the reference period chosen in ",
                  "the left panel (default 1991\u20132020). For stations with shorter records the full ",
                  "record is used."),
          tags$li(sprintf("Monthly and annual means are given only when at least %d%% of days have data.",
                          round(100 * MIN_COMPLETENESS))),
          tags$li("Annual volume = annual mean discharge \u00d7 number of days \u00d7 86,400 s."),
          tags$li("Water years run from 1 October to 30 September and are named after the year in ",
                  "which they end."),
          tags$li("Trend lines are ordinary least-squares fits for illustration, not a formal trend test.")
        ),
        h5("Version and source"),
        p(sprintf("Version %s, data through %s (source: %s). Published on Zenodo; the app checks for new ",
                  i$version %||% "?", format(i$data_through), i$source),
          "versions automatically. Documentation, station details and known issues: ",
          a("dataset README", href = paste0(DATA_REPO_URL, "#readme"), target = "_blank", .noWS = "after"), "."),
        h5("How to cite"),
        p(cite_text(ds$info)),
        p(class = "small text-muted",
          "This cites the version shown in the app. The DOI ",
          a(paste0("https://doi.org/", CONCEPT_DOI), href = paste0("https://doi.org/", CONCEPT_DOI),
            target = "_blank", .noWS = "after"),
          " always points to the latest version. Please also acknowledge the original data ",
          "providers: Roshydromet, the U.S. Geological Survey and the Water Survey of Canada. ",
          "Licensed under ",
          a("CC BY 4.0", href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", .noWS = "after"), ".")
      ),

      section(
        "Water chemistry dataset",
        h5("What it contains"),
        p("Chemistry of water samples from the six largest Arctic rivers (Ob', Yenisey, Lena, ",
          "Kolyma, Yukon and Mackenzie), collected near the river mouths since 2003 through the ",
          "PARTNERS project and ArcticGRO I\u2013V: field measurements (temperature, pH, ",
          "conductivity), alkalinity and major ions, dissolved organic carbon, nutrients, trace ",
          "elements, mercury, particulates, water isotopes and absorbance. Particulate samples ",
          "(TSS, POC, PN and their isotopes) were also collected at one or two depths from 2013. ",
          "Full methods and units: ", wq_metadata_link(), "."),
        h5("Data flags"),
        tags$ul(
          tags$li(tags$strong("AV, accepted value: "), "quality control complete (filled markers)."),
          tags$li(tags$strong("PV, provisional value: "), "not yet through the full quality control ",
                  "process (open markers)."),
          tags$li(tags$strong("CAU, caution: "), "quality control complete; value outside the normal ",
                  "range (diamonds)."),
          tags$li(tags$strong("CV, changed value: "), "changed after being recorded as AV."),
          tags$li(tags$strong("BD, below detection: "), "below the detection limit; drawn at the ",
                  "bottom of the plot and given as BD in downloads."),
          tags$li(tags$strong("DV, NC, NA: "), "deleted, not collected, or not yet available; ",
                  "not shown.")
        ),
        h5("In the charts"),
        tags$ul(
          tags$li("Colours show the season of sampling: spring (April\u2013June), summer ",
                  "(July\u2013September) and winter (October\u2013March)."),
          tags$li("Daily discharge in \u201cOver time\u201d is the gauge record from the ArcticGRO ",
                  "discharge dataset described above.")
        ),
        h5("Discharge paired with each sample"),
        p("Each sample is paired with discharge from the ArcticGRO discharge dataset, at these gauges:"),
        tags$table(
          class = "table table-sm about-table",
          tags$thead(tags$tr(tags$th("River"), tags$th("Sampling site"), tags$th("Discharge gauge"),
                             tags$th("Distance"), tags$th("Travel time used"))),
          tags$tbody(
            tags$tr(tags$td("Ob'"), tags$td("Salekhard"), tags$td("Salekhard"), tags$td("same site"), tags$td("same day")),
            tags$tr(tags$td("Yenisey"), tags$td("Dudinka"), tags$td("Igarka"),
                    tags$td("~250 km downstream of the gauge"), tags$td("gauge value 2 days before sampling")),
            tags$tr(tags$td("Lena"), tags$td("Zhigansk"), tags$td("Kyusyur"),
                    tags$td("~520 km upstream of the gauge"), tags$td("gauge value 4 days after sampling")),
            tags$tr(tags$td("Kolyma"), tags$td("Cherskiy"), tags$td("Kolymskoye"),
                    tags$td("~160 km downstream of the gauge"), tags$td("gauge value 1 day before sampling")),
            tags$tr(tags$td("Yukon"), tags$td("Pilot Station"), tags$td("Pilot Station"), tags$td("same site"), tags$td("same day")),
            tags$tr(tags$td("Mackenzie"), tags$td("Tsiigehtchic"), tags$td("Arctic Red River (Tsiigehtchic)"), tags$td("same site"), tags$td("same day"))
          )
        ),
        p("Where the sampling site and the gauge differ, the gauge value is taken on the day the ",
          "sampled water passes the gauge, so that each sample is paired with the discharge of the ",
          "same water. Following Holmes et al. (2011), the travel time is the distance between the ",
          "two sites divided by an assumed river velocity of 1.5 m/s (rounded to whole days); this ",
          "velocity is at the high end of values modelled and observed for these rivers, so the ",
          "adjustment is accurate during high flow and does not over-correct at other times. ",
          "Where the discharge dataset has no value for that day, the discharge recorded with the ",
          "sample is used. Downloads give the gauge date and the source of each discharge value."),
        p(class = "small text-muted", "Reference: ", HOLMES_2011_LINK()),
        h5("Version and source"),
        p(sprintf("Version %s, %s samples, latest results from %s. Source: ",
                  w$version, format(w$n_samples, big.mark = ","), format(w$last_sample, "%d %B %Y")),
          wq_source_ui(w$source), ". ",
          "The app reads the ", wq_sheet_link("ArcticGRO water quality spreadsheet"),
          " every hour, so new results appear as soon as they are entered."),
        h5("How to cite"),
        p(wq_cite())
      ),

      section(
        "Contact",
        p("Questions about the data or this app: ",
          a("info@arcticgreatrivers.org", href = "mailto:info@arcticgreatrivers.org", .noWS = "after"), ".")
      )
    )
  })
}

shinyApp(ui, server)

# plots.R - interactive plotly charts.
# Conventions: one y-axis per chart; reference-period statistics in greys;
# each selected year keeps its own colour; provisional data drawn dotted;
# hover tooltips on every chart.

library(plotly)

UNITS <- "m\u00b3/s"
LOG_TICKS <- as.vector(outer(c(1, 2, 5), 10^(-3:6)))

base_layout <- function(p, ytitle, log_y = FALSE, legend = TRUE) {
  yaxis <- list(title = list(text = ytitle, font = list(family = PLOT_FONT, color = COL$text_2)),
                type = if (log_y) "log" else "linear",
                gridcolor = COL$grid, zeroline = FALSE,
                tickfont = list(family = PLOT_FONT, color = COL$text_2), separatethousands = TRUE,
                hoverformat = ",.0f", rangemode = if (log_y) "normal" else "tozero")
  if (log_y) {
    yaxis$tickvals <- LOG_TICKS
    yaxis$ticktext <- format(LOG_TICKS, big.mark = ",", scientific = FALSE,
                             drop0trailing = TRUE, trim = TRUE)
  }
  p |>
    layout(
      paper_bgcolor = COL$surface, plot_bgcolor = COL$surface,
      font = list(family = PLOT_FONT, color = COL$text, size = 13),
      yaxis = yaxis,
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = -0.14, yanchor = "top",
                    font = list(family = PLOT_FONT, color = COL$text_2)),
      margin = list(l = 70, r = 60, t = 20, b = 10),
      hoverlabel = list(bgcolor = "white", font = list(family = PLOT_FONT, color = COL$text))
    ) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
           toImageButtonOptions = list(format = "png", scale = 2))
}

empty_plot <- function(msg) {
  plot_ly() |>
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           paper_bgcolor = COL$surface, plot_bgcolor = COL$surface,
           annotations = list(list(text = msg, showarrow = FALSE,
                                   font = list(family = PLOT_FONT, color = COL$text_2, size = 14)))) |>
    config(displayModeBar = FALSE)
}

# plotly turns hoverinfo = "skip" on ribbons into one value per point, which the
# "x unified" tooltip ignores (the bands then show up as empty grey rows).
# Setting it back to a single value hides the bands from the tooltip.
fix_skip_hover <- function(p) {
  b <- plotly_build(p)
  for (i in seq_along(b$x$data)) {
    h <- b$x$data[[i]]$hoverinfo
    h <- h[!is.na(h)]
    if (length(h) && all(h == "skip")) b$x$data[[i]]$hoverinfo <- "skip"
  }
  b
}

fmt_num <- function(x) ifelse(is.na(x), "\u2013", formatC(round(x), format = "d", big.mark = ","))

# Year labels at the end of each line: spread them out vertically so labels
# for years that end at similar values don't sit on top of each other.
# labs: data.frame with x, y, text. Returns the vertical shift (px) for each.
spread_labels <- function(labs, y_top, log_y = FALSE, plot_px = 400, min_gap_px = 16) {
  n <- nrow(labs)
  if (n < 2) return(rep(0, n))
  pos_of <- function(v) if (log_y) log10(pmax(v, 1e-6)) else v
  bottom <- if (log_y) min(pos_of(labs$y)) - 1 else 0
  span <- max(pos_of(y_top) - bottom, 1e-9)
  f <- (pos_of(labs$y) - bottom) / span * plot_px          # position in px from the bottom
  shift <- rep(0, n)
  # labels that end close together in time are spread apart, from the lowest up
  xnum <- as.numeric(labs$x)
  for (grp in unique(round(xnum / 20))) {
    idx <- which(round(xnum / 20) == grp)
    if (length(idx) < 2) next
    idx <- idx[order(f[idx])]
    want <- f[idx]
    want[1] <- max(want[1], min_gap_px / 2)
    for (k in seq_along(idx)[-1]) want[k] <- max(want[k], want[k - 1] + min_gap_px)
    over <- max(want) - plot_px                       # keep the stack inside the plot
    if (over > 0) want <- want - over
    shift[idx] <- want - f[idx]
  }
  shift
}

# Split one year's values into an approved (solid) and a provisional (dotted)
# trace that join where the flag changes.
split_by_flag <- function(q, flag) {
  is_p <- flag %in% "P"
  n <- length(is_p)
  prev_not_p <- c(FALSE, !is_p[-n])   # first provisional day joins the approved line
  next_p     <- c(is_p[-1], FALSE)    # last approved day joins the provisional line
  list(approved    = ifelse(!is_p | prev_not_p, q, NA),
       provisional = ifelse(is_p | next_p, q, NA))
}

# ---- daily hydrograph (Charctic style) ----------------------------------------

plot_hydrograph <- function(clim, series, year_colors, ref, year_type, log_y = FALSE,
                            show_bands = TRUE) {
  if (!nrow(series) && !nrow(clim)) return(empty_plot("No data for this selection"))
  p <- plot_ly()
  ref_txt <- sprintf("%d\u2013%d", ref[1], ref[2])
  if (nrow(clim) && show_bands) {
    x <- as.Date(clim$pdate)
    p <- p |>
      add_ribbons(x = x, ymin = clim$min, ymax = clim$max, name = paste("Min\u2013max", ref_txt),
                  fillcolor = COL$band_max, line = list(width = 0), hoverinfo = "skip") |>
      add_ribbons(x = x, ymin = clim$p10, ymax = clim$p90, name = "10th\u201390th percentile",
                  fillcolor = COL$band_p10, line = list(width = 0), hoverinfo = "skip") |>
      add_ribbons(x = x, ymin = clim$p25, ymax = clim$p75, name = "25th\u201375th percentile",
                  fillcolor = COL$band_iqr, line = list(width = 0), hoverinfo = "skip") |>
      add_lines(x = x, y = clim$median, name = paste("Median", ref_txt),
                line = list(color = COL$median, width = 1.5, dash = "dash"),
                text = paste0("10th\u201390th percentile: ", fmt_num(clim$p10), "\u2013", fmt_num(clim$p90)),
                hovertemplate = paste0("Median ", ref_txt, ": %{y:,.0f}<br>%{text}<extra></extra>"))
  }
  labs <- data.frame(x = as.Date(character()), y = numeric(), text = character())
  for (y in names(year_colors)) {
    s <- series[yr == as.integer(y)]
    if (!nrow(s)) next
    col <- year_colors[[y]]
    x <- as.Date(s$pdate)
    parts <- split_by_flag(s$q, s$flag)
    lab <- if (year_type == "water") paste("WY", y) else y
    # drawn lines: solid = approved, dotted = provisional (no tooltip of their own)
    p <- p |>
      add_lines(x = x, y = parts$approved, name = lab, legendgroup = y,
                showlegend = any(!is.na(parts$approved)),
                line = list(color = col, width = 2), connectgaps = FALSE,
                hoverinfo = "skip")
    if (any(!is.na(parts$provisional))) {
      # a year with only provisional data gets its legend entry here
      p <- p |>
        add_lines(x = x, y = parts$provisional, name = lab,
                  legendgroup = y, showlegend = !any(!is.na(parts$approved)),
                  line = list(color = col, width = 2, dash = "dot"), connectgaps = FALSE,
                  hoverinfo = "skip")
    }
    # one invisible line per year carries the tooltip, so each year appears once
    # and says whether that day's value is provisional
    p <- p |>
      add_lines(x = x, y = s$q, name = lab, legendgroup = y, showlegend = FALSE,
                opacity = 0, line = list(color = col, width = 2), connectgaps = FALSE,
                text = paste0(sprintf("<span style='color:%s'>\u2501\u2501</span> ", col), lab,
                              ifelse(s$flag %in% "P", " (provisional)", "")),
                hovertemplate = "%{text}: %{y:,.0f}<extra></extra>")
    # direct label at the last value of the year (added below, once all are known)
    last <- tail(s[!is.na(q)], 1)
    if (nrow(last))
      labs <- rbind(labs, data.frame(x = as.Date(last$pdate), y = last$q,
                                     text = sprintf("<span style='color:%s'>\u2501</span> %s", col, lab)))
  }
  if (nrow(labs)) {
    y_top <- max(c(labs$y, if (nrow(clim) && show_bands) clim$max, series$q), na.rm = TRUE)
    if (!log_y) y_top <- y_top * 1.05
    shift <- spread_labels(labs, y_top, log_y)
    for (i in seq_len(nrow(labs)))
      p <- p |> add_annotations(x = labs$x[i], y = if (log_y) log10(labs$y[i]) else labs$y[i],
                                text = labs$text[i], xanchor = "left", yanchor = "middle",
                                showarrow = FALSE, xshift = 4, yshift = shift[i],
                                font = list(family = PLOT_FONT, color = COL$text_2, size = 12))
  }
  start <- if (year_type == "water") as.Date("2001-10-01") else as.Date("2001-01-01")
  base_layout(p, paste0("Discharge (", UNITS, ")"), log_y) |>
    layout(hovermode = "x unified",
           xaxis = list(title = "", type = "date", tickformat = "%b", dtick = "M1",
                        hoverformat = "%b %d", showgrid = FALSE,
                        tickfont = list(family = PLOT_FONT, color = COL$text_2),
                        range = c(start - 1, seq(start, by = "1 year", length.out = 2)[2] + 12))) |>
    fix_skip_hover()
}

# ---- monthly means against the reference period -----------------------------------

plot_monthly <- function(mclim, mv_years, year_colors, ref, year_type, log_y = FALSE) {
  if (!nrow(mclim) && !nrow(mv_years)) return(empty_plot("No data for this selection"))
  order_m <- if (year_type == "water") c(10:12, 1:9) else 1:12
  pos <- function(m) match(m, order_m)
  p <- plot_ly()
  ref_txt <- sprintf("%d\u2013%d", ref[1], ref[2])
  if (nrow(mclim)) {
    mc <- mclim[order(pos(month))]
    x <- pos(mc$month)
    p <- p |>
      add_ribbons(x = x, ymin = mc$min, ymax = mc$max, name = paste("Min\u2013max", ref_txt),
                  fillcolor = COL$band_max, line = list(width = 0), hoverinfo = "skip") |>
      add_ribbons(x = x, ymin = mc$p25, ymax = mc$p75, name = "25th\u201375th percentile",
                  fillcolor = COL$band_iqr, line = list(width = 0), hoverinfo = "skip") |>
      add_lines(x = x, y = mc$median, name = paste("Median", ref_txt),
                line = list(color = COL$median, width = 1.5, dash = "dash"),
                hovertemplate = "Median: %{y:,.0f}<extra></extra>")
  }
  for (y in names(year_colors)) {
    yy <- as.integer(y)
    s <- if (year_type == "water")
      mv_years[(year == yy & month <= 9) | (year == yy - 1 & month >= 10)] else mv_years[year == yy]
    if (!nrow(s)) next
    s <- s[!is.na(mean_q)][order(pos(month))]
    if (!nrow(s)) next
    lab <- if (year_type == "water") paste("WY", y) else y
    p <- p |>
      add_trace(x = pos(s$month), y = s$mean_q, type = "scatter", mode = "lines+markers",
                name = lab, line = list(color = year_colors[[y]], width = 2),
                marker = list(color = ifelse(s$provisional, COL$surface, year_colors[[y]]),
                              size = 8, symbol = "circle",
                              line = list(color = year_colors[[y]], width = 2)),
                connectgaps = FALSE,
                hovertemplate = paste0(lab, ": %{y:,.0f}<extra></extra>"))
  }
  base_layout(p, paste0("Monthly mean discharge (", UNITS, ")"), log_y) |>
    layout(hovermode = "x unified",
           xaxis = list(title = "", tickvals = 1:12, ticktext = MONTH_LABELS[order_m],
                        showgrid = FALSE, tickfont = list(family = PLOT_FONT, color = COL$text_2), range = c(0.6, 12.6))) |>
    fix_skip_hover()
}

# ---- annual statistic over the whole record ----------------------------------------

ANNUAL_METRICS <- c(
  "Mean discharge (m\u00b3/s)" = "mean_q",
  "Annual volume (km\u00b3)"    = "volume_km3",
  "Peak daily discharge (m\u00b3/s)" = "peak_q",
  "Minimum daily discharge (m\u00b3/s)" = "min_q",
  "Day of annual peak (days from start of year)" = "peak_day"
)

plot_annual <- function(av, metric, ref, year_type) {
  label <- names(ANNUAL_METRICS)[ANNUAL_METRICS == metric]
  d <- av[!is.na(get(metric))]
  # peak/min only meaningful for (nearly) complete years
  if (metric %in% c("peak_q", "min_q", "peak_day")) d <- d[completeness >= MIN_COMPLETENESS]
  if (nrow(d) < 2) return(empty_plot("Not enough complete years for this statistic"))
  v <- d[[metric]]
  fmt <- if (metric == "volume_km3") ",.1f" else ",.0f"
  ylab <- if (year_type == "water") "Water year" else "Year"
  p <- plot_ly() |>
    add_trace(x = d$yr, y = v, type = "scatter", mode = "lines", name = label,
              line = list(color = COL$series[1], width = 2),
              hovertemplate = paste0(ylab, " %{x}: %{y:", fmt, "}<extra></extra>"))
  prov <- d$pct_provisional > 0
  if (any(prov))
    p <- p |> add_trace(x = d$yr[prov], y = v[prov], type = "scatter", mode = "markers",
                        name = "Includes provisional data",
                        marker = list(color = COL$surface, size = 8, symbol = "circle",
                                      line = list(color = COL$series[1], width = 2)),
                        hoverinfo = "skip")
  in_ref <- d$yr >= ref[1] & d$yr <= ref[2]
  if (sum(in_ref) >= MIN_REF_YEARS) {
    m <- mean(v[in_ref])
    p <- p |> add_lines(x = range(d$yr), y = c(m, m),
                        name = sprintf("Mean %d\u2013%d", ref[1], ref[2]),
                        line = list(color = COL$median, width = 1.5, dash = "dash"),
                        hoverinfo = "skip")
  }
  if (nrow(d) >= 10) {
    fit <- lm(v ~ d$yr)
    slope <- coef(fit)[2]
    p <- p |> add_lines(x = d$yr, y = fitted(fit),
                        name = sprintf("Linear trend (%s%s per decade)",
                                       formatC(slope * 10, format = "fg", digits = 3, flag = "+"),
                                       if (metric == "peak_day") " days" else ""),
                        line = list(color = COL$text_2, width = 1.5, dash = "dot"),
                        hoverinfo = "skip")
  }
  base_layout(p, label) |>
    layout(hovermode = "closest",
           yaxis = list(rangemode = if (metric == "peak_day") "normal" else "tozero"),
           xaxis = list(title = "", showgrid = FALSE, tickfont = list(family = PLOT_FONT, color = COL$text_2)))
}

# ---- station map (ArcticGRO basin map with clickable stations) -----------------------

# latitude/longitude -> pixel position on the basin map image
map_xy <- function(lat, lon) {
  r <- MAP_K * tan((90 - lat) * pi / 360)
  list(x = MAP_CX + r * sin(lon * pi / 180), y = MAP_CY + r * cos(lon * pi / 180))
}

plot_station_map <- function(stations, selected_id) {
  pos <- map_xy(stations$latitude, stations$longitude)
  sel <- stations$station_id == selected_id
  hover <- sprintf("<b>%s</b><br>%s, station %s<br>Drainage area: %s km\u00b2<br>Record: %s to %s<br><i>Click to select</i>",
                   stations$label, stations$agency, stations$station_code,
                   format(stations$drainage_area_km2, big.mark = ","),
                   stations$start_date, stations$end_date)
  dot <- "#104281"
  p <- plot_ly(source = "map") |>
    add_trace(type = "scatter", mode = "markers", x = pos$x[!sel], y = pos$y[!sel],
              text = hover[!sel], hoverinfo = "text", customdata = stations$station_id[!sel],
              name = "Gauging station",
              marker = list(size = 13, color = dot, line = list(color = "white", width = 2.5))) |>
    add_trace(type = "scatter", mode = "markers", x = pos$x[sel], y = pos$y[sel],
              text = hover[sel], hoverinfo = "text", customdata = stations$station_id[sel],
              name = "Selected station",
              marker = list(size = 22, color = "white", line = list(color = dot, width = 6)))
  if (any(sel))
    p <- p |> add_annotations(x = pos$x[sel], y = pos$y[sel], text = stations$label[sel],
                              showarrow = FALSE, yshift = 24, bgcolor = "rgba(255,255,255,0.9)",
                              bordercolor = dot, borderpad = 4,
                              font = list(family = PLOT_FONT, color = COL$text, size = 14))
  ax <- list(range = c(0, MAP_SIZE), visible = FALSE, fixedrange = FALSE)
  p |>
    layout(
      images = list(list(source = MAP_IMAGE, xref = "x", yref = "y", x = 0, y = 0,
                         sizex = MAP_SIZE, sizey = MAP_SIZE, xanchor = "left", yanchor = "top",
                         sizing = "stretch", layer = "below")),
      xaxis = c(ax, list(constrain = "domain")),
      yaxis = c(list(range = c(MAP_SIZE, 0), visible = FALSE, scaleanchor = "x",
                     constrain = "domain")),
      dragmode = "pan",
      paper_bgcolor = COL$surface, plot_bgcolor = COL$surface,
      margin = list(l = 0, r = 0, t = 0, b = 0), showlegend = FALSE,
      hoverlabel = list(bgcolor = "white", font = list(family = PLOT_FONT, color = COL$text))
    ) |>
    config(displaylogo = FALSE, scrollZoom = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toImage")) |>
    event_register("plotly_click")
}

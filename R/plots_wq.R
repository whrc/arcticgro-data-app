# plots_wq.R - water quality (chemistry) charts.
# Conventions: one panel per river (small multiples) so each river keeps its own
# discharge scale and at most three colours (seasons) share a panel; marker shape
# shows data quality and sample depth; hover tooltips on every point.

library(plotly)

# Marker symbol for each point: surface = circle, depth sample = square;
# provisional = open; caution = diamond; below detection = open triangle-down
wq_symbol <- function(flag, sample) {
  base <- ifelse(sample == "surface", "circle", "square")
  out <- ifelse(flag == "PV", paste0(base, "-open"), base)
  out[flag == "CAU"] <- "diamond"
  out[flag == "BD"] <- "triangle-down-open"
  out
}

wq_flag_text <- function(flag) {
  lab <- c(AV = "Accepted value", PV = "Provisional value",
           CAU = "Caution: outside the normal range", CV = "Changed value (corrected after QC)",
           DV = "Deleted value", BD = "Below detection limit")
  out <- unname(lab[flag]); out[is.na(out)] <- flag[is.na(out)]; out
}

param_label <- function(p, params) {
  u <- params[parameter == p, unit]
  if (length(u) && nzchar(u)) sprintf("%s (%s)", p, u) else p
}

# Hover text for chemistry points
wq_hover <- function(d, p, unit) {
  val <- ifelse(d$flag == "BD", "below detection",
                paste(formatC(d$value, format = "fg", digits = 4, big.mark = ","), unit))
  depth <- ifelse(d$sample == "surface", "Surface sample",
                  ifelse(is.na(d$depth_m), "Depth sample", sprintf("Depth sample, %s m", d$depth_m)))
  q <- if (!"q_source" %in% names(d)) rep("", nrow(d)) else
    ifelse(is.na(d$q_sample), "",
           sprintf("<br>Discharge: %s m³/s<br><i>%s</i>",
                   formatC(d$q_sample, format = "d", big.mark = ","),
                   sub("^discharge dataset: ", "", d$q_source)))
  sprintf("<b>%s</b> · %s<br>%s: %s<br>%s<br>%s%s<br>%s",
          d$river, format(d$date, "%d %b %Y"),
          p, val, wq_flag_text(d$flag), depth, q, d$phase)
}

# y position for below-detection points: 0 on a linear axis, just under the
# smallest value on a log axis
bd_y <- function(d, log_y) {
  y <- d$value
  if (log_y) {
    pos <- d$value[d$value > 0 & d$flag != "BD"]
    floor_y <- if (length(pos)) min(pos) * 0.6 else 1e-3
    y[d$flag == "BD" | y <= 0] <- floor_y
  } else y[d$flag == "BD"] <- 0
  y
}

wq_axis <- function(title, log = FALSE, ...) {
  extra <- list(...)
  c(list(title = list(text = title, font = list(family = PLOT_FONT, color = COL$text_2, size = 12)),
         type = extra$type %||% (if (log) "log" else "-"), gridcolor = COL$grid, zeroline = FALSE,
         tickfont = list(family = PLOT_FONT, color = COL$text_2, size = 11),
         separatethousands = TRUE, exponentformat = "none"),
    if (log && !identical(extra$type, "date")) list(dtick = "D2"),
    extra[names(extra) != "type"])
}

panel_title <- function(text) {
  list(text = sprintf("<b>%s</b>", text), x = 0, y = 1, xref = "paper", yref = "paper",
       xanchor = "left", yanchor = "bottom", showarrow = FALSE, yshift = 2,
       font = list(family = PLOT_FONT, color = COL$text, size = 13))
}

wq_finish <- function(p, legend = TRUE, top = 64) {
  p |>
    layout(paper_bgcolor = COL$surface, plot_bgcolor = COL$surface,
           font = list(family = PLOT_FONT, color = COL$text, size = 12),
           showlegend = legend,
           legend = list(orientation = "h", x = 0, xanchor = "left", y = 1, yanchor = "top",
                         yref = "container", itemclick = FALSE, itemdoubleclick = FALSE,
                         font = list(family = PLOT_FONT, color = COL$text_2)),
           margin = list(l = 70, r = 20, t = top, b = 40),
           hoverlabel = list(bgcolor = "white", align = "left",
                             font = list(family = PLOT_FONT, color = COL$text, size = 12))) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
           toImageButtonOptions = list(format = "png", scale = 2))
}

# Season-coloured points for one panel; legend shown once (first panel)
add_season_points <- function(p, d, x, y, hover, show_legend) {
  d$season <- wq_season(d$date)
  for (k in seq_along(levels(d$season))) {
    s <- levels(d$season)[k]
    i <- which(d$season == s)
    if (!length(i)) {
      # keep the legend entry even if this panel has no points in this season
      if (show_legend) p <- p |> add_markers(x = NA_real_, y = NA_real_, name = s, legendgroup = s,
                                             marker = list(color = SEASON_COL[k], size = 9),
                                             hoverinfo = "skip")
      next
    }
    p <- p |> add_markers(x = x[i], y = y[i], name = s, legendgroup = s, showlegend = show_legend,
                          text = hover[i], hoverinfo = "text",
                          marker = list(color = SEASON_COL[k], size = 9, opacity = 0.85,
                                        symbol = wq_symbol(d$flag[i], d$sample[i]),
                                        line = list(color = SEASON_COL[k], width = 1.5)))
  }
  p
}

wq_grid <- function(n) {
  ncol <- if (n <= 1) 1 else if (n <= 4) 2 else 3
  list(ncol = ncol, nrow = ceiling(n / ncol))
}

# ---- 1. over time ---------------------------------------------------------------------
# One river: discharge (top) and the parameter (bottom) on a shared time axis.
# Several rivers: one panel per river, shared y axis for comparison.
plot_wq_time <- function(d, daily_by_river, p, params, log_y = FALSE, years = NULL) {
  if (!nrow(d)) return(empty_plot("No values for this selection"))
  unit <- params[parameter == p, unit]
  ylab <- param_label(p, params)
  rivers <- unique(d$river)
  xr <- if (!is.null(years)) as.Date(c(sprintf("%d-01-01", years[1]), sprintf("%d-12-31", years[2])))
  panels <- lapply(seq_along(rivers), function(k) {
    r <- rivers[k]; dr <- d[river == r]
    y <- bd_y(dr, log_y)
    pp <- plot_ly() |> add_season_points(dr, as.Date(dr$date), y, wq_hover(dr, p, unit), show_legend = k == 1) |>
      layout(xaxis = wq_axis("", type = "date", range = xr, showgrid = FALSE),
             yaxis = wq_axis(if (length(rivers) == 1) ylab else "", log_y),
             annotations = list(panel_title(if (length(rivers) == 1) ylab else r)))
    pp
  })
  if (length(rivers) == 1) {
    r <- rivers[1]; q <- daily_by_river[[r]]
    qp <- plot_ly()
    if (!is.null(q) && nrow(q)) {
      if (!is.null(xr)) q <- q[date >= xr[1] & date <= xr[2]]
      qp <- qp |> add_lines(x = as.Date(q$date), y = q$q, name = "Daily discharge",
                            line = list(color = COL$median, width = 1), showlegend = FALSE,
                            hovertemplate = "%{x|%d %b %Y}<br>Discharge: %{y:,.0f} m³/s<extra></extra>")
    }
    dr <- unique(d[!is.na(q_sample), .(date, q_date, q_sample)])
    if (nrow(dr)) {
      lag <- unname(WQ_TRAVEL_DAYS[r]) %||% 0L
      note <- if (lag == 0) "" else if (lag > 0)
        sprintf("<br>Water reaches the gauge %d days later", lag) else
        sprintf("<br>Water passed the gauge %d day%s earlier", -lag, if (lag == -1) "" else "s")
      qp <- qp |> add_markers(x = as.Date(dr$q_date), y = dr$q_sample, name = "Sampling days",
                              text = format(dr$date, "%d %b %Y"),
                              marker = list(color = COL$text, size = 5), showlegend = FALSE,
                              hovertemplate = paste0("Sampled %{text}", note,
                                                     "<br>Discharge: %{y:,.0f} m³/s<extra></extra>"))
    }
    qp <- qp |> layout(xaxis = wq_axis("", type = "date", range = xr, showgrid = FALSE),
                       yaxis = wq_axis("m³/s", rangemode = "tozero"),
                       annotations = list(panel_title(sprintf("%s — daily discharge at %s (dots: samples)", r, WQ_GAUGE_NAME[[r]]))))
    out <- subplot(qp, panels[[1]], nrows = 2, heights = c(0.32, 0.68), shareX = TRUE,
                   titleY = TRUE, margin = 0.07)
  } else {
    out <- subplot(panels, nrows = length(rivers), shareX = TRUE, shareY = TRUE,
                   titleY = TRUE, margin = 0.035) |>
      layout(yaxis = wq_axis(ylab, log_y))
    # same y scale in every river panel, so rivers can be compared by eye
    for (k in seq_along(rivers)[-1])
      out$x$layout[[paste0("yaxis", k)]] <- c(out$x$layout[[paste0("yaxis", k)]] %||% list(),
                                               list(matches = "y"))
  }
  wq_finish(out) |> layout(hovermode = "closest")
}

# ---- 2. against discharge --------------------------------------------------------------
plot_wq_cq <- function(d, p, params, log_y = TRUE, log_x = TRUE) {
  d <- d[!is.na(q_sample)]
  if (!nrow(d)) return(empty_plot("No values with discharge for this selection"))
  unit <- params[parameter == p, unit]
  rivers <- unique(d$river); g <- wq_grid(length(rivers))
  panels <- lapply(seq_along(rivers), function(k) {
    dr <- d[river == rivers[k]]
    plot_ly() |> add_season_points(dr, dr$q_sample, bd_y(dr, log_y), wq_hover(dr, p, unit),
                                   show_legend = k == 1) |>
      layout(xaxis = wq_axis(if (k > length(rivers) - g$ncol) "Discharge (m³/s)" else "", log_x),
             yaxis = wq_axis(if ((k - 1) %% g$ncol == 0) param_label(p, params) else "", log_y),
             annotations = list(panel_title(rivers[k])))
  })
  out <- subplot(panels, nrows = g$nrow, shareX = FALSE, shareY = FALSE, titleX = TRUE,
                 titleY = TRUE, margin = c(0.05, 0.03, 0.06, 0.06))
  wq_finish(out) |> layout(hovermode = "closest")
}

# ---- 3. two parameters -----------------------------------------------------------------
# dx, dy: values of the x and y parameters, matched by river, date, kit and sample
plot_wq_xy <- function(dxy, px, py, params, log_x = FALSE, log_y = FALSE) {
  if (!nrow(dxy)) return(empty_plot("No samples with both parameters for this selection"))
  ux <- params[parameter == px, unit]; uy <- params[parameter == py, unit]
  rivers <- unique(dxy$river); g <- wq_grid(length(rivers))
  panels <- lapply(seq_along(rivers), function(k) {
    dr <- dxy[river == rivers[k]]
    dy <- copy(dr)[, `:=`(value = value_y, flag = flag_y)]
    dx <- copy(dr)[, `:=`(value = value_x, flag = flag_x)]
    hover <- paste0(wq_hover(dy, py, uy), "<br>",
                    px, ": ", ifelse(dx$flag == "BD", "below detection",
                                     paste(formatC(dx$value, format = "fg", digits = 4, big.mark = ","), ux)))
    worst <- ifelse(dy$flag == "BD" | dx$flag == "BD", "BD",
             ifelse(dy$flag == "CAU" | dx$flag == "CAU", "CAU",
             ifelse(dy$flag == "PV" | dx$flag == "PV", "PV", dy$flag)))
    dy$flag <- worst
    plot_ly() |> add_season_points(dy, bd_y(dx, log_x), bd_y(dy, log_y), hover, show_legend = k == 1) |>
      layout(xaxis = wq_axis(if (k > length(rivers) - g$ncol) param_label(px, params) else "", log_x),
             yaxis = wq_axis(if ((k - 1) %% g$ncol == 0) param_label(py, params) else "", log_y),
             annotations = list(panel_title(rivers[k])))
  })
  out <- subplot(panels, nrows = g$nrow, shareX = FALSE, shareY = FALSE, titleX = TRUE,
                 titleY = TRUE, margin = c(0.05, 0.03, 0.06, 0.06))
  wq_finish(out) |> layout(hovermode = "closest")
}

# Plot height (px) for a number of river panels
wq_height <- function(view, n) {
  if (view == "time") return(if (n <= 1) 560 else 70 + 150 * n)
  70 + 290 * wq_grid(n)$nrow
}

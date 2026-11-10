# stats.R - statistics behind the plots and tables.
# All discharge values are m3/s. "Year" is either the calendar year or the
# water year (1 Oct - 30 Sep, named after the year in which it ends, as USGS does).

library(data.table)

MONTH_LABELS <- month.abb

# Days in a given month (vectorised)
days_in_month <- function(y, m) {
  start <- as.IDate(sprintf("%d-%02d-01", y, m))
  nxt   <- as.IDate(sprintf("%d-%02d-01", y + (m == 12), m %% 12 + 1))
  as.integer(nxt - start)
}

# Add the year (calendar or water) and a plotting date on a common axis.
# Feb 29 has no place on a 365-day axis, so it gets pdate = NA.
add_year_cols <- function(d, year_type = "calendar") {
  d <- copy(d)
  m  <- month(d$date)
  md <- sprintf("%02d-%02d", m, mday(d$date))
  if (year_type == "water") {
    d[, yr := year(date) + (m >= 10)]
    axis_year <- ifelse(m >= 10, 2001L, 2002L)   # axis runs 1 Oct 2001 - 30 Sep 2002
  } else {
    d[, yr := year(date)]
    axis_year <- 2001L
  }
  d[, pdate := as.IDate(ifelse(md == "02-29", NA_character_, paste0(axis_year, "-", md)))]
  d[]
}

# Years with at least one value
station_years <- function(d, year_type = "calendar") {
  sort(unique(add_year_cols(d[!is.na(q)], year_type)$yr))
}

# Reference period actually used: requested period clipped to available data;
# falls back to the full record when the overlap is too short.
effective_ref <- function(years, ref) {
  r <- c(max(ref[1], min(years)), min(ref[2], max(years)))
  if (r[2] - r[1] + 1 < MIN_REF_YEARS) r <- range(years)
  as.integer(r)
}

# Percentiles of daily discharge for each day of the year over the reference period
daily_climatology <- function(d, ref, year_type = "calendar") {
  x <- add_year_cols(d[!is.na(q)], year_type)[yr >= ref[1] & yr <= ref[2] & !is.na(pdate)]
  if (!nrow(x)) return(data.table())
  out <- x[, .(n = .N,
               min = min(q), p10 = quantile(q, 0.10, names = FALSE),
               p25 = quantile(q, 0.25, names = FALSE), median = median(q),
               p75 = quantile(q, 0.75, names = FALSE),
               p90 = quantile(q, 0.90, names = FALSE), max = max(q)),
           by = pdate][n >= MIN_REF_YEARS]
  setorder(out, pdate)
  out
}

# Daily values for selected years on the common axis
year_series <- function(d, years, year_type = "calendar") {
  x <- add_year_cols(d, year_type)[yr %in% years & !is.na(pdate)]
  setorder(x, yr, pdate)
  x
}

# Monthly statistics for every station/month. A mean is only given when at
# least MIN_COMPLETENESS of the days have data.
monthly_values <- function(d) {
  x <- d[, .(n_obs = sum(!is.na(q)),
             mean_q = mean(q, na.rm = TRUE),
             min_q  = suppressWarnings(min(q, na.rm = TRUE)),
             max_q  = suppressWarnings(max(q, na.rm = TRUE)),
             provisional = any(flag == "P", na.rm = TRUE)),
         by = .(station_id, year = year(date), month = month(date))]
  x[, n_days := days_in_month(year, month)]
  x[, complete := n_obs / n_days >= MIN_COMPLETENESS]
  x[complete == FALSE, mean_q := NA_real_]
  x[n_obs == 0, c("min_q", "max_q") := NA_real_]
  setorder(x, station_id, year, month)
  x[]
}

# Median and range of monthly means across the reference period
monthly_climatology <- function(mv, ref, year_type = "calendar") {
  x <- copy(mv)[!is.na(mean_q)]
  x[, yr := if (year_type == "water") year + (month >= 10) else year]
  x <- x[yr >= ref[1] & yr <= ref[2]]
  x[, .(n = .N, min = min(mean_q), p25 = quantile(mean_q, 0.25, names = FALSE),
        median = median(mean_q), p75 = quantile(mean_q, 0.75, names = FALSE),
        max = max(mean_q)), by = month][n >= MIN_REF_YEARS][order(month)]
}

# Annual statistics per station and year
annual_values <- function(d, year_type = "calendar") {
  x <- copy(d)
  x[, yr := if (year_type == "water") year(date) + (month(date) >= 10) else year(date)]
  x <- x[, {
    has <- !is.na(q)
    leap <- (yr %% 4 == 0 & yr %% 100 != 0) | yr %% 400 == 0
    n_exp <- 365L + leap
    i_max <- if (any(has)) which.max(q) else NA_integer_
    i_min <- if (any(has)) which.min(q) else NA_integer_
    .(n_obs = sum(has), n_days = n_exp,
      mean_q = mean(q, na.rm = TRUE),
      peak_q = if (any(has)) q[i_max] else NA_real_,
      peak_date = if (any(has)) date[i_max] else as.IDate(NA),
      min_q = if (any(has)) q[i_min] else NA_real_,
      min_date = if (any(has)) date[i_min] else as.IDate(NA),
      pct_provisional = round(100 * mean(flag[has] == "P"), 1))
  }, by = .(station_id, yr)]
  x[, completeness := round(n_obs / n_days, 3)]
  x[completeness < MIN_COMPLETENESS, mean_q := NA_real_]
  x[, volume_km3 := mean_q * 86400 * n_days / 1e9]
  x[, peak_day := as.integer(peak_date - as.IDate(sprintf(
        "%d-%s", yr - (year_type == "water"), if (year_type == "water") "10-01" else "01-01"))) + 1L]
  x[n_obs == 0, pct_provisional := NA_real_]
  setorder(x, station_id, yr)
  x[]
}

# ---- tables for the Data & download tab -------------------------------------

download_table <- function(ds, station_ids, from, to, step, year_type = "calendar") {
  st <- ds$stations[, .(station_id, river, station = station_name, station_code, agency)]
  d <- ds$daily[station_id %in% station_ids]
  if (step == "daily") {
    out <- d[date >= from & date <= to,
             .(station_id, date, discharge_m3s = q, flag)]
  } else if (step == "monthly") {
    mv <- monthly_values(d)
    mv <- mv[as.IDate(sprintf("%d-%02d-01", year, month)) >= as.IDate(format(from, "%Y-%m-01")) &
               as.IDate(sprintf("%d-%02d-01", year, month)) <= to]
    out <- mv[, .(station_id, year, month,
                  mean_m3s = round(mean_q, 2), min_m3s = min_q, max_m3s = max_q,
                  days_with_data = n_obs, days_in_month = n_days,
                  includes_provisional = provisional)]
  } else {
    av <- annual_values(d, year_type)
    y_from <- if (year_type == "water") year(from) + (month(from) >= 10) else year(from)
    y_to   <- if (year_type == "water") year(to) + (month(to) >= 10) else year(to)
    av <- av[yr >= y_from & yr <= y_to]
    out <- av[, .(station_id, year = yr,
                  year_type = if (year_type == "water") "water year (Oct-Sep)" else "calendar year",
                  mean_m3s = round(mean_q, 2), volume_km3 = round(volume_km3, 2),
                  peak_m3s = peak_q, peak_date, min_m3s = min_q, min_date,
                  days_with_data = n_obs, completeness, pct_provisional)]
  }
  out <- merge(st, out, by = "station_id", sort = FALSE)
  out[, station_id := NULL]
  setorderv(out, intersect(c("river", "station", "date", "year", "month"), names(out)))
  out[]
}

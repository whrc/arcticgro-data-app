# data_wq.R - load the ArcticGRO water quality (chemistry) data.
#
# Source: the master Google Sheet (WQ_SHEET_URL in config.R), downloaded as an
# Excel file. The sheet has one tab per river; each tab has a few title rows,
# a header row (Phase, River, Date, ID, then each parameter twice: value and
# flag), a units row, then one row per sampling date. Particulate parameters
# can also have one or two depth samples ("Sample depth 1", "TSS-one", ...).
#
# get_wq() returns a list:
#   values  data.table, one row per sample and parameter:
#           river, phase, date, kit, sample (surface / depth 1 / depth 2), depth_m,
#           q_sample (discharge on the sample day, m3/s), parameter, value, flag
#   params  data.table: parameter, unit, description, group, has_depth
#   flags   data.table: flag, label, description
#   info    list: version, source, loaded_at, n_samples, last_sample
#
# Behaviour (same pattern as the discharge data):
#   - re-downloads the sheet at most every WQ_CHECK_INTERVAL_HOURS
#   - checks every download; if a check fails or Google can't be reached, keeps
#     the last good copy, or falls back to the bundled copy in data-fallback/

library(data.table)

.wq <- new.env()

WQ_RIVERS <- c("Ob", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")

# Parameter groups (order = order in the app's parameter menu)
WQ_GROUPS <- list(
  "Field measurements"        = c("Temperature", "pH", "Conductivity"),
  "Dissolved organic carbon"  = c("DOC", "DOC-13C", "DOC-14C", "DOC-fm"),
  "Nutrients"                 = c("TDN", "NO3", "NH4", "TDP", "SRP", "SiO2"),
  "Alkalinity and major ions" = c("Alkalinity", "Ca", "Mg", "Na", "K", "Cl", "SO4"),
  "Trace elements"            = c("As", "Ba", "F", "Li", "Rb", "Sr", "U"),
  "Mercury"                   = c("UTHg", "UMeHg"),
  "Particulates"              = c("TSS", "POC", "POC-13C", "POC-14C", "POC-fm", "PN", "PN-15N"),
  "Water isotopes"            = c("H2O-18O", "H2O-2H"),
  "Absorbance (CDOM)"         = c("A254", "A375", "A412", "A420", "A440")
)
# Particulate parameters that also have depth samples (column suffix -one / -two)
WQ_DEPTH_PARAMS <- c("TSS", "POC", "POC-13C", "PN", "PN-15N")

WQ_FLAG_LABELS <- c(AV = "Accepted value", PV = "Provisional value", CAU = "Caution",
                    CV = "Changed value", DV = "Deleted value", BD = "Below detection",
                    NC = "Not collected", NA_ = "Not available yet")

# ---- parsing the workbook ---------------------------------------------------------

.cell_chr <- function(x) {
  x <- as.character(x)
  x[!is.na(x)] <- trimws(x[!is.na(x)])
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# One river tab -> long table
parse_wq_sheet <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                             col_types = "list", .name_repair = "minimal"))
  raw <- as.data.frame(raw)
  first <- vapply(raw[[1]], function(v) if (length(v) && !is.na(v[[1]])) as.character(v[[1]]) else NA_character_, "")
  hdr_row <- which(trimws(first) == "Phase")[1]
  if (is.na(hdr_row)) stop("no header row (\"Phase\") in tab ", sheet)
  cell <- function(i, j) { v <- raw[[j]][[i]]; if (length(v) && !is.na(v)) v else NA }
  hdr   <- vapply(seq_along(raw), function(j) { v <- cell(hdr_row, j); if (is.na(v)) "" else trimws(as.character(v)) }, "")
  units <- vapply(seq_along(raw), function(j) { v <- cell(hdr_row + 1, j); if (is.na(v)) "" else trimws(as.character(v)) }, "")

  need <- c("Phase", "River", "Date", "ID")
  if (!all(need %in% hdr[1:4])) stop("unexpected first columns in tab ", sheet)
  rows <- (hdr_row + 2):nrow(raw)
  getcol <- function(j) lapply(raw[[j]][rows], function(v) if (length(v)) v[[1]] else NA)
  as_num <- function(lst) suppressWarnings(as.numeric(vapply(lst, function(v) if (is.null(v) || is.na(v)) NA_character_ else as.character(v), "")))
  as_chr <- function(lst) .cell_chr(vapply(lst, function(v) if (is.null(v) || is.na(v)) NA_character_ else as.character(v), ""))
  as_dt  <- function(lst) as.IDate(vapply(lst, function(v) {
    if (is.null(v) || (length(v) == 1 && is.na(v))) return(NA_character_)
    if (inherits(v, c("POSIXct", "Date"))) return(format(as.Date(v)))
    if (is.numeric(v)) return(format(as.Date(v, origin = "1899-12-30")))
    format(as.Date(substr(as.character(v), 1, 10)))
  }, ""))

  dates <- as_dt(getcol(3))
  ok <- !is.na(dates)
  if (!any(ok)) stop("no dated rows in tab ", sheet)
  meta <- data.table(phase = as_chr(getcol(1))[ok], river_raw = as_chr(getcol(2))[ok],
                     date = dates[ok], kit = as_chr(getcol(4))[ok])

  # value/flag column pairs: a header followed by the same header (flag column)
  out <- list(); depth1 <- depth2 <- NULL; q_sample <- NULL; unit_of <- character()
  j <- 5
  while (j < length(hdr)) {
    name <- hdr[j]
    if (!nzchar(name)) { j <- j + 1; next }
    v <- as_num(getcol(j))[ok]
    f <- as_chr(getcol(j + 1))[ok]
    if (name == "Discharge") q_sample <- v
    else if (name == "Sample depth 1") depth1 <- v
    else if (name == "Sample depth 2") depth2 <- v
    else {
      base <- sub("-(one|two)$", "", name)
      sample <- if (grepl("-one$", name)) "depth 1" else if (grepl("-two$", name)) "depth 2" else "surface"
      if (!(base %in% names(unit_of)) && nzchar(units[j]) && units[j] != "flag") unit_of[base] <- units[j]
      out[[length(out) + 1]] <- data.table(i = seq_len(sum(ok)), parameter = base, sample = sample,
                                           value = v, flag = f)
    }
    j <- j + 2
  }
  long <- rbindlist(out)
  long[, `:=`(phase = meta$phase[i], river = sheet, date = meta$date[i], kit = meta$kit[i],
              q_sample = if (is.null(q_sample)) NA_real_ else q_sample[i],
              depth_m = fifelse(sample == "depth 1", if (is.null(depth1)) NA_real_ else depth1[i],
                        fifelse(sample == "depth 2", if (is.null(depth2)) NA_real_ else depth2[i], NA_real_)))]
  # depth samples that were never taken are not rows
  long <- long[!(sample != "surface" & is.na(value) & (is.na(flag) | flag %in% c("NC", "NA")))]
  long[, i := NULL]
  list(values = long, units = unit_of, samples = meta[, river := sheet])
}

parse_wq_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  missing <- setdiff(WQ_RIVERS, sheets)
  if (length(missing)) stop("missing river tabs: ", paste(missing, collapse = ", "))

  parts <- lapply(WQ_RIVERS, function(s) parse_wq_sheet(path, s))
  values <- rbindlist(lapply(parts, `[[`, "values"), use.names = TRUE)
  units <- unlist(lapply(parts, `[[`, "units"))
  units <- units[!duplicated(names(units))]

  # Clean up flags: blank flag with no value = not available yet; blank flag with
  # a value = provisional (not yet flagged)
  values[is.na(flag) & is.na(value), flag := "NA"]
  values[is.na(flag) & !is.na(value), flag := "PV"]
  values[, flag := toupper(flag)]
  values[flag %in% c("NC", "NA", "DV"), value := NA_real_]
  values[river == "Ob", river := "Ob'"]

  # Units and descriptions
  desc <- tryCatch({
    u <- as.data.table(suppressMessages(readxl::read_excel(path, sheet = "Units")))
    setnames(u, tolower(names(u)))
    setNames(as.character(u$description), as.character(u$parameter))
  }, error = function(e) character())
  unit_fix <- c("DOC-fm" = "fraction modern", "POC-fm" = "fraction modern",
                "A254" = "absorbance", "A375" = "absorbance", "A412" = "absorbance",
                "A420" = "absorbance", "A440" = "absorbance", "UTHg" = "ng/L", "UMeHg" = "ng/L",
                "Alkalinity" = "mg/L as CaCO3")
  all_params <- unique(values$parameter)
  grp <- rep(NA_character_, length(all_params))
  for (g in names(WQ_GROUPS)) grp[all_params %in% WQ_GROUPS[[g]]] <- g
  grp[is.na(grp)] <- "Other"
  params <- data.table(parameter = all_params,
                       unit = ifelse(all_params %in% names(unit_fix), unit_fix[all_params],
                                     ifelse(all_params %in% names(units), units[all_params], "")),
                       description = ifelse(all_params %in% names(desc), desc[all_params], ""),
                       group = grp, has_depth = all_params %in% WQ_DEPTH_PARAMS)
  params[is.na(unit), unit := ""]
  params[, unit := pretty_unit(unit)]
  params[, order := match(parameter, unlist(WQ_GROUPS))]
  setorder(params, order, na.last = TRUE)
  params[, order := NULL]

  flags <- tryCatch({
    f <- as.data.table(suppressMessages(readxl::read_excel(path, sheet = "Flag codes")))
    setnames(f, c("flag", "label", "description")[seq_len(ncol(f))])
    f[!is.na(flag)]
  }, error = function(e) data.table(flag = names(WQ_FLAG_LABELS), label = WQ_FLAG_LABELS,
                                    description = ""))

  # Version: "VERSION 20260601" in the title rows of the first tab
  top <- suppressMessages(readxl::read_excel(path, sheet = WQ_RIVERS[1], col_names = FALSE,
                                             range = "A1:A6", col_types = "text"))
  ver <- sub(".*VERSION\\s*([0-9]{8}).*", "\\1", grep("VERSION", toupper(top[[1]]), value = TRUE)[1])
  if (is.na(ver)) ver <- ""

  setcolorder(values, c("river", "phase", "date", "kit", "sample", "depth_m", "q_sample",
                        "parameter", "value", "flag"))
  setkey(values, river, parameter, date)
  samples <- unique(values[, .(river, date, kit)])
  list(values = values, params = params, flags = flags,
       info = list(version = ver, loaded_at = Sys.time(),
                   n_samples = nrow(samples),
                   last_sample = max(values[!is.na(value), date])))
}

# Units as shown in the app: "ug/L as N" -> "\u00b5g/L as N", "per mil" -> "\u2030", ...
pretty_unit <- function(u) {
  u <- gsub("\\bug/", "\u00b5g/", u)
  u <- gsub("\\bng/l\\b", "ng/L", u)
  u <- gsub("^per mil$", "\u2030", u)
  u <- gsub("^deg C$", "\u00b0C", u)
  u <- gsub("^uS/cm$", "\u00b5S/cm", u)
  u <- gsub("^std units$", "pH units", u)
  u <- gsub("^mg CaCO3/L$", "mg/L as CaCO3", u)
  u
}

# Checks on every download: a failed check keeps the previous copy
validate_wq <- function(wq) {
  v <- wq$values
  problems <- character()
  if (!all(c("Ob'", WQ_RIVERS[-1]) %in% v$river)) problems <- c(problems, "a river tab is empty")
  if (!"DOC" %in% v$parameter) problems <- c(problems, "DOC column not found")
  if (nrow(v) < 1000) problems <- c(problems, "far fewer values than expected")
  if (any(v$date < as.IDate("1990-01-01") | v$date > Sys.Date() + 1, na.rm = TRUE))
    problems <- c(problems, "dates outside 1990-today")
  known <- c("AV", "PV", "CAU", "CV", "DV", "BD", "NC", "NA")
  odd <- setdiff(unique(v$flag), known)
  if (length(odd)) message("Water quality: unknown flag codes kept as-is: ", paste(odd, collapse = ", "))
  if (length(problems)) stop(paste(problems, collapse = "; "))
  invisible(TRUE)
}

# ---- download from Google ---------------------------------------------------------

download_wq <- function(url = WQ_SHEET_URL) {
  f <- tempfile(fileext = ".xlsx")
  resp <- httr2::request(url) |>
    httr2::req_user_agent("ArcticGRO Data Explorer") |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform(path = f)
  type <- httr2::resp_content_type(resp)
  if (grepl("html", type %||% ""))
    stop("Google returned a web page instead of the spreadsheet: check that the sheet is ",
         "published to the web or shared as 'Anyone with the link can view'")
  wq <- parse_wq_workbook(f)
  validate_wq(wq)
  wq$info$source <- "ArcticGRO water quality spreadsheet (Google Sheet)"
  wq
}

load_wq_fallback <- function() {
  wq <- readRDS(WQ_FALLBACK_FILE)
  wq$info$source <- "bundled copy (Google Sheet could not be reached)"
  wq
}

get_wq <- function(force = FALSE) {
  now <- Sys.time()
  fresh <- !is.null(.wq$data) && !is.null(.wq$checked) &&
    difftime(now, .wq$checked, units = "hours") < WQ_CHECK_INTERVAL_HOURS
  if (fresh && !force) return(.wq$data)
  .wq$checked <- now
  wq <- NULL
  if (!OFFLINE && nzchar(WQ_SHEET_URL)) {
    wq <- tryCatch(download_wq(), error = function(e) {
      message("Water quality sheet unavailable: ", conditionMessage(e))
      NULL
    })
  }
  if (is.null(wq)) wq <- .wq$data %||% load_wq_fallback()
  .wq$data <- wq
  wq
}

# Season of a sample (used for colours): ArcticGRO seasons
#   spring = April-June, summer = July-September, winter = October-March
wq_season <- function(date) {
  m <- month(date)
  factor(fifelse(m %in% 4:6, "Spring (Apr\u2013Jun)",
         fifelse(m %in% 7:9, "Summer (Jul\u2013Sep)", "Winter (Oct\u2013Mar)")),
         levels = c("Winter (Oct\u2013Mar)", "Spring (Apr\u2013Jun)", "Summer (Jul\u2013Sep)"))
}

# Discharge for each sample, looked up in the ArcticGRO discharge dataset (gauge
# per river: WQ_STATION in config.R), so chemistry and discharge always match.
# For rivers where the sampling site is not at the gauge, the gauge value is taken
# on the day the sampled water passes the gauge (travel time WQ_TRAVEL_DAYS,
# following Holmes et al. 2011). Where the discharge dataset has no value for that
# day, the discharge recorded with the sample in the spreadsheet is kept.
# Adds: q_date (gauge date used), q_source (description, shown in hover and downloads)
wq_attach_discharge <- function(values, daily) {
  v <- copy(values)
  v[, `:=`(station_id = unname(WQ_STATION[river]),
           lag = unname(WQ_TRAVEL_DAYS[river]))]
  v[is.na(lag), lag := 0L]
  v[, q_date := date + lag]
  q <- daily[station_id %in% unique(v$station_id) & !is.na(q), .(station_id, q_date = date, q_ds = q)]
  v <- q[v, on = .(station_id, q_date)]
  gauge <- unname(WQ_GAUGE_NAME[v$river])
  lag_txt <- fifelse(v$lag == 0, "same day",
             fifelse(v$lag > 0, sprintf("%d days after sampling (travel time)", v$lag),
                                 sprintf("%d day%s before sampling (travel time)", -v$lag,
                                         fifelse(v$lag == -1, "", "s"))))
  v[, q_source := fifelse(!is.na(q_ds), sprintf("discharge dataset: %s gauge, %s", gauge, lag_txt),
                  fifelse(!is.na(q_sample), "recorded with the sample (no value in discharge dataset)",
                          NA_character_))]
  v[!is.na(q_ds), q_sample := q_ds]
  v[is.na(q_ds), q_date := date]
  v[, c("q_ds", "station_id", "lag") := NULL]
  setcolorder(v, c(names(values), "q_date", "q_source"))
  setkey(v, river, parameter, date)
  v[]
}

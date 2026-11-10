# data_zenodo.R - load the discharge data from the latest Zenodo version.
#
# get_dataset() returns a list:
#   daily    data.table: station_id, date, q (m3/s), flag (A/P/NA)
#   stations data.table: one row per station (from stations.csv)
#   info     list: version, doi, published, record_id, source, data_through
#
# Behaviour:
#   - checks Zenodo at most every CHECK_INTERVAL_HOURS
#   - downloads files only when a new version has been published
#   - if Zenodo is unreachable, keeps the data already loaded, or falls back to
#     the bundled copy in data-fallback/

library(data.table)
library(httr2)

.store <- new.env()

# ---- reading the files ---------------------------------------------------------

read_station_csv <- function(path, station_id) {
  d <- fread(path, na.strings = c("NA", ""), encoding = "UTF-8",
             colClasses = list(character = c("station_code", "flag")))
  data.table(station_id = station_id,
             date = as.IDate(d$date),
             q    = as.numeric(d$discharge_m3s),
             flag = d$flag)
}

# dir: folder with stations.csv and the station CSVs (directly or in data/)
build_dataset <- function(dir, info) {
  st <- fread(file.path(dir, "stations.csv"), na.strings = c("NA", ""),
              encoding = "UTF-8", colClasses = list(character = "station_code"))
  st[, station_id := sub("\\.csv$", "", file_name)]
  st[, label := sprintf("%s at %s", river, station_name)]
  data_dir <- if (file.exists(file.path(dir, st$file_name[1]))) dir else file.path(dir, "data")
  daily <- rbindlist(lapply(seq_len(nrow(st)), function(i)
    read_station_csv(file.path(data_dir, st$file_name[i]), st$station_id[i])))
  daily[is.na(q), flag := NA_character_]
  setkey(daily, station_id, date)
  info$data_through <- max(daily[!is.na(q), date])
  info$loaded_at <- Sys.time()
  list(daily = daily, stations = st, info = info)
}

# ---- Zenodo API ----------------------------------------------------------------

zenodo_request <- function(path, timeout = 15) {
  request(paste0(ZENODO_BASE, path)) |>
    req_user_agent("ArcticGRO Discharge Explorer") |>
    req_timeout(timeout) |>
    req_retry(max_tries = 2)
}

zenodo_file_keys <- function(rec) {
  f <- rec$files
  e <- f$entries %||% f
  if (is.list(e) && length(e)) {
    if (!is.null(e[[1]]$key)) return(vapply(e, function(x) x$key, ""))
    if (!is.null(names(e)) && all(nzchar(names(e)))) return(names(e))
  }
  ff <- zenodo_request(sprintf("/api/records/%s/files", rec$id)) |>
    req_perform() |> resp_body_json()
  vapply(ff$entries, function(x) x$key, "")
}

zenodo_latest <- function() {
  rec <- zenodo_request(sprintf("/api/records/%s/versions/latest", ZENODO_RECORD_ID)) |>
    req_perform() |> resp_body_json()
  list(record_id = as.character(rec$id),
       version   = rec$metadata$version %||% "",
       doi       = rec$doi %||% rec$pids$doi$identifier %||% "",
       published = rec$metadata$publication_date %||% "",
       keys      = zenodo_file_keys(rec))
}

download_record <- function(latest, dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  keys <- latest$keys[grepl("\\.csv$", latest$keys)]
  if (!"stations.csv" %in% keys) stop("stations.csv not found in Zenodo record")
  reqs <- lapply(keys, function(k)
    zenodo_request(sprintf("/api/records/%s/files/%s/content", latest$record_id,
                           utils::URLencode(k, reserved = TRUE)), timeout = 120))
  paths <- file.path(dest, keys)
  if (exists("req_perform_parallel", asNamespace("httr2"))) {
    # all files at once instead of one after another
    resps <- req_perform_parallel(reqs, paths = paths, on_error = "stop")
  } else {
    for (i in seq_along(reqs)) req_perform(reqs[[i]], path = paths[i])
  }
  dest
}

# Downloaded versions are kept between R sessions, so each Zenodo version is
# downloaded only once per computer/server. Override with DISCHARGE_CACHE_DIR.
cache_dir <- function() {
  for (d in c(Sys.getenv("DISCHARGE_CACHE_DIR"),
              tools::R_user_dir("arcticgro-discharge", "cache"),
              file.path(tempdir(), "arcticgro-cache"))) {
    if (!nzchar(d)) next
    ok <- dir.create(d, recursive = TRUE, showWarnings = FALSE) || dir.exists(d)
    if (ok && file.access(d, 2) == 0) return(d)
  }
  tempdir()
}

# version labels like "v20261104" and "20261104" are the same version
same_version <- function(a, b) {
  norm <- function(x) sub("^v", "", tolower(trimws(x %||% "")))
  nzchar(norm(a)) && identical(norm(a), norm(b))
}

# The bundled copy in data-fallback/ (read once per R session)
bundled_data <- function() {
  if (is.null(.store$bundled)) .store$bundled <- readRDS(FALLBACK_FILE)
  .store$bundled
}

load_from_zenodo <- function() {
  latest <- zenodo_latest()
  current <- .store$data
  if (!is.null(current) && identical(current$info$record_id, latest$record_id))
    return(current)
  # Bundled copy is already this version: no download needed
  b <- tryCatch(bundled_data(), error = function(e) NULL)
  if (!is.null(b) && (identical(b$info$record_id, latest$record_id) ||
                      same_version(b$info$version, latest$version))) {
    b$info[c("version", "doi", "published", "record_id")] <-
      latest[c("version", "doi", "published", "record_id")]
    b$info$source <- "Zenodo"
    return(b)
  }
  rds <- file.path(cache_dir(), sprintf("record_%s.rds", latest$record_id))
  if (file.exists(rds)) return(readRDS(rds))
  message("Downloading Zenodo record ", latest$record_id, " (", latest$version, ")")
  dir <- download_record(latest, file.path(cache_dir(), latest$record_id))
  ds <- build_dataset(dir, list(version = latest$version, doi = latest$doi,
                                published = latest$published,
                                record_id = latest$record_id, source = "Zenodo"))
  saveRDS(ds, rds)
  ds
}

load_fallback <- function() {
  ds <- bundled_data()
  ds$info$source <- "bundled copy (Zenodo could not be reached)"
  ds
}

# ---- main entry point -----------------------------------------------------------

get_dataset <- function(force = FALSE) {
  now <- Sys.time()
  fresh <- !is.null(.store$data) && !is.null(.store$checked) &&
    difftime(now, .store$checked, units = "hours") < CHECK_INTERVAL_HOURS
  if (fresh && !force) return(.store$data)
  .store$checked <- now

  ds <- NULL
  if (!OFFLINE) {
    ds <- tryCatch(load_from_zenodo(), error = function(e) {
      message("Zenodo unavailable: ", conditionMessage(e))
      NULL
    })
  }
  if (is.null(ds)) ds <- .store$data %||% load_fallback()
  .store$data <- ds
  ds
}

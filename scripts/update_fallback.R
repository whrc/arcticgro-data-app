# update_fallback.R - refresh the bundled copy of the data in data-fallback/.
#
# The app normally reads the latest version from Zenodo. The bundled copy is
# only used if Zenodo cannot be reached, so it does not need updating for every
# release - but refreshing it now and then keeps the fallback recent.
#
# Usage (from the app folder):
#   Rscript scripts/update_fallback.R                          # from Zenodo (latest version)
#   Rscript scripts/update_fallback.R ../arcticgro-discharge   # from a local copy of the data repository

source("R/config.R")
source("R/data_zenodo.R")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  dir <- normalizePath(args[1])
  cat("Building fallback from local folder:", dir, "\n")
  version <- tryCatch({
    readme <- readLines(file.path(dir, "README.md"), warn = FALSE)
    sub(".*\\*\\*Version:\\*\\*\\s*([^ ]+).*", "\\1", grep("\\*\\*Version:\\*\\*", readme, value = TRUE)[1])
  }, error = function(e) "local")
  ds <- build_dataset(dir, list(version = version, doi = "", published = "",
                                record_id = "local", source = "local copy"))
} else {
  cat("Downloading latest version from Zenodo...\n")
  ds <- load_from_zenodo()
}

dir.create(dirname(FALLBACK_FILE), showWarnings = FALSE)
saveRDS(ds, FALLBACK_FILE, compress = "xz")
cat(sprintf("Saved %s: %d stations, %s daily rows, version %s, data through %s (%.1f MB)\n",
            FALLBACK_FILE, nrow(ds$stations), format(nrow(ds$daily), big.mark = ","),
            ds$info$version, ds$info$data_through, file.size(FALLBACK_FILE) / 1e6))

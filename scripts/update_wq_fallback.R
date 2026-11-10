# update_wq_fallback.R - refresh the bundled copy of the water quality data
# (data-fallback/arcticgro_water_quality.rds).
#
# The app normally reads the master Google Sheet. The bundled copy is only used
# if Google can't be reached, so refresh it now and then (e.g. with each new
# version of the sheet).
#
# Usage (from the app folder):
#   Rscript scripts/update_wq_fallback.R                       # download from the Google Sheet
#   Rscript scripts/update_wq_fallback.R path/to/file.xlsx     # from a downloaded copy

source("R/config.R")
source("R/data_wq.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  cat("Reading", args[1], "\n")
  wq <- parse_wq_workbook(args[1])
  validate_wq(wq)
} else {
  cat("Downloading the Google Sheet...\n")
  wq <- download_wq()
}
wq$info$source <- "bundled copy"
dir.create(dirname(WQ_FALLBACK_FILE), showWarnings = FALSE)
saveRDS(wq, WQ_FALLBACK_FILE, compress = "xz")
cat(sprintf("Saved %s: version %s, %d samples, %s values, latest results %s\n",
            WQ_FALLBACK_FILE, wq$info$version, wq$info$n_samples,
            format(nrow(wq$values), big.mark = ","), wq$info$last_sample))

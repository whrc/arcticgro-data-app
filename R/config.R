# config.R - settings for the ArcticGRO Discharge Explorer.
# Files in R/ are loaded automatically by Shiny before app.R runs.

APP_TITLE <- "ArcticGRO Data Explorer"

# Zenodo record. Any published version id works: the app always looks up the
# newest version of the record. Override with environment variables if needed.
ZENODO_BASE      <- Sys.getenv("ZENODO_BASE_URL", "https://zenodo.org")
ZENODO_RECORD_ID <- Sys.getenv("ZENODO_RECORD_ID", "23957361")
CONCEPT_DOI      <- "10.5281/zenodo.23957360"   # "cite all versions" DOI
DATA_REPO_URL    <- "https://github.com/whrc/arcticgro-discharge"

# How often (hours) a running app checks Zenodo for a new version.
CHECK_INTERVAL_HOURS <- 6

# Set DISCHARGE_OFFLINE=true to skip Zenodo and use the bundled copy
# (useful when developing without internet).
OFFLINE <- tolower(Sys.getenv("DISCHARGE_OFFLINE", "false")) == "true"

# Bundled copy of the data, used if Zenodo cannot be reached.
FALLBACK_FILE <- "data-fallback/arcticgro_discharge.rds"

# ---- Water quality (chemistry) ---------------------------------------------------
# Master Google Sheet, downloaded as an Excel file. Either
#  - share the sheet as "Anyone with the link can view" and keep this export link, or
#  - File > Share > Publish to web > Entire document > Microsoft Excel (.xlsx), and
#    put that link here (or in the WQ_SHEET_URL environment variable on Connect);
#    the editable sheet then stays private.
WQ_SHEET_ID  <- "1zPVbl0sbqt-c5UJPbHqzV143LG72b5AZKAbay4Alv6M"
WQ_SHEET_URL <- Sys.getenv("WQ_SHEET_URL",
  sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=xlsx", WQ_SHEET_ID))
WQ_SHEET_VIEW_URL <- sprintf("https://docs.google.com/spreadsheets/d/%s/edit?gid=0#gid=0", WQ_SHEET_ID)
WQ_CHECK_INTERVAL_HOURS <- 1            # how often a running app re-reads the sheet
WQ_FALLBACK_FILE <- "data-fallback/arcticgro_water_quality.rds"
WQ_DATA_PAGE <- "https://www.arcticgreatrivers.org/data"
WQ_METADATA_URL <- "https://drive.google.com/file/d/1C4uWkAxr6KRaInMTsGL1CQSApFOSDz4B/view"
WQ_DEFAULT_PARAM <- "DOC"
WQ_DEFAULT_FLAGS <- c("AV", "PV", "CAU", "CV", "BD")

# Daily discharge station used with each chemistry river
WQ_STATION <- c("Ob'" = "Ob_Salekhard", "Yenisey" = "Yenisey_Igarka", "Lena" = "Lena_Kyusyur",
                "Kolyma" = "Kolyma_Kolymskoe", "Yukon" = "Yukon_Pilot_Station",
                "Mackenzie" = "Mackenzie_Arctic_Red_River")

# Travel time of water between the chemistry sampling site and the discharge gauge
# (Holmes et al. 2011, Estuaries and Coasts, doi:10.1007/s12237-011-9386-6):
# distance between the two sites / an assumed river velocity of 1.5 m/s, rounded
# to whole days. Positive = the sampled water reaches the gauge that many days
# later (sampling site upstream of the gauge); negative = it passed the gauge
# earlier (sampling site downstream).
#   Lena:    Zhigansk is ~520 km upstream of Kyusyur      -> 520 km / 1.5 m/s = 4.0 days
#   Yenisey: Dudinka is ~250 km downstream of Igarka       -> 1.9 days
#   Kolyma:  Cherskiy is ~160 km downstream of Kolymskoye  -> 1.2 days
#   Ob', Yukon, Mackenzie: sampled at the gauge            -> 0
WQ_TRAVEL_DAYS <- c("Ob'" = 0L, "Yenisey" = -2L, "Lena" = 4L, "Kolyma" = -1L,
                    "Yukon" = 0L, "Mackenzie" = 0L)
WQ_SAMPLING_SITE <- c("Ob'" = "Salekhard", "Yenisey" = "Dudinka", "Lena" = "Zhigansk",
                      "Kolyma" = "Cherskiy", "Yukon" = "Pilot Station",
                      "Mackenzie" = "Tsiigehtchic")
WQ_GAUGE_NAME <- c("Ob'" = "Salekhard", "Yenisey" = "Igarka", "Lena" = "Kyusyur",
                   "Kolyma" = "Kolymskoye", "Yukon" = "Pilot Station",
                   "Mackenzie" = "Arctic Red River (Tsiigehtchic)")
HOLMES_2011 <- paste0("Holmes, R.M., et al. 2011. Seasonal and annual fluxes of nutrients and ",
                      "organic matter from large rivers to the Arctic Ocean and surrounding seas. ",
                      "Estuaries and Coasts. https://doi.org/10.1007/s12237-011-9386-6")

# Season colours (first three slots of the validated palette: safe on scatter plots)
SEASON_COL <- c("#2a78d6", "#eb6834", "#1baf7a")

# Statistics defaults
DEFAULT_REF_PERIOD <- c(1991L, 2020L)  # reference period for percentiles/medians
MIN_COMPLETENESS   <- 0.9              # share of days needed for a monthly/annual mean
MIN_REF_YEARS      <- 5                # years needed to show a percentile band
MAX_YEARS_SHOWN    <- 4                # years that can be overlaid on one plot

# Website style (matches arcticgreatrivers.org)
SITE <- list(
  navy   = "#0f3557",   # header / hero background
  blue   = "#1e6aa8",   # buttons, links, headline numbers
  sky    = "#41a6d9",   # accent on dark backgrounds
  text   = "#10202e",
  text_2 = "#3d4a57",
  border = "#e3e8ed",
  tint   = "#f3f7fa"    # light panels (sidebar)
)
FONT_BODY     <- "Inter"
FONT_HEADINGS <- "Montserrat"
GOOGLE_FONTS_URL <- paste0("https://fonts.googleapis.com/css2?family=Montserrat:wght@500;600;700",
                           "&family=Inter:wght@400;500;600&display=swap")
PLOT_FONT <- "Inter, Helvetica Neue, Arial, sans-serif"

# Colours (validated categorical palette; years keep their colour while selected)
COL <- list(
  series    = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100"),
  band_max  = "#eef2f5",   # min-max range
  band_p10  = "#dfe6ec",   # 10th-90th percentile
  band_iqr  = "#cdd7e0",   # 25th-75th percentile
  median    = "#4d5b68",
  text      = SITE$text,
  text_2    = SITE$text_2,
  grid      = "#e7ecf0",
  surface   = "#ffffff",
  station   = "#2a78d6",
  selected  = "#eb6834"
)

# Station map: ArcticGRO basin map (www/arcticgro_basins_map.jpg), a north-polar
# stereographic projection with 0 deg longitude pointing down and circles at
# 75, 60 and 45 deg N. These numbers convert latitude/longitude to image pixels;
# they only need changing if the map image is replaced.
MAP_IMAGE <- "arcticgro_basins_map.jpg"
MAP_SIZE  <- 1560          # image width = height (px)
MAP_CX    <- 780           # pixel position of the North Pole
MAP_CY    <- 780
MAP_K     <- 1858          # radius (px) = MAP_K * tan(colatitude / 2)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

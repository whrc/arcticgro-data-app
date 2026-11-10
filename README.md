# ArcticGRO Data Explorer

Interactive web app (R Shiny) for exploring and downloading Arctic Great Rivers
Observatory (ArcticGRO) data. Two sections:

- **Discharge**: daily river discharge from the
  [ArcticGRO Discharge Dataset](https://doi.org/10.5281/zenodo.23957360) on Zenodo.
  The app always shows the **latest Zenodo version**: it checks Zenodo when it starts
  and every 6 hours while it runs, and downloads new files only when a new version
  has been published. No redeploy is needed after a data release.
- **Water chemistry**: ArcticGRO water quality samples from the six largest Arctic
  rivers, read from the team's master Google Sheet. The app re-reads the sheet every
  hour, so new results appear without redeploying.

Open a section directly by adding `?section=discharge` or `?section=chemistry` to the
app's address (also `map`, `about`).

## What's in the app

**Discharge**

| Tab | Content |
|---|---|
| Daily hydrograph | Selected years (up to 4) against the reference-period range, 10th–90th and 25th–75th percentiles and median; calendar or water year; log scale; table view |
| Monthly & annual | Monthly means against the reference period; annual mean, volume, peak, minimum and peak timing over the whole record with trend; annual summary table |
| Data & download | Filter stations, dates, time step (daily, monthly, annual) and flags; download CSV |

**Water chemistry** (left panel: rivers, parameter, years, surface/depth samples, data-quality flags, log scale)

| Tab | Content |
|---|---|
| Over time | One river: daily discharge (with sampling days) above the parameter. Several rivers: one panel per river on a shared scale |
| Against discharge | Each sample against discharge on the sampling day, one panel per river |
| Two parameters | Any two parameters against each other, one panel per river |
| Data & download | Table; CSV of one or all parameters, long (one row per value) or wide (one row per sample, like the spreadsheet) |

Discharge for each sample comes from the discharge dataset. For the Lena, Yenisey and
Kolyma, where samples are taken away from the gauge, the gauge value is shifted by the
travel time of water between the two sites (`WQ_TRAVEL_DAYS` in `R/config.R`;
Holmes et al. 2011, https://doi.org/10.1007/s12237-011-9386-6).

Points are coloured by season (spring Apr–Jun, summer Jul–Sep, winter Oct–Mar). Marker shape shows data quality: filled circle accepted (AV, CV), open
circle provisional (PV), diamond caution (CAU), square depth sample, open triangle
below detection (BD, drawn at the bottom of the axis). DV, NC and NA values are not
shown.

**Also:** Map (ArcticGRO basin map with clickable gauging stations) and About
(data sources, flags, methods, citations).

## Files

```
app.R                      layout (UI) and app logic (server)
R/config.R                 settings: Zenodo record, Google Sheet link, colours, defaults
R/data_zenodo.R            loads the latest Zenodo version, caches it, falls back if offline
R/stats.R                  percentiles, monthly and annual statistics, download tables
R/plots.R                  plotly charts
R/data_wq.R                reads the water quality Google Sheet, checks it, falls back if offline
R/plots_wq.R               water chemistry charts
data-fallback/             bundled copies of both datasets, used only if Zenodo or Google can't be reached
scripts/update_fallback.R      refreshes the bundled discharge copy
scripts/update_wq_fallback.R   refreshes the bundled water quality copy
www/arcticgro_basins_map.jpg  ArcticGRO basin map used on the Map tab
www/site.css               website look: navy header, Montserrat/Inter fonts, ArcticGRO blue
www/embed.js               embed mode (?embed=1), auto-height in an iframe, loading message
```

The Map tab places the stations on the ArcticGRO basin map. The image is a
north-polar stereographic projection; the settings that convert latitude and
longitude to positions on the image are `MAP_*` in `R/config.R`. If you replace
the image with a different map, these need recalculating.

Settings you may want to change are all in `R/config.R` (reference period,
completeness threshold, check interval, colours). The website colours are
`SITE` in `R/config.R` (used by the plots) and the same values in `www/site.css`
(used by the page); change both together.

## Run it in Positron

1. Open this folder in Positron (File > Open Folder).
2. Install the packages once, in the R console:

   ```r
   install.packages(c("shiny", "bslib", "plotly", "DT", "data.table", "httr2",
                      "readxl", "jsonlite", "rsconnect"))
   ```

3. Open `app.R` and click **Run App** (top right of the editor), or run
   `shiny::runApp()` in the console.

The sidebar shows which data version is loaded and whether it came from Zenodo
or the bundled copy. To work offline: `Sys.setenv(DISCHARGE_OFFLINE = "true")`
before running.

## Publish on Posit Connect Cloud

One-time setup:

1. Create a public GitHub repository (e.g. `whrc/arcticgro-data-app`) and push
   this folder to it, the same way as the data repository.
2. In the R console, from this folder, create the dependency file Connect Cloud
   needs, then commit and push it:

   ```r
   rsconnect::writeManifest()
   ```

   Run this again whenever you add or update an R package.
3. Sign in at https://connect.posit.cloud > **Publish** > **Shiny** > choose the
   repository and branch `main` > primary file `app.R` > keep **Automatically
   publish on push** on > **Publish**.

After that, every push to GitHub updates the app. Data updates need nothing:
publishing a new Zenodo version is enough.

You can also publish directly from Positron with the Posit Publisher extension.

## Publish on Posit Connect

1. In Positron, publish with the Posit Publisher extension (or run
   `rsconnect::deployApp()` in the console) and choose your Connect server.
2. In the app's settings on Connect:
   - **Access**: "Anyone – no login required", so website visitors don't get a login box.
   - **Access > Custom URL**: e.g. `/arcticgro-discharge/`, so the address stays
     the same after redeploying.
   - **Runtime > Min processes**: 1, so the app stays warm and visitors don't
     wait for it to start (check with your Connect admin that this is fine).
3. Use the app's own address (**Open Solo**, or the custom URL) in the website,
   not the address of the Connect dashboard page.

If the frame on the website stays blank or says the server refused to connect,
ask the Connect admin whether framing of content is blocked
(`FrameOptionsContent` setting).

## Connect the water quality Google Sheet

The app downloads the master sheet as an Excel file. Choose one of:

1. **Share by link (simplest):** in the sheet, **Share > General access > Anyone with
   the link > Viewer**. The app uses the export link built from the sheet ID in
   `R/config.R` (`WQ_SHEET_ID`); nothing else to change. Anyone with the link can
   then view the sheet.
2. **Publish to web (keeps the sheet private):** **File > Share > Publish to web >
   Entire document > Microsoft Excel (.xlsx) > Publish**, copy the link, and either
   put it in `WQ_SHEET_URL` in `R/config.R` or set the environment variable
   `WQ_SHEET_URL` in the app's settings on Posit Connect (**Vars**). Published copies
   update a few minutes after edits.

To check it works, run the app in Positron: the chemistry sidebar should say
**Source: ArcticGRO water quality spreadsheet (Google Sheet)**. If it says "bundled copy", the R console
shows why (for example, the sheet isn't shared or published).

The app checks every download: the six river tabs, the header row, dates, and the
DOC column. If a check fails (for example, a column is renamed by mistake), it keeps
showing the last good copy, so a typo in the sheet can't break the app. Keep the
layout of the sheet the same: one tab per river named Ob, Yenisey, Lena, Kolyma,
Yukon and Mackenzie; header row starting with Phase, River, Date, ID; each parameter
as a value column followed by its flag column. New parameters added as value/flag
column pairs appear in the app automatically (under "Other" until they are added to
a group in `R/data_wq.R`).

## Embed it in the ArcticGRO website

Paste this into a Beaver Builder **HTML** module in a full-width row, and replace
`https://YOUR-APP-ADDRESS/` with the app's address (keep `?embed=1`). Use
`?embed=1&section=discharge` on the Discharge page and `?embed=1&section=chemistry`
on the Water Quality page:

```html
<div id="arcticgro-discharge-wrap" style="position:relative; min-height:600px;">
  <div id="arcticgro-discharge-loading" style="position:absolute; inset:0; display:flex; justify-content:center; padding-top:160px; font-family:Montserrat,Arial,sans-serif; font-weight:600; color:#0f3557;">
    Loading the ArcticGRO data explorer&hellip; this can take up to 30 seconds.
  </div>
  <iframe id="arcticgro-discharge" src="https://YOUR-APP-ADDRESS/?embed=1&section=discharge"
          title="ArcticGRO Data Explorer" scrolling="no"
          style="position:relative; width:100%; height:1100px; border:0; display:block;"
          onload="document.getElementById('arcticgro-discharge-loading').style.display='none'"></iframe>
</div>
<script>
(function () {
  var frame = document.getElementById('arcticgro-discharge');
  var appOrigin = new URL(frame.src).origin;
  window.addEventListener('message', function (e) {
    if (e.origin !== appOrigin || !e.data) return;
    if (e.data.type === 'arcticgro-discharge-height') frame.style.height = e.data.height + 'px';
    if (e.data.type === 'arcticgro-discharge-top') {
      var top = frame.getBoundingClientRect().top;
      if (top < 0) window.scrollTo({ top: top + window.scrollY - 20, behavior: 'smooth' });
    }
  });
})();
</script>
```

What it does:

- `?embed=1` hides the app's title bar text (the website has its own header);
  the tabs stay.
- The app tells the page its height whenever it changes, so the frame always fits
  the current tab and there is no second scrollbar. When a visitor switches tabs
  further down the page, the page scrolls back to the top of the app.
- The loading message shows while the app starts and disappears when it loads.
- Inside the website, the full-screen buttons on cards are hidden (they don't
  work well inside a frame). Opened on its own, the app is unchanged.

## Refresh the bundled copies (occasionally)

```r
# discharge, from Zenodo
source("scripts/update_fallback.R")
# or from a local copy of the data repository
system("Rscript scripts/update_fallback.R ../arcticgro-discharge")

# water quality, from the Google Sheet
source("scripts/update_wq_fallback.R")
# or from a downloaded Excel copy
system("Rscript scripts/update_wq_fallback.R ~/Downloads/ArcticGRO_Water_Quality_Data.xlsx")
```

Then redeploy so the updated files in `data-fallback/` go to Connect.

## Notes on the statistics

- Percentiles and medians use daily values from the reference period chosen in the
  app (default 1991–2020; the full record for stations with shorter records).
- Monthly and annual means need at least 90% of days with data.
- Annual volume = annual mean discharge × days × 86,400 s, in km³.
- Water years run 1 October – 30 September and are named after the year they end in.
- 29 February is not shown on the day-of-year axis.
- Trend lines are least-squares fits for illustration, not a formal trend test.

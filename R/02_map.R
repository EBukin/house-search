# Compute terrain and planning statistics per harvested parcel and build an
# interactive leaflet map with switchable imagery providers.
#
# Usage: Rscript R/02_map.R [site_id]      (run R/01_harvest.R first)
#
# Output under output/<site_id>/:
#   parcel_stats.csv        one row per parcel: cadastral attributes + stats
#   parcel_stats.geojson    same, with parcel geometry (EPSG:4326)
#   map.html                self-contained interactive map
#
# Terrain statistics use the 5 m LiDAR DTM. Slope and aspect are derived on the
# native 5 m grid (3x3 Horn) and, like elevation, bilinearly resampled to 1 m so
# that small parcels are summarised over their actual footprint.
#   slope_*          degrees
#   aspect_mean_deg  circular mean of aspect over cells with slope >= 2 deg
#   south_facing_pct share of the area facing SE-S-SW (aspect 135-225 deg)
#   southness        mean of -cos(aspect): +1 due south, -1 due north, 0 E/W or flat
#   sun_winter_idx   direct-beam irradiance at winter-solstice noon relative to
#   sun_equinox_idx  flat ground (1 = as flat ground, >1 better, 0 = in shade of slope)

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(leaflet)
  library(leaflet.extras)
  library(htmlwidgets)
  library(htmltools)
})
source("R/utils.R")

site_id <- site_arg()
site <- load_site(site_id)
src <- site_dir(site_id)
out <- output_dir(site_id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cli_h1("Parcel statistics and map for {.val {site_id}}")

# ---- 1. Inputs ---------------------------------------------------------------

parcels <- st_read(file.path(src, "cadastre", "parcels.geojson"), quiet = TRUE) |>
  st_transform(crs_metric) |>
  st_make_valid()
layer_files <- list.files(file.path(src, "cadastre", "layers"), pattern = "\\.geojson$", full.names = TRUE)
layers <- lapply(setNames(layer_files, sub("\\.geojson$", "", basename(layer_files))), function(f) {
  st_read(f, quiet = TRUE) |> st_transform(crs_metric) |> st_make_valid()
})
find_layer <- function(pattern) {
  hit <- grep(pattern, names(layers), value = TRUE)
  if (length(hit)) layers[[hit[1]]] else NULL
}

point <- st_sfc(st_point(c(site$lon, site$lat)), crs = 4326) |> st_transform(crs_metric)
buffer <- st_buffer(point, site$buffer_m)

dtm5 <- rast(file.path(src, "dem", "dtm5.asc"))
crs(dtm5) <- paste0("EPSG:", crs_metric)
names(dtm5) <- "elev"

# ---- 2. Terrain surfaces -----------------------------------------------------

slope5 <- terrain(dtm5, "slope", unit = "degrees", neighbors = 8)
aspect5 <- terrain(dtm5, "aspect", unit = "radians", neighbors = 8)
# Aspect is circular: resample its sine/cosine, not the angle itself.
surf5 <- c(dtm5, slope5, sin(aspect5), cos(aspect5))
names(surf5) <- c("elev", "slope", "asp_sin", "asp_cos")
surf1 <- resample(surf5, rast(ext(surf5), resolution = 1, crs = crs(surf5)), method = "bilinear")

slope_r <- surf1$slope * pi / 180
aspect_r <- atan2(surf1$asp_sin, surf1$asp_cos) %% (2 * pi)

# Relative direct-beam irradiance at solar noon (sun due south, azimuth pi).
sun_index <- function(elevation_deg) {
  z <- (90 - elevation_deg) * pi / 180
  cos_i <- cos(z) * cos(slope_r) + sin(z) * sin(slope_r) * cos(pi - aspect_r)
  ifel(cos_i < 0, 0, cos_i) / cos(z)
}
decl_winter <- -23.44
sun_winter <- sun_index(90 - site$lat + decl_winter)
sun_equinox <- sun_index(90 - site$lat)

cells <- c(surf1$elev, surf1$slope, aspect_r, sun_winter, sun_equinox)
names(cells) <- c("elev", "slope", "aspect", "sun_winter", "sun_equinox")

# ---- 3. Per-parcel statistics ------------------------------------------------

parcels <- parcels |>
  mutate(
    parcel_id = sprintf("F%s-%s", Foglio, trimws(Acad_text)),
    is_target = as.vector(st_intersects(parcels, point, sparse = FALSE)),
    dist_to_point_m = round(as.numeric(st_distance(parcels, point)), 1),
    area_m2 = round(as.numeric(st_area(parcels)), 1),
    perimeter_m = round(as.numeric(st_length(st_cast(parcels, "MULTILINESTRING"))), 1)
  )

circ_mean_deg <- function(a) (atan2(mean(sin(a)), mean(cos(a))) * 180 / pi) %% 360
compass <- function(deg) {
  dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ifelse(is.na(deg), NA, dirs[(round(deg / 45) %% 8) + 1])
}

vals <- terra::extract(cells, vect(parcels), ID = TRUE)
terrain_stats <- vals |>
  filter(!is.na(elev)) |>
  group_by(ID) |>
  summarise(
    dtm_cells_1m = n(),
    elev_min = min(elev), elev_mean = mean(elev), elev_max = max(elev),
    slope_min = min(slope), slope_mean = mean(slope), slope_max = max(slope),
    slope_mean_pct = mean(tan(slope * pi / 180)) * 100,
    aspect_mean_deg = if (any(slope >= 2)) circ_mean_deg(aspect[slope >= 2]) else NA_real_,
    south_facing_pct = 100 * mean(slope >= 2 & aspect >= 3 * pi / 4 & aspect <= 5 * pi / 4),
    southness = mean(ifelse(slope >= 2, -cos(aspect), 0)),
    sun_winter_idx = mean(sun_winter),
    sun_equinox_idx = mean(sun_equinox)
  ) |>
  mutate(aspect_dir = compass(aspect_mean_deg), across(where(is.double), \(x) round(x, 2)))
parcels <- parcels |>
  mutate(ID = row_number()) |>
  left_join(terrain_stats, by = "ID") |>
  select(-ID)

# Planning / landscape layers: dominant class ("Sigla") and its share of the parcel.
overlay_class <- function(parcels, layer, field = "Sigla") {
  if (is.null(layer) || !field %in% names(layer)) return(rep(NA_character_, nrow(parcels)))
  inter <- suppressWarnings(st_intersection(
    select(parcels, parcel_id), select(layer, class = all_of(field))
  ))
  if (nrow(inter) == 0) return(rep(NA_character_, nrow(parcels)))
  inter$a <- as.numeric(st_area(inter))
  best <- inter |>
    st_drop_geometry() |>
    group_by(parcel_id, class) |>
    summarise(a = sum(a), .groups = "drop_last") |>
    slice_max(a, n = 1, with_ties = FALSE) |>
    ungroup()
  pa <- setNames(parcels$area_m2, parcels$parcel_id)
  lbl <- sprintf("%s (%.0f%%)", trimws(best$class), pmin(100, 100 * best$a / pa[best$parcel_id]))
  unname(setNames(lbl, best$parcel_id)[parcels$parcel_id])
}
overlays <- c(
  prg_zoning = "destinazioni_urbanistiche",
  vincolo_idrogeologico = "vincolo_idrogeologico",
  bosco_vincolo_paesaggistico = "foreste_e_boschi",
  copertura_boscata = "coperturaboscata",
  ambito_paesaggio = "ambiti_di_paesaggio",
  unita_paesaggio = "unita_di_paesaggio",
  morfologia_insediativa = "morfologie_insediative"
)
for (nm in names(overlays)) parcels[[nm]] <- overlay_class(parcels, find_layer(overlays[[nm]]))

buildings <- find_layer("edifici_catastali")
if (!is.null(buildings)) {
  b_int <- suppressWarnings(st_intersection(select(parcels, parcel_id), select(buildings, b_label = Acad_text)))
  b_sum <- b_int |>
    mutate(a = as.numeric(st_area(b_int))) |>
    st_drop_geometry() |>
    group_by(parcel_id) |>
    summarise(buildings_n = n(), buildings_m2 = round(sum(a), 1),
              buildings = paste(b_label, collapse = ", "))
  parcels <- left_join(parcels, b_sum, by = "parcel_id") |>
    mutate(buildings_n = coalesce(buildings_n, 0L), buildings_m2 = coalesce(buildings_m2, 0))
}

parcels <- parcels |>
  relocate(parcel_id, is_target, dist_to_point_m, Foglio, particella = Acad_text,
           area_m2, area_cadastral_m2 = Area) |>
  arrange(desc(is_target), dist_to_point_m)

parcels_ll <- st_transform(parcels, 4326)
write.csv(st_drop_geometry(parcels), file.path(out, "parcel_stats.csv"), row.names = FALSE, na = "")
st_write(parcels_ll, file.path(out, "parcel_stats.geojson"), driver = "GeoJSON",
         delete_dsn = TRUE, quiet = TRUE, layer_options = c("COORDINATE_PRECISION=8", "RFC7946=YES"))
cli_alert_success("Statistics for {nrow(parcels)} parcels written to {.path {out}}")

# ---- 4. Map ------------------------------------------------------------------

fmt <- function(x, d = 1) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, big.mark = " "))
popup_html <- function(p) {
  rows <- c(
    "Foglio / particella" = sprintf("%s / %s", p$Foglio, p$particella),
    "Area (geometry)" = paste(fmt(p$area_m2, 0), "m²"),
    "Area (cadastral attribute)" = paste(fmt(p$area_cadastral_m2, 0), "m²"),
    "Perimeter" = paste(fmt(p$perimeter_m, 0), "m"),
    "Distance to point" = paste(fmt(p$dist_to_point_m, 0), "m"),
    "Elevation min / mean / max" = sprintf("%s / %s / %s m", fmt(p$elev_min), fmt(p$elev_mean), fmt(p$elev_max)),
    "Slope min / mean / max" = sprintf("%s / %s / %s°", fmt(p$slope_min), fmt(p$slope_mean), fmt(p$slope_max)),
    "Mean slope" = paste(fmt(p$slope_mean_pct, 0), "%"),
    "Mean aspect" = if (is.na(p$aspect_mean_deg)) "flat" else sprintf("%s° (%s)", fmt(p$aspect_mean_deg, 0), p$aspect_dir),
    "South-facing (SE–SW)" = paste(fmt(p$south_facing_pct, 0), "%"),
    "Southness (-1 N … +1 S)" = fmt(p$southness, 2),
    "Noon sun, winter / equinox" = sprintf("%s / %s × flat", fmt(p$sun_winter_idx, 2), fmt(p$sun_equinox_idx, 2)),
    "PRG zoning" = p$prg_zoning,
    "Vincolo idrogeologico" = p$vincolo_idrogeologico,
    "Bosco (vincolo paesaggistico)" = p$bosco_vincolo_paesaggistico,
    "Copertura boscata" = p$copertura_boscata,
    "Unità di paesaggio" = p$unita_paesaggio,
    "Buildings" = if (isTRUE(p$buildings_n > 0)) sprintf("%d (%s m²): %s", p$buildings_n, fmt(p$buildings_m2, 0), p$buildings) else "none",
    "Cadastral dates (in / end)" = sprintf("%s / %s", p$DataIn, trimws(p$DataFi))
  )
  rows[is.na(rows)] <- "-"
  sprintf(
    "<div class='pp'><h4>%s%s</h4><table>%s</table></div>",
    htmlEscape(p$parcel_id), if (isTRUE(p$is_target)) " ★ selected point" else "",
    paste0("<tr><th>", names(rows), "</th><td>", htmlEscape(rows), "</td></tr>", collapse = "")
  )
}
parcels_ll$popup <- vapply(seq_len(nrow(parcels_ll)), \(i) popup_html(parcels_ll[i, ]), character(1))
parcels_ll$label <- sprintf("%s — %s m², slope %s°, %s",
                            parcels_ll$parcel_id, fmt(parcels_ll$area_m2, 0),
                            fmt(parcels_ll$slope_mean), parcels_ll$aspect_dir)

# Colour scales: single-hue sequential for magnitude, diverging with grey mid for N/S.
pal_slope <- colorNumeric("Oranges", domain = parcels_ll$slope_mean)
pal_elev <- colorNumeric("Purples", domain = parcels_ll$elev_mean)
pal_south <- colorNumeric(c("#2b6cb0", "#a0a0a0", "#c2410c"), domain = c(-1, 1))
pal_slope_r <- colorNumeric("Oranges", domain = c(0, max(values(slope5), na.rm = TRUE)), na.color = NA)

elev_rng <- global(dtm5, "range", na.rm = TRUE)
contours <- as.contour(surf1$elev, levels = seq(floor(elev_rng[[1]]), ceiling(elev_rng[[2]]), by = 1)) |>
  st_as_sf() |>
  st_transform(4326)
contours$major <- contours$level %% 5 == 0

buffer_ll <- st_transform(buffer, 4326)
point_ll <- st_transform(point, 4326)
centroids <- st_point_on_surface(st_geometry(parcels)) |> st_transform(4326) |> st_sf(particella = parcels$particella, geometry = _)
bb <- st_bbox(st_buffer(buffer, 30) |> st_transform(4326))

metric_layer <- function(map, group, pal, values, title, fmt_fun = labelFormat()) {
  map |>
    addPolygons(data = parcels_ll, group = group, fillColor = pal(values), fillOpacity = 0.65,
                color = "#ffffff", weight = 1.5, opacity = 1,
                popup = ~popup, label = ~label, popupOptions = popupOptions(maxWidth = 460)) |>
    addLegend("bottomright", pal = pal, values = values, title = title, group = group,
              labFormat = fmt_fun, opacity = 0.9)
}

imagery <- list(
  "Esri World Imagery" = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                              attr = "Imagery © Esri, Maxar, Earthstar Geographics", native = 19),
  "Google Satellite" = list(url = "https://mt{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
                            attr = "Imagery © Google", native = 20, sub = "0123"),
  "Google Hybrid" = list(url = "https://mt{s}.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
                         attr = "Imagery © Google", native = 20, sub = "0123"),
  "Esri World Topo" = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
                           attr = "© Esri", native = 19),
  "OpenTopoMap" = list(url = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                       attr = "© OpenStreetMap contributors, SRTM | © OpenTopoMap (CC-BY-SA)", native = 17, sub = "abc"),
  "OpenStreetMap" = list(url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
                         attr = "© OpenStreetMap contributors", native = 19)
)

m <- leaflet(options = leafletOptions(maxZoom = 22, zoomSnap = 0.5)) |>
  fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
for (nm in names(imagery)) {
  b <- imagery[[nm]]
  m <- addTiles(m, urlTemplate = b$url, attribution = b$attr, group = nm,
                options = tileOptions(maxNativeZoom = b$native, maxZoom = 22,
                                      subdomains = b$sub %||% "abc"))
}

groups_overlay <- c("Parcels", "Parcel labels", "Buffer 100 m", "Contours 1 m",
                    "Buildings", "Slope raster", "Mean slope", "Mean elevation", "South exposure")

m <- m |>
  addRasterImage(slope5, colors = pal_slope_r, opacity = 0.6, group = "Slope raster", project = TRUE) |>
  addPolylines(data = contours, group = "Contours 1 m", color = "#fde68a",
               weight = ifelse(contours$major, 1.4, 0.6), opacity = 0.8,
               label = ~paste(level, "m")) |>
  addPolygons(data = buffer_ll, group = "Buffer 100 m", fill = FALSE, color = "#ffffff",
              weight = 2, dashArray = "6 6", opacity = 0.9) |>
  metric_layer("Mean slope", pal_slope, parcels_ll$slope_mean, "Mean slope",
               labelFormat(suffix = "°")) |>
  metric_layer("Mean elevation", pal_elev, parcels_ll$elev_mean, "Mean elevation",
               labelFormat(suffix = " m")) |>
  metric_layer("South exposure", pal_south, parcels_ll$southness, "Southness<br>(-1 N, +1 S)") |>
  addPolygons(data = parcels_ll, group = "Parcels", fillOpacity = 0.05, fillColor = "#ffffff",
              color = ifelse(parcels_ll$is_target, "#ef4444", "#facc15"),
              weight = ifelse(parcels_ll$is_target, 3.5, 2), opacity = 1,
              popup = ~popup, label = ~label, popupOptions = popupOptions(maxWidth = 460),
              highlightOptions = highlightOptions(weight = 4, color = "#22d3ee", bringToFront = TRUE)) |>
  addLabelOnlyMarkers(data = centroids, group = "Parcel labels", label = ~particella,
                      labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                                  textOnly = TRUE, className = "plabel"))
if (!is.null(buildings)) {
  m <- addPolygons(m, data = st_transform(buildings, 4326), group = "Buildings",
                   color = "#f472b6", weight = 2, fillOpacity = 0.3, label = ~Acad_text)
}
m <- m |>
  addCircleMarkers(data = point_ll, radius = 6, color = "#ffffff", weight = 2,
                   fillColor = "#ef4444", fillOpacity = 1,
                   label = sprintf("%s: %.6f, %.6f", site$label, site$lat, site$lon)) |>
  addLayersControl(baseGroups = names(imagery), overlayGroups = groups_overlay,
                   options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(c("Slope raster", "Mean slope", "Mean elevation", "South exposure", "Contours 1 m")) |>
  addScaleBar("bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
  addMeasure(position = "topleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
             activeColor = "#22d3ee", completedColor = "#22d3ee") |>
  addFullscreenControl() |>
  addMiniMap(tiles = providers$OpenStreetMap, toggleDisplay = TRUE, zoomLevelOffset = -6)

# Bing Aerial needs quadkey tile addressing, which L.TileLayer lacks: add it in JS
# and register it with the layers control created above.
m <- onRender(m, "
function(el, x) {
  var map = this;
  var BingLayer = L.TileLayer.extend({
    getTileUrl: function (c) {
      var q = '';
      for (var i = c.z; i > 0; i--) {
        var d = 0, mask = 1 << (i - 1);
        if ((c.x & mask) !== 0) d++;
        if ((c.y & mask) !== 0) d += 2;
        q += d;
      }
      return 'https://ecn.t' + ((c.x + c.y) % 4) + '.tiles.virtualearth.net/tiles/a' + q + '.jpeg?g=1';
    }
  });
  var bing = new BingLayer('', {maxNativeZoom: 19, maxZoom: 22, attribution: 'Imagery \\u00a9 Microsoft Bing'});
  if (map.currentLayersControl) map.currentLayersControl.addBaseLayer(bing, 'Bing Aerial');
  var coords = L.control({position: 'bottomleft'});
  coords.onAdd = function () { this._div = L.DomUtil.create('div', 'coords'); return this._div; };
  coords.addTo(map);
  map.on('mousemove', function (e) {
    coords._div.innerHTML = e.latlng.lat.toFixed(6) + ', ' + e.latlng.lng.toFixed(6);
  });
}")

css <- tags$style(HTML("
  .pp h4 { margin: 0 0 6px; font: 600 14px system-ui, sans-serif; }
  .pp table { border-collapse: collapse; font: 12px system-ui, sans-serif; }
  .pp th { text-align: left; font-weight: 500; color: #555; padding: 2px 10px 2px 0; vertical-align: top; white-space: nowrap; }
  .pp td { padding: 2px 0; }
  .plabel { color: #fff; font: 600 12px system-ui, sans-serif; text-shadow: 0 0 3px #000, 0 0 3px #000; }
  .coords { background: rgba(255,255,255,.85); padding: 2px 6px; font: 12px ui-monospace, monospace; border-radius: 3px; }
"))
title <- tags$div(
  style = "background: rgba(255,255,255,.9); padding: 6px 10px; border-radius: 4px; font: 13px system-ui, sans-serif;",
  HTML(sprintf("<b>%s</b> — %d parcels within %d m of %.6f, %.6f<br>
               <span style='color:#555'>Cadastre: comune GisMaster portal &middot; DTM: Regione Piemonte ICE 2009-2011 5 m (CC BY 4.0)</span>",
               htmlEscape(site$label), nrow(parcels_ll), site$buffer_m, site$lat, site$lon))
)
m <- m |>
  addControl(title, position = "bottomleft", className = "map-title") |>
  prependContent(css)
m$sizingPolicy <- sizingPolicy(defaultWidth = "100%", defaultHeight = "100vh",
                               padding = 0, browser.fill = TRUE)

map_file <- normalizePath(file.path(out, "map.html"), mustWork = FALSE)
saveWidget(m, map_file, selfcontained = TRUE, title = sprintf("%s parcels", site$label))
unlink(file.path(out, "map_files"), recursive = TRUE)
cli_alert_success("Map written to {.path {map_file}}")

# Harvest cadastral parcels and all other municipal GIS layers within a buffer
# around a site, plus the Regione Piemonte 5 m LiDAR DTM covering them.
#
# Usage: Rscript R/01_harvest.R [site_id]
#
# Sources
# - GisMaster municipal portal (sportellounicodigitale.it). Its page embeds the
#   list of ArcGIS MapServer services for the comune (IdCliente = ISTAT code);
#   the services live in the folder <IdCliente> on one of the gisNN hosts.
# - Regione Piemonte "RIPRESA AEREA ICE 2009-2011 - DTM 5" WCS (CC BY 4.0).
#
# Output (all text formats) under data/<site_id>/:
#   harvest.json                     site definition, query parameters, URLs, timestamp
#   cadastre/parcels.geojson         parcels intersecting the buffer (all attributes)
#   cadastre/layers/*.geojson        every other queryable layer intersecting the buffer,
#                                    geometries cropped to the parcels' extent + 25 m
#   metadata/portal_services.json    service list embedded in the portal page
#   metadata/services/*.json         MapServer + full layer metadata per service
#   dem/dtm5.asc (+ .prj)            DTM clip as ESRI ASCII grid, EPSG:32632
#   dem/*.xml                        WCS coverage description and ISO metadata record

suppressPackageStartupMessages({
  library(httr2)
  library(sf)
  library(terra)
})
source("R/utils.R")

site_id <- site_arg()
site <- load_site(site_id)
out <- site_dir(site_id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cli_h1("Harvesting {.val {site_id}} ({site$lat}, {site$lon}), buffer {site$buffer_m} m")

user_agent <- "house-search harvester (R httr2)"

http_get <- function(url, query = list()) {
  request(url) |>
    req_url_query(!!!query) |>
    req_user_agent(user_agent) |>
    req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    req_timeout(120) |>
    req_perform()
}

arcgis_json <- function(url, query = list()) {
  body <- http_get(url, c(query, f = "json")) |> resp_body_string()
  x <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (!is.null(x$error)) {
    cli_abort("ArcGIS error from {.url {url}}: {x$error$message}")
  }
  x
}

# ---- 1. Portal: discover ArcGIS host and service list ------------------------

portal_url <- site$cadastre_portal
id_cliente <- sub(".*[?&]IdCliente=([^&]+).*", "\\1", portal_url)
cli_progress_step("Reading portal page (IdCliente {id_cliente})")
portal_html <- http_get(portal_url) |> resp_body_string()

# Service definitions are emitted as `gisMasterServices[i].prop = value;`.
svc_lines <- regmatches(
  portal_html,
  gregexpr("gisMasterServices\\[[0-9]+\\]\\.[A-Za-z]+ = [^;]+;", portal_html)
)[[1]]
svc_tbl <- data.frame(
  idx = as.integer(sub("^gisMasterServices\\[([0-9]+)\\].*", "\\1", svc_lines)),
  key = sub("^gisMasterServices\\[[0-9]+\\]\\.([A-Za-z]+) = .*", "\\1", svc_lines),
  val = gsub("^'|'$", "", sub("^[^=]+= (.*);$", "\\1", svc_lines))
)
portal_services <- lapply(split(svc_tbl, svc_tbl$idx), function(d) as.list(setNames(d$val, d$key)))
names(portal_services) <- NULL
write_json_pretty(
  list(portal_url = portal_url, id_cliente = id_cliente, services = portal_services),
  file.path(out, "metadata", "portal_services.json")
)

hosts <- unique(regmatches(
  portal_html,
  gregexpr("https://gis[0-9]+\\.sportellounicodigitale\\.it", portal_html)
)[[1]])
rest_root <- NULL
for (h in hosts) {
  folder <- try(arcgis_json(sprintf("%s/arcgis/rest/services/%s", h, id_cliente)), silent = TRUE)
  if (!inherits(folder, "try-error") && length(folder$services) > 0) {
    rest_root <- sprintf("%s/arcgis/rest/services", h)
    break
  }
}
if (is.null(rest_root)) cli_abort("No ArcGIS host in the portal page has folder {.val {id_cliente}}.")
services <- vapply(folder$services, `[[`, character(1), "name")
cli_progress_step("Found {length(services)} MapServer service{?s} at {.url {rest_root}}")

# ---- 2. Service and layer metadata -------------------------------------------

svc_meta <- list()
for (s in services) {
  base <- sprintf("%s/%s/MapServer", rest_root, s)
  meta <- arcgis_json(base)
  layers <- arcgis_json(paste0(base, "/layers"))
  write_json_pretty(
    list(url = base, service = meta, layers = layers$layers, tables = layers$tables),
    file.path(out, "metadata", "services", paste0(basename(s), ".json"))
  )
  svc_meta[[s]] <- list(base = base, meta = meta, layers = layers$layers)
}

# ---- 3. Query every queryable feature layer within the buffer ----------------

point_geom <- jsonlite::toJSON(
  list(x = site$lon, y = site$lat, spatialReference = list(wkid = 4326)),
  auto_unbox = TRUE, digits = NA
)
buffer_query <- list(
  geometry = point_geom,
  geometryType = "esriGeometryPoint",
  inSR = 4326,
  spatialRel = "esriSpatialRelIntersects",
  distance = site$buffer_m,
  units = "esriSRUnit_Meter"
)

xy_only <- function(coords) {
  if (length(coords) > 0 && is.numeric(coords[[1]])) return(coords[1:2])
  lapply(coords, xy_only)
}

# Returns a GeoJSON FeatureCollection (as a list) of features in the buffer.
query_layer <- function(layer_url, max_records) {
  ids <- arcgis_json(paste0(layer_url, "/query"), c(buffer_query, returnIdsOnly = "true"))
  oids <- unlist(ids$objectIds)
  if (length(oids) == 0) return(NULL)
  chunk <- min(200L, max_records)
  features <- list()
  for (part in split(sort(oids), ceiling(seq_along(oids) / chunk))) {
    body <- http_get(paste0(layer_url, "/query"), list(
      objectIds = paste(part, collapse = ","),
      outFields = "*",
      returnGeometry = "true",
      outSR = 4326,
      f = "geojson"
    )) |> resp_body_string()
    fc <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    # Some layers carry dummy Z/M values ([x, y, 0, null]); keep x, y only.
    fc$features <- lapply(fc$features, function(ft) {
      if (!is.null(ft$geometry)) ft$geometry$coordinates <- xy_only(ft$geometry$coordinates)
      ft
    })
    features <- c(features, fc$features)
  }
  list(type = "FeatureCollection", features = features)
}

parcel_layer <- NULL
layer_log <- list()
aux_layers <- list()
for (s in names(svc_meta)) {
  for (ly in svc_meta[[s]]$layers) {
    caps <- ly$capabilities %||% ""
    queryable <- identical(ly$type, "Feature Layer") && grepl("Query", caps)
    entry <- list(service = s, layer_id = ly$id, name = ly$name, type = ly$type,
                  geometry_type = ly$geometryType %||% NA, n_features = 0L,
                  file = NA, clipped = FALSE)
    if (queryable) {
      layer_url <- sprintf("%s/%d", svc_meta[[s]]$base, ly$id)
      fc <- tryCatch(query_layer(layer_url, ly$maxRecordCount %||% 1000L), error = function(e) {
        cli_warn("Query failed for {s} / {ly$name}: {conditionMessage(e)}")
        NULL
      })
      if (!is.null(fc) && length(fc$features) > 0) {
        entry$n_features <- length(fc$features)
        entry$url <- layer_url
        if (identical(ly$name, "Particelle catastali")) {
          # Parcels are stored exactly as served: every parcel touching the buffer.
          entry$file <- file.path(out, "cadastre", "parcels.geojson")
          write_json_pretty(fc, entry$file)
          parcel_layer <- layer_url
        } else {
          entry$file <- file.path(out, "cadastre", "layers",
                                  sprintf("%s__%02d_%s.geojson", basename(s), ly$id, slugify(ly$name)))
          entry$clipped <- TRUE
          aux_layers[[entry$file]] <- fc
        }
      }
    }
    cli_inform("  {.strong {basename(s)}} [{ly$id}] {ly$name}: {entry$n_features} feature{?s}")
    layer_log[[length(layer_log) + 1]] <- entry
  }
}
if (is.null(parcel_layer)) cli_abort("No 'Particelle catastali' features found in the buffer.")

# Other layers (zoning, constraints, forest...) can be municipality-wide
# polygons: keep their attributes but crop geometries to the parcels' extent.
parcels <- st_read(file.path(out, "cadastre", "parcels.geojson"), quiet = TRUE)
clip_box <- st_as_sfc(st_bbox(st_buffer(st_transform(parcels, crs_metric), 25)))
for (f in names(aux_layers)) {
  lyr <- st_read(jsonlite::toJSON(aux_layers[[f]], auto_unbox = TRUE, digits = NA), quiet = TRUE)
  lyr <- st_transform(lyr, crs_metric) |> st_make_valid()
  lyr <- suppressWarnings(st_intersection(lyr, clip_box))
  lyr <- lyr[!st_is_empty(lyr), ]
  if (nrow(lyr) == 0) next
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  st_write(st_transform(lyr, 4326), f, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
           layer_options = c("COORDINATE_PRECISION=8", "RFC7946=YES"))
}

# ---- 4. DTM from Regione Piemonte WCS ----------------------------------------

wcs_url <- "https://geomap.reteunitaria.piemonte.it/ws/taims/rp-01/taimsdtmwcs/wcs_ice_2009_2011_dtm"
dtm_record <- "https://www.geoportale.piemonte.it/geonetwork/srv/api/records/r_piemon:224de2ac-023e-441c-9ae0-ea493b217a8e/formatters/xml"
dem_dir <- file.path(out, "dem")
dir.create(dem_dir, recursive = TRUE, showWarnings = FALSE)

cli_progress_step("Downloading 5 m DTM")
bb <- st_bbox(st_transform(parcels, crs_metric))
# Snap to the DTM grid (origin 290748, 5165171; 5 m cells) with a 25 m margin.
snap <- function(v, origin, f) origin + f((v - origin) / 5) * 5
bbox <- c(
  snap(bb[["xmin"]] - 25, 290748, floor), snap(bb[["ymin"]] - 25, 5165171, floor),
  snap(bb[["xmax"]] + 25, 290748, ceiling), snap(bb[["ymax"]] + 25, 5165171, ceiling)
)
tif <- tempfile(fileext = ".tif")
dtm_query <- list(
  SERVICE = "WCS", VERSION = "1.0.0", REQUEST = "GetCoverage", COVERAGE = "DTM",
  CRS = "EPSG:32632", BBOX = paste(bbox, collapse = ","),
  RESX = 5, RESY = 5, FORMAT = "GEOTIFF_16"
)
request(wcs_url) |>
  req_url_query(!!!dtm_query) |>
  req_user_agent(user_agent) |>
  req_timeout(300) |>
  req_perform(path = tif) |>
  invisible()
dtm <- rast(tif)
NAflag(dtm) <- -99
names(dtm) <- "elevation_m"
writeRaster(dtm, file.path(dem_dir, "dtm5.asc"), filetype = "AAIGrid",
            gdal = c("DECIMAL_PRECISION=2", "FORCE_CELLSIZE=YES"),
            NAflag = -99, overwrite = TRUE)
unlink(list.files(dem_dir, pattern = "\\.aux\\.xml$", full.names = TRUE))

writeLines(
  http_get(wcs_url, list(SERVICE = "WCS", VERSION = "1.0.0", REQUEST = "DescribeCoverage")) |>
    resp_body_string(),
  file.path(dem_dir, "wcs_describe_coverage.xml")
)
writeLines(http_get(dtm_record) |> resp_body_string(), file.path(dem_dir, "dtm5_iso_metadata.xml"))

# ---- 5. Harvest log ----------------------------------------------------------

write_json_pretty(
  list(
    site = site,
    harvested_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    arcgis_rest_root = rest_root,
    services = services,
    parcel_layer = parcel_layer,
    buffer_query = buffer_query,
    layers = layer_log,
    dtm = list(
      source = "Regione Piemonte - RIPRESA AEREA ICE 2009-2011 - DTM 5 (LiDAR), CC BY 4.0",
      wcs = wcs_url, query = dtm_query, metadata = dtm_record,
      file = file.path(dem_dir, "dtm5.asc"), crs = "EPSG:32632"
    )
  ),
  file.path(out, "harvest.json")
)
cli_progress_done()
cli_alert_success("Harvested {nrow(parcels)} parcels into {.path {out}}")

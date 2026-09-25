# Shared helpers for the harvest and map scripts.

suppressPackageStartupMessages({
  library(cli)
  library(jsonlite)
})

# Project CRS for metric computations (the cadastre's native CRS in Piemonte).
crs_metric <- 32632

# Read a site definition from config/sites.json by id.
load_site <- function(site_id, config = "config/sites.json") {
  sites <- jsonlite::read_json(config, simplifyVector = FALSE)
  ids <- vapply(sites, `[[`, character(1), "id")
  if (!site_id %in% ids) {
    cli::cli_abort(c(
      "Unknown site {.val {site_id}}.",
      i = "Known sites in {.file {config}}: {.val {ids}}."
    ))
  }
  sites[[match(site_id, ids)]]
}

site_dir <- function(site_id, ...) file.path("data", site_id, ...)
output_dir <- function(site_id, ...) file.path("output", site_id, ...)

# Site id from the first command-line argument, defaulting to the first site.
site_arg <- function(config = "config/sites.json") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 1) return(args[[1]])
  jsonlite::read_json(config)[[1]]$id
}

write_json_pretty <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE,
                       digits = NA, null = "null", na = "null")
}

slugify <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

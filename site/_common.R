# Helpers shared by the site pages. Pages execute with the working directory
# at the Quarto project root (site/), so repo files are one level up.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})
repo <- normalizePath("..")
source(file.path(repo, "R", "utils.R"), chdir = FALSE)

sites <- read_json(file.path(repo, "config", "sites.json"))
names(sites) <- vapply(sites, `[[`, "", "id")
glossary <- read_json(file.path(repo, "config", "translations.json"))

tr_layer <- function(name) {
  g <- glossary$layers[[name]]
  list(en = g$en %||% name, theme = g$theme %||% "other", description = g$description %||% "")
}
tr_field <- function(f) glossary$fields[[f]] %||% f

site_data <- function(id) {
  harvest <- read_json(file.path(repo, "data", id, "harvest.json"))
  stats <- read.csv(file.path(repo, "output", id, "parcel_stats.csv"),
                    colClasses = c(particella = "character", DataIn = "character",
                                   DataFi = "character"), check.names = FALSE)
  # ov_<slug> columns hold overlay classes; map them back to their source layer.
  lyr <- Filter(\(l) !is.na(l$file %||% NA), harvest$layers)
  overlays <- bind_rows(lapply(lyr, \(l) {
    tl <- tr_layer(l$name)
    tibble(col = paste0("ov_", slugify(l$name)), layer = l$name, en = tl$en,
           theme = tl$theme, description = tl$description)
  })) |>
    filter(col %in% names(stats)) |>
    distinct(col, .keep_all = TRUE)
  list(site = sites[[id]], harvest = harvest, stats = stats,
       target = stats[stats$is_target %in% c(TRUE, "TRUE"), ][1, ], overlays = overlays)
}

num <- function(x, d = 0) ifelse(is.na(x), "–", formatC(as.numeric(x), format = "f", digits = d, big.mark = ","))

slope_word <- function(deg) {
  cut(deg, c(-Inf, 5, 10, 15, 25, Inf),
      labels = c("flat to gentle", "gentle", "moderate", "steep", "very steep")) |> as.character()
}

# Overlay classes of a parcel row whose layer has one of the given themes.
overlay_text <- function(d, row, themes) {
  o <- d$overlays[d$overlays$theme %in% themes, ]
  v <- vapply(o$col, \(c) as.character(row[[c]]), "")
  v <- v[!is.na(v) & v != ""]
  if (!length(v)) return("none")
  paste(v, collapse = "; ")
}

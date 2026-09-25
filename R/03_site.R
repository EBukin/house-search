# Build the GitHub Pages site (Quarto website in site/, rendered to docs/).
#
# Usage: Rscript R/03_site.R      (run 01_harvest.R and 02_map.R for each site first)
#
# Generates one page per site in config/sites.json (site/houses/<id>.qmd, all
# sharing site/_house.qmd), copies each site's map into site/maps/, then renders.

source("R/utils.R")

sites <- jsonlite::read_json("config/sites.json")
dir.create("site/houses", showWarnings = FALSE)
dir.create("site/maps", showWarnings = FALSE)
unlink(list.files("site/houses", pattern = "\\.qmd$", full.names = TRUE))

for (i in seq_along(sites)) {
  s <- sites[[i]]
  map <- output_dir(s$id, "map.html")
  if (!file.exists(map)) cli::cli_abort("Missing {.file {map}}: run R/02_map.R {s$id} first.")
  file.copy(map, file.path("site", "maps", paste0(s$id, ".html")), overwrite = TRUE)
  comune <- jsonlite::read_json(site_dir(s$id, "harvest.json"))$comune %||% ""
  writeLines(c(
    "---",
    sprintf('title: "%s — %s"', s$label, comune),
    sprintf('subtitle: "%.6f, %.6f"', s$lat, s$lon),
    sprintf("order: %d", i),
    "---",
    "",
    "```{r}",
    "#| include: false",
    sprintf('site_id <- "%s"', s$id),
    "```",
    "",
    "{{< include ../_house.qmd >}}"
  ), file.path("site", "houses", paste0(s$id, ".qmd")))
}

# Navbar menu: one entry per house, between the marker comments in _quarto.yml.
yml <- readLines("site/_quarto.yml")
start <- grep("# houses-menu-start", yml, fixed = TRUE)
end <- grep("# houses-menu-end", yml, fixed = TRUE)
indent <- sub("#.*", "", yml[start])
menu <- sprintf("%s- houses/%s.qmd", indent, vapply(sites, `[[`, "", "id"))
writeLines(c(yml[seq_len(start)], menu, yml[end:length(yml)]), "site/_quarto.yml")

quarto <- Sys.getenv("QUARTO_PATH", unset = Sys.which("quarto"))
if (!nzchar(quarto)) {
  cand <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Positron", "resources", "app",
                    "quarto", "bin", "quarto.exe")
  if (file.exists(cand)) quarto <- cand else cli::cli_abort("Quarto not found; set QUARTO_PATH.")
}
# Quarto mangles a backslashed R_HOME inherited from Rscript on Windows
# ("...\bin" -> "\b"); give it R's bin directory with forward slashes instead.
r_bin <- normalizePath(R.home("bin"), winslash = "/")
Sys.unsetenv("R_HOME")
Sys.setenv(QUARTO_R = r_bin)
status <- system2(quarto, c("render", "site"))
if (status != 0) cli::cli_abort("quarto render failed")
invisible(file.create("docs/.nojekyll"))
cli::cli_alert_success("Site rendered to {.path docs/}")

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Searching for houses in Italy and understanding their land plots and geometries. The code is plain R scripts, not a package. Candidate sites (id, lat/lon, buffer, link to the comune's cadastre portal) are listed in `config/sites.json`.

## Commands

Run from the repo root (the scripts use relative paths). The site id defaults to the first entry in `config/sites.json`.

```sh
Rscript R/01_harvest.R option2   # harvest cadastre + DTM into data/option2/
Rscript R/02_map.R option2       # stats + map into output/option2/
Rscript R/03_site.R              # GitHub Pages site: site/ (Quarto) -> docs/
```

Required packages: sf, terra, httr2, jsonlite, dplyr, leaflet, leaflet.extras, htmlwidgets, cli, knitr, rmarkdown. `saveWidget(selfcontained = TRUE)` needs pandoc. `03_site.R` needs Quarto: it uses `QUARTO_PATH`, then `quarto` on PATH, then the copy bundled with Positron. There are no tests.

When a site's coordinates change, delete `data/<id>` and `output/<id>` before re-harvesting, so layer files from the old buffer don't linger. Then run all three scripts again.

## Site (GitHub Pages)

- The site is served from `main:/docs`. `docs/` is committed build output: re-run `R/03_site.R` and commit `docs/` after any data change.
- `site/index.qmd` is the summary of all houses. `03_site.R` generates `site/houses/<id>.qmd` stubs that set `site_id` and include `site/_house.qmd`. It also copies `output/<id>/map.html` to `site/maps/` (embedded as an iframe) and writes the navbar menu between marker comments in `site/_quarto.yml`. `site/houses/` and `site/maps/` are git-ignored intermediates.
- Pages run with `execute-dir: project`, so paths are relative to `site/`. Shared helpers are in `site/_common.R`.
- `03_site.R` unsets `R_HOME` and sets `QUARTO_R` because Quarto on Windows mangles a backslashed `R_HOME` inherited from Rscript.

## Architecture

- **Cadastre source:** the comune's GisMaster portal (`geoportale.sportellounicodigitale.it/GisMaster/Default.aspx?IdCliente=<ISTAT code>`). The portal page embeds `gisMasterServices[i]...` JS and the `gisNN.sportellounicodigitale.it` hosts. The ArcGIS REST MapServers live in folder `<IdCliente>` on one of those hosts. `01_harvest.R` discovers all of this from the portal URL, so adding a site in another comune that uses the same portal should only need a new `sites.json` entry.
- Parcels are the layer named `Particelle catastali`. Useful attributes: `Foglio` (sheet), `Acad_text` (parcel number), `Area` (cadastral m²). Every other queryable layer (buildings, PRG zoning, constraints, forest, landscape) is also harvested. Those layers can be municipality-wide polygons, so they are cropped to the parcels' extent + 25 m. Parcels themselves are stored exactly as served.
- **DTM:** Regione Piemonte ICE 2009-2011 5 m LiDAR, fetched from the WCS 1.0.0 service and snapped to the native grid (origin 290748, 5165171). It is stored as ESRI ASCII grid (`dem/dtm5.asc`) so it stays text. Works only for sites in Piemonte.
- Layer schemas differ between comuni (zoning is `Destinazioni urbanistiche` in one comune and `AreNor` in another; class fields are `Sigla`, `Siglia`, `classe` or `Acad_text`). `02_map.R` therefore treats layers generically. Every polygon layer outside a "Cartografia catastale" service becomes an `ov_<slug>` column holding the parcel's dominant class. Every harvested layer is drawn on the map, with its theme, colour and English text taken from `config/translations.json`. Add new layer names and codes there when a new comune brings them.
- `02_map.R` does metric work in EPSG:32632. Slope and aspect are computed at 5 m, then resampled bilinearly to 1 m (aspect via sin/cos) so small parcels get meaningful stats. The script also joins the dominant `Sigla` class of each overlay layer to each parcel. Bing Aerial is added in `onRender` JS because it needs quadkey tile URLs.
- The Piemonte orthophoto WMTS and the Agenzia Entrate cadastral WMS do not offer EPSG:3857, so they can't be used as leaflet tile layers.
- Windows curl (schannel) fails certificate revocation checks against some of these hosts; use `curl --ssl-no-revoke` when probing by hand. R's httr2 works as is.

## Conventions

### Harvested data
- Commit every piece of data harvested from the internet under `data/<site-or-source>/`. Keep `harvest.json` there with the URLs, query parameters and timestamp.
- Store data in text formats so it diffs well in git: JSON for records, GeoJSON for geometries, CSV for flat tables, ASCII grid for rasters. Use binary formats only when no text format will work.
- Derived results (stats, maps) go to `output/<site>/`.

### R development
- When writing R code, use the R skills from the Posit plugins (`r-lib:*`, and `shiny:*` / `quarto:*` where they apply). For example, use `r-lib:cli` for messages and errors, and `r-lib:mirai` for parallel harvesting.
- An R MCP REPL (`mcp__r__repl`) is available for running R code interactively.

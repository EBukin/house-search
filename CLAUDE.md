# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Searching for houses in Italy and understanding their land plots and geometries. The project is written in R (see `.gitignore` for RStudio artifacts).

The repository is at an early stage. There is no code, build or test setup yet. Update this file once they exist.

## Conventions

### Harvested data
- Commit every piece of data harvested from the internet (listings, cadastral/plot geometries, API responses) under `data/<source-or-topic>/`, for example `data/immobiliare/` or `data/catasto/`.
- Store data in text formats so it diffs well in git: JSON for records, GeoJSON for geometries, CSV for flat tables. Use binary formats (`.rds`, `.gpkg`, `.parquet`, images) only when no text format will work.

### R development
- When writing R code, use the R skills from the Posit plugins (`r-lib:*`, and `shiny:*` / `quarto:*` where they apply). For example, use `r-lib:r-package-development` for package structure, `r-lib:testing-r-packages` for testthat tests, `r-lib:cli` for messages and errors, and `r-lib:mirai` for parallel or async harvesting.
- An R MCP REPL (`mcp__r__repl`) is available for running R code interactively.

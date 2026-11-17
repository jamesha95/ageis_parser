# National Greenhouse Accounts downloader
# Replaces "AGEIS scraper.R", which no longer works because the old
# ageis.climatechange.gov.au ASP.NET page was retired in late 2022.
#
# The new site (greenhouseaccounts.climatechange.gov.au) is a JavaScript
# app that loads its numbers from a JSON API. This script calls that API
# directly, so there is no HTML to parse and no ViewState/cookies to copy.
#
# Endpoints used (found by inspecting the site's own requests):
#   /api/Defaults            locations, gases and other lookup trees
#   /api/SectorHierarchy     UNFCCC sector tree for a location
#   /api/OutputTimeSeries    every year (1990 onward) for up to 16 sectors per call
#
# Note: the API refuses more than 16 sectors per request, so sectors are
# sent in chunks of 16.
#
# Rate limiting: the site sits behind a firewall (Azure Front Door) that
# answered "403 Forbidden" after ~60 requests at 2 per second. The limit isn't
# published, so requests are spaced out, a 403 triggers a long pause and retry,
# and each location x gas result is saved to disk so a failure loses little work.

# Set up --------------------------------------------------

library(tidyverse)
library(httr2)   # install.packages("httr2") if needed
library(fst)

base_url <- "https://greenhouseaccounts.climatechange.gov.au/api"

# Same codes the old form used: fuel 1 = "not applicable", datatype 13 = "Emissions - net"
fuel_not_applicable <- 1
datatype_net        <- 13
max_sectors_per_call <- 16

seconds_between_requests <- 2     # raise this if you still get 403s
cache_dir <- "data/anga_cache"    # one file per location x gas
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)


# Helpers --------------------------------------------------

# GET an API endpoint and return the parsed JSON as a list
anga_get <- function(endpoint, ...) {
  body <- request(base_url) |>
    req_url_path_append(endpoint) |>
    req_url_query(...) |>
    req_user_agent("ageis_parser R script (personal research)") |>
    req_throttle(rate = 1 / seconds_between_requests) |>
    req_retry(
      max_tries    = 4,
      # treat the firewall's 403 as "slow down", not a permanent refusal
      is_transient = \(resp) resp_status(resp) %in% c(403, 429, 503),
      backoff      = \(attempt) 60 * attempt   # wait 1, 2, then 3 minutes
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)

  if (!is.null(body$error) && nzchar(body$error)) {
    stop("API error from ", endpoint, ": ", body$error, call. = FALSE)
  }
  body
}

# Flatten one of the API's nested {id, name, level, children} trees into a tibble
flatten_tree <- function(nodes, parent_id = NA_character_) {
  map_dfr(nodes, function(n) {
    bind_rows(
      tibble(
        id           = as.character(n$id),
        name         = n$name,
        level        = n$level,
        confidential = n$confidential,
        parent_id    = parent_id
      ),
      flatten_tree(n$children, parent_id = as.character(n$id))
    )
  })
}


# Metadata --------------------------------------------------

defaults <- anga_get("Defaults")

location_table <- flatten_tree(defaults$locationtree$Nodes) |>
  select(location_id = id, location = name)

# CO2-e gases (AR5 global warming potentials) and raw gases (e.g. Methane in tonnes of CH4)
gas_table <- bind_rows(
  flatten_tree(defaults$gastreear5$Nodes) |> mutate(basis = "AR5 CO2-e"),
  flatten_tree(defaults$gastreeraw$Nodes) |> mutate(basis = "raw")
) |>
  select(gas_id = id, gas = name, basis)

first_year <- defaults$firstyearnir
last_year  <- defaults$lastyearnir

# UNFCCC sector tree for one location (level 0 = "Total UNFCCC")
anga_sectors <- function(location_id) {
  anga_get("SectorHierarchy", LocationID = location_id, top = "UNFCCC")$Nodes |>
    flatten_tree() |>
    rename(sector_id = id, sector = name, sector_level = level)
}


# Key function --------------------------------------------------

# All years for a set of sectors, one location and one gas.
# Returns one row per year x sector. Emissions are in gigagrams (= kilotonnes).
anga_timeseries <- function(sector_ids, location_id, gas_id) {
  chunks <- split(sector_ids, ceiling(seq_along(sector_ids) / max_sectors_per_call))

  empty <- tibble(year = integer(), location_id = character(), sector_id = character(),
                  gas_id = character(), emissions_kt = double())

  map_dfr(chunks, function(ids) {
    res <- anga_get(
      "OutputTimeSeries",
      urlencodedsectorlist = paste(ids, collapse = ","),
      locationid           = location_id,
      fuelid               = fuel_not_applicable,
      gasid                = gas_id,
      datatypeid           = datatype_net
    )
    if (length(res$data) == 0) return(NULL)

    map_dfr(res$data, \(r) tibble(
      year         = r$date,
      location_id  = as.character(r$l),
      sector_id    = as.character(r$s),
      gas_id       = as.character(r$g),
      emissions_kt = r$v %||% NA_real_
    ))
  }) |>
    bind_rows(empty)   # guarantees the columns exist even if nothing came back
}


# Quick test --------------------------------------------------
# Australia, total CO2-e (AR5), top-level sectors. Should take a few seconds.
# 1990 "Total UNFCCC" should be roughly 616,000 kt.

aus_sectors <- anga_sectors(3)

test <- anga_timeseries(
  sector_ids  = aus_sectors |> filter(sector_level <= 1) |> pull(sector_id),
  location_id = 3,
  gas_id      = 103
) |>
  left_join(aus_sectors, by = "sector_id")

test |> filter(year == first_year)


# Full download --------------------------------------------------
# Choose what to download. The full sector tree is ~1,300 sectors, i.e. ~80
# requests per location x gas, so start small and scale up.

locations_wanted <- location_table$location_id                 # all 10 locations
gases_wanted     <- c("103", "107", "108", "109")              # CO2-e total, CO2, CH4, N2O (AR5)
max_level        <- 5                                          # deepest sector level to keep

all_data <- map_dfr(locations_wanted, function(loc) {
  sectors <- anga_sectors(loc) |> filter(sector_level <= max_level)

  map_dfr(gases_wanted, function(g) {
    cache_file <- file.path(cache_dir, glue::glue("loc{loc}_gas{g}_level{max_level}.rds"))

    # Already downloaded on an earlier run? Reuse it instead of asking the server again.
    if (file.exists(cache_file)) {
      message("Location ", loc, ", gas ", g, ": using saved copy")
      return(readRDS(cache_file))
    }

    message("Location ", loc, ", gas ", g, ": downloading")
    result <- anga_timeseries(sectors$sector_id, location_id = loc, gas_id = g) |>
      # keep every sector/year combination so missing values are visible
      right_join(
        expand_grid(sectors, year = first_year:last_year) |>
          mutate(location_id = as.character(loc), gas_id = g),
        by = c("year", "location_id", "sector_id", "gas_id")
      )

    saveRDS(result, cache_file)
    result
  })
})


# Tidy and save --------------------------------------------------

cleaned_data <- all_data |>
  left_join(location_table, by = "location_id") |>
  left_join(gas_table, by = "gas_id") |>
  mutate(
    available = case_when(
      !is.na(emissions_kt) ~ "available",
      confidential         ~ "confidential",
      TRUE                 ~ "not_available"
    ),
    gas = gas |> str_to_lower() |> str_replace_all("[:blank:]", "_")
  ) |>
  select(year, location, gas, sector_level, sector_id, parent_id, sector,
         emissions_kt, available)

write_fst(cleaned_data, "data/all_data.fst")

# just a test graph to see if it looks sensible
cleaned_data |> 
  filter(
    location == "AUSTRALIA", 
    gas == "carbon_dioxide_equivalent_-_ar5", 
    sector_level == 1) |>
  ggplot(aes(x = year, y = emissions_kt, col = sector)) + 
  geom_col()

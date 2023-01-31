#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  # Get federal marine bioregions
  pipedat::pipedat("f635934a")

  # Load federal bioregions
  dat <- sf::st_read(
    "data/data-raw/federal_marine_bioregions-f635934a/federal_marine_bioregions-f635934a.geojson",
    quiet = TRUE
  )

  # Select Scotian Shelf bioregion
  uid <- c("Scotian Shelf")
  aoi <- dat[dat$NAME_E %in% uid, ] |>
    sf::st_transform(crs = param$crs)


  # Export data
  out <- here::here("data","aoi")
  chk_create(out)
  sf::st_write(
    obj = aoi,
    dsn = here::here(out, "aoi.gpkg"),
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

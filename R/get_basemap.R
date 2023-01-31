#' Script to get basemaps for figures
#'
#' @export
get_basemap <- function() {
  out <- here::here("data","basemap")
  chk_create(out)
  
  # CANADA
  prov <- c(
    'Québec', 
    'Nova Scotia',
    'New Brunswick', 
    'Newfoundland and Labrador', 
    'Prince Edward Island'
  )
  raster::getData('GADM', country = 'CAN', level = 1, path = 'data') |>
  sf::st_as_sf() |>
  dplyr::filter(NAME_1 %in% prov) |>
  sf::st_simplify(dTolerance = .01, preserveTopology = F)
  sf::st_write(here::here(out, "canada.gpkg"), quiet = TRUE, overwrite = TRUE)
  
  # USA
  raster::getData('GADM', country = 'USA', level = 0, path = 'data') |>
  sf::st_as_sf() |>
  sf::st_simplify(dTolerance = .05, preserveTopology = F) |>
  sf::st_write(here::here(out, "usa.gpkg"), quiet = TRUE, overwrite = TRUE)
  







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

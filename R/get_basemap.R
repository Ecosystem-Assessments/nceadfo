#' Script to get basemaps for figures
#'
#' @export
get_basemap <- function() {
  out <- here::here("data","basemap")
  chk_create(out)
  
  # CANADA EAST
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
  sf::st_simplify(dTolerance = 200, preserveTopology = F) |>
  sf::st_write(here::here(out, "canada.gpkg"), quiet = TRUE, overwrite = TRUE)

  # CANADA
  raster::getData('GADM', country = 'CAN', level = 0, path = 'data') |>
  sf::st_as_sf() |>
  sf::st_simplify(dTolerance = 600, preserveTopology = FALSE) |>
  sf::st_make_valid() |>
  sf::st_write(here::here(out, "canada_full.gpkg"), quiet = TRUE, overwrite = TRUE)
  
  # USA
  raster::getData('GADM', country = 'USA', level = 0, path = 'data') |>
  sf::st_as_sf() |>
  sf::st_simplify(dTolerance = 600, preserveTopology = F) |>
  sf::st_write(here::here(out, "usa.gpkg"), quiet = TRUE, overwrite = TRUE)
}

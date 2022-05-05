#' Script to make grid for the assessment
#'
#' @export
make_grid <- function(cellsize) {
  # Load area of interest
  aoi <- sf::st_read("data/data-basemap/aoi.geojson", quiet = TRUE)

  # Make grid with unique identifier
  grid_poly <- sf::st_make_grid(aoi, cellsize = cellsize)  
  grid_poly <- grid_poly[aoi] |>
               sf::st_sf() |>
               dplyr::mutate(uid = 1:dplyr::n())  
  
  # Export
  sf::st_write(
    obj = grid_poly,
    dsn = "./data/data-basemap/grid_poly.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )
}
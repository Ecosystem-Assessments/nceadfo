#' Script to make grid for the assessment
#'
#' @export
make_grid <- function(cellsize) {
  # Load area of interest
  aoi <- sf::st_read("data/data-basemap/aoi.geojson", quiet = TRUE)

  # Make grids
  grid_poly <- sf::st_make_grid(aoi, cellsize = cellsize)  
  grid_poly <- grid_poly[aoi] |>
               sf::st_sf() |>
               dplyr::mutate(uid = 1:dplyr::n())  
  
  # Raster grid
  grid_ras <- stars::st_rasterize(grid_poly, dx = cellsize, dy = cellsize)

  # Export
  sf::st_write(
    obj = grid_poly,
    dsn = "./data/data-grid/grid_poly.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  stars::write_stars(
    obj = grid_ras,
    dsn = "./data/data-grid/grid_raster.tif",
    delete_dsn = TRUE,
    quiet = TRUE
  )
}
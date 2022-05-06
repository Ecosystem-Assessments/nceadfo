#' Script to integrate and prepare data for analysis using the pipedat package
#'
#' @export
integrate_data <- function() {
  uid <- param$pipedat$integration_pipelines
  crs <- param$crs
  grd_poly <- sf::st_read("data/data-basemap/grid_poly.geojson")
  pipedat::pipin(uid, grid = grd_poly)
}
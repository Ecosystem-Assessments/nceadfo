#' Area of interest
#'
#' Area of interest for contribution agreement on the assessment of network cumulative effects of global changes on communities of the Scotian Shelf Bioregion
#'
#' @keywords area of interest
#' @keywords scotian shelf
#'
#' @export
#'
#' @details This function exports the area of interest for the project
#'

fmt_aoi <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import  data
  ncea_load("data0001")


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Select Scotian Shelf bioregion
  uid <- c("11. Scotian Shelf / Plate-forme Scotian")
  # uid <- c("11. Scotian Shelf / Plate-forme Scotian",
  #          "10. Newfoundland-Labrador Shelves / Plates-formes de Terre-Neuve et du Labrador")

  # Area of interest
  aoi <- data0001[data0001$Legend %in% uid, ]


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export to disk
  st_write(obj = aoi,
           dsn = "./data/data-output/aoi.geojson",
           delete_dsn = TRUE)

  # Export for lazy load
  save(aoi, file = './data/aoi.RData')
}

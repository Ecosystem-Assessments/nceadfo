#' Data 0002 : Study grid
#'
#' Gridded study area used jointly with N. Kelly and G. Murphy for the Maritimes region
#'
#' @keywords study grid
#'
#' @source TO DO
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0002 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0002-study_grid/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0002 <- st_read(glue("{folder}/pu.shp"), quiet = TRUE)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0002,
           dsn = "./data/data-format/data0002-grid.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}

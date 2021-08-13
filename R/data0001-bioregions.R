#' Data 0001 : Federal Marine Bioregions
#'
#' The spatial planning framework for Canada's national network of Marine Protected Areas (MPAs) is comprised of 13 ecologically defined bioregions that cover Canada's oceans and the Great Lakes.
#'
#' @keywords
#' @keywords
#' @keywords
#'
#' @source https://open.canada.ca/data/en/dataset/23eb8b56-dac8-4efc-be7c-b8fa11ba62e9
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0001 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0001-bioregions/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  if (!file.exists(paste0(output, 'DFO_Marine_Bioregions.zip'))) {
    # Download from Open Canada portal
    uid <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
    library(rgovcan)
    govcan_dl_resources(uid, path = folder)

    # Unzip file
    unzip(zipfile = paste0(folder, 'DFO_Marine_Bioregions.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0001 <- st_read(paste0(folder, 'DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb'),
                      layer = 'DFO_Marine_Bioregions')

  # -----
  data0001 <- st_transform(data0001, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0001,
           dsn = "./data/data-format/data0001-bioregions.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}

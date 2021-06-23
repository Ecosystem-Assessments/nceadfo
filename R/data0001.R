#' Data 0001 :
#'
#'
#'
#' @keywords
#' @keywords
#' @keywords
#'
#' @source
#'
#' @export
#'
#' @details
#'

get_data0001 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0001-name/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Zostera_Zostere.zip'))) {
    # URL
    zostere_inv <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/Zostera_Zostere.zip',
                     'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/DataDictionary_DictionnaireDonnees.csv')

    # Download
    download.file(zostere_inv[1], destfile = paste0(folder, 'Zostera_Zostere.zip'))
    download.file(zostere_inv[2], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Zostera_Zostere.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0001 <- st_read(paste0(folder, 'Zostera_Zostere.shp'))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0001,
           dsn = "./data/data-format/data0001-zostere.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}

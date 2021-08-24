#' Data 0008 : CanCoast 2.0
#'
#' CanCoast 2.0: data and indices to describe the sensitivity of Canada's marine coasts to changing climate
#'
#' @keywords sea level
#' @keywords climate change
#' @keywords stressors
#'
#' @source https://open.canada.ca/data/en/dataset/73714ed4-a795-a7ae-7e93-36100ce7c242
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0008 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0008-sea_level/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'cancoast_v2.zip'))) {
    # URL
    dat <- c('https://ftp.maps.canada.ca/pub/nrcan_rncan/publications/STPublications_PublicationsST/314/314669/of_8551.zip')

    # Download
    # Default R options limit download time to 60 seconds. Modify for larger files
    oldopt <- options()$timeout
    options(timeout=1000)
    download.file(dat[1], destfile = paste0(folder, 'cancoast_v2.zip'))
    options(timeout=oldopt)


    # Unzip
    unzip(zipfile = paste0(folder, 'cancoast_v2.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0008 <- st_read(paste0(folder, 'CANCOAST_SEALEVELCHANGE_2006_2020_V1/CANCOAST_SEALEVELCHANGE_2006_2020_V1.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0008,
           dsn = "./data/data-format/data0008-sea_level.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}

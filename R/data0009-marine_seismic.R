#' Data 0009 : Canadian National Marine Seismic Data Repository
#'
#' The Geological Survey of Canada (Atlantic and Pacific) has collected marine survey field records on marine expeditions for over 50 years. This release makes available the results of an ongoing effort to scan and convert our inventory of analog marine survey field records (seismic, sidescan and sounder) to digital format.
#'
#' @keywords stressors
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/e1fa0090-4b06-e476-5c71-e2326666a4d0
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0009 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0009-marine_seismic/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'GSC_Seismic_Reflection.gdb.zip'))) {
    # URL
    dat <- c('https://ftp.maps.canada.ca/pub/nrcan_rncan/Seismology_Sismologie/Seismic_Reflection-Imagerie_Sismique/GSC_Seismic_Reflection.gdb.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'GSC_Seismic_Reflection.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'GSC_Seismic_Reflection.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0009 <- st_read(paste0(folder, 'GSC_Seismic_Reflection.gdb'),
                      layer = "GSC_Seismic_Reflection", quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0009,
           dsn = "./data/data-format/data0009-marine_seismic.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}

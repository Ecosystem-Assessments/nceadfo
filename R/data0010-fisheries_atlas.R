#' Data 0010 : Maritimes Region Fisheries Atlas: Catch Weight Landings Mapping (2014–2018)
#'
#' These datasets show commercial fisheries catch weight landings of directed fisheries and bycatch from the Scotian Shelf, the Bay of Fundy, and Georges Bank from NAFO Divisions 4VWX and the Canadian portions of 5Y and 5Z
#'
#' @keywords commercial fisheries
#'
#' @source https://open.canada.ca/data/en/dataset/44ef4d33-20b7-45fc-974c-d73a0a8fbae8
#' @source https://waves-vagues.dfo-mpo.gc.ca/Library/40885690.pdf  
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0010 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0010-fisheries_atlas/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, '40885690.pdf'))) {
    # URL
    dat <- c('https://waves-vagues.dfo-mpo.gc.ca/Library/40885690.pdf',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Maritimes_Fisheries_Atlas_2014_2018/Maritimes_Fisheries_Atlas_2014_2018.gdb.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, '40885690.pdf'))
    download.file(dat[2], destfile = paste0(folder, 'Maritimes_Fisheries_Atlas_2014_2018.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Maritimes_Fisheries_Atlas_2014_2018.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # WARNING: Data not exported as formatted data since we already have fisheries data.
  #          We still deemed it important to include the dataset here so that it is included
  #          in the data catalogue coming from this project.
  message("This dataset is not exported as formatted data since we already have fisheries data and because this dataset contains many layers. We still deemed it important to include the dataset here so that it is included in the data catalogue coming from this project.")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # x <- st_layers(paste0(folder, 'Maritimes_Fisheries_Atlas_2014_2018.gdb'))
  # data0010 <- st_read(paste0(folder, 'Maritimes_Fisheries_Atlas_2014_2018.gdb'),
  #                     layer = "Maritimes_GroundfishBottomTrawl_Landings_Weight_2014_2018",
  #                     quiet = TRUE) %>%
  #             st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # # Output
  # st_write(obj = data0010,
  #          dsn = "./data/data-format/data0010-fisheries_atlas.geojson",
  #          delete_dsn = TRUE)
  # _________________________________________________________________________ #

}

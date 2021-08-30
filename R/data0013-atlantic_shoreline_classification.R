#' Data 0013: Atlantic Shoreline Classification
#'
#' The Atlantic dataset is part of Environment and Climate Change Canada’s Shoreline Classification and Pre-Spill database. Shoreline classification data has been developed for use by the Environmental Emergencies Program of Environment and Climate Change Canada for environmental protection purposes. Marine and estuarine shorelines are classified according to the character (substrate and form) of the upper intertidal (foreshore) or upper swash zone (Sergy, 2008).
#'
#' @keywords
#'
#' @source https://open.canada.ca/data/en/dataset/30449352-2556-42df-9ffe-47ea8e696f91
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0013 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0013-shoreline_classification/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))) {
    # URL
    dat <- c('http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/atlantic-shoreline-classification/How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf',
             'http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/atlantic-shoreline-classification/ShorelineClassification_AR_OpenDataCatalogue.gdb.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))
    download.file(dat[2], destfile = paste0(folder, 'ShorelineClassification_AR_OpenDataCatalogue.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'ShorelineClassification_AR_OpenDataCatalogue.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0013 <- st_read(paste0(folder, 'ShorelineClassification_AR_OpenDataCatalogue.gdb'),
                      layer = "O14Oceans_ShorelineClass_AR")

  # Transform projection
  data0013 <- st_transform(data0013, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0013,
           dsn = "./data/data-format/data0013-shoreline_classification.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}

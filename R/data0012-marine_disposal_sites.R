#' Data 0012 : Marine disposal sites
#'
#' The Active and Inactive Disposal at Sea Sites in Canadian Waters dataset provides spatial and related information of at-sea disposal sites approved for use in Canada in the last ten years and that remain open for consideration for additional use.
#'
#' @keywords
#'
#' @source https://open.canada.ca/data/en/dataset/da99526e-284f-4e06-8d04-193785cd1a96)
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0012 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0012-marine_disposal_sites/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CAN_DAS.gdb.zip'))) {
    # URL
    dat <- c('http://data.ec.gc.ca/data/sites/assess/active-and-inactive-disposal-at-sea-sites-in-canadian-waters/active-and-inactive-disposal-at-sea-sites-2018/CAN_DAS.gdb.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'CAN_DAS.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'CAN_DAS.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0012 <- st_read(paste0(folder, 'CAN_DAS.gdb'),
                      layer = "DAS_Active_Inactive_Sites_2018",
                      quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0012,
           dsn = "./data/data-format/data0012-marine_disposal_sites.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}

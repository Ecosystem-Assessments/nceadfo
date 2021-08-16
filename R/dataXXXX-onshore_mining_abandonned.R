#' Data XXXX : Onshore mining abandonned
#'
#'
#'
#' @keywords
#'
#' @source https://novascotia.ca/natr/meb/download/dp010dds.asp
#'
#' @export
#'
#' @details This function loads and formats the data
#'

# get_data0036 <- function() {
#   # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
#   # Download data
#   # ----------------------------------------
#   # Output folder
#   output <- "data0036-habitat_faunique/"
#   folder <- paste0("./data/data-raw/", output)
#   if (!file.exists(folder)) dir.create(folder)
#
#   # Proceed only if data is not already loaded
#   if (!file.exists(paste0(folder, 'Habitats_fauniques_SHP.zip'))) {
#     # URL
#     dat <- c('https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Faune/Habitats_fauniques/Habitats_fauniques_SHP.zip',
#              'https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Faune/Habitats_fauniques/Metadonnees_FGDB_HAFA.pdf')
#
#     # Download
#     download.file(dat[1], destfile = paste0(folder, 'Habitats_fauniques_SHP.zip'))
#     download.file(dat[2], destfile = paste0(folder, 'Metadonnees_FGDB_HAFA.pdf'))
#
#     # Unzip
#     unzip(zipfile = paste0(folder, 'Habitats_fauniques_SHP.zip'), exdir = folder)
#   }
#   # _________________________________________________________________________ #
#
#   # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
#   # Import data
#   # ----------------------------------------
#   data0036 <- st_read(paste0(folder, 'Habitats_fauniques.shp'), quiet = TRUE) %>%
#               st_transform(crs = global_parameters()$crs)
#   # _________________________________________________________________________ #
#
#   # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
#   # Export data
#   # ----------------------------------------
#   # Output
#   st_write(obj = data0036,
#            dsn = "./data/data-format/data0036-habitat_faunique.geojson",
#            delete_dsn = TRUE)
#   # _________________________________________________________________________ #
#
# }

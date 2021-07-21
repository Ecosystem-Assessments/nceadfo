#' Data XXXX : Fishing atlas - longline and trap
#'
#' Fisheries Atlas: Maritimes Region Inshore Lobster (2012-2014) (WARNING: check, now in the process of being updated to 2015-2019)
#'
#' @keywords
#' @keywords
#' @keywords
#'
#' @source https://open.canada.ca/data/en/dataset/3d2e1a84-20f5-4a61-90e4-94760c80ebb9
#' @source http://publications.gc.ca/collections/collection_2019/mpo-dfo/Fs97-6-3293-eng.pdf  
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_dataXXXX <- function() {
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Download data
  # # ----------------------------------------
  # # Output folder
  # output <- "data0001-bioregions/"
  # folder <- paste0("./data/data-raw/", output)
  # if (!file.exists(folder)) dir.create(folder)
  #
  # if (!file.exists(paste0(output, 'DFO_Marine_Bioregions.zip'))) {
  #   # Download from Open Canada portal
  #   uid <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
  #   library(rgovcan)
  #   govcan_dl_resources(uid, path = folder)
  #
  #   # Unzip file
  #   unzip(zipfile = paste0(folder, 'DFO_Marine_Bioregions.zip'), exdir = folder)
  # }
  # # _________________________________________________________________________ #
  #
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Import data
  # # ----------------------------------------
  # data0001 <- st_read(paste0(folder, 'DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb'),
  #                     layer = 'DFO_Marine_Bioregions') %>%
  #             st_transform(4326)
  # # _________________________________________________________________________ #
}

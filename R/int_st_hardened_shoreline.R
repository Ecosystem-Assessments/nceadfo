#' Hardened shoreline
#'
#' Integrated data for hardened shoreline in the Scotian Shelf bioregion
#'
#' @keywords hardened shoreline
#' @keywords stressors
#'
#' @export
#'
#' @details This function imports formatted data and prepares the data layer that will be used for the cumualtive effects assessment
#'

st_hardened_shoreline <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Method
  # -------------------
  #
  # From Grace and Noreen's shared document
  #
  # > Locations of hardened shoreline structures (riprap, sea walls, groins, jetties,
  #   breakwaters, etc.) with the first ocean pixel adjacent to hardened shoreline
  #   segments classified as "impacted"
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0013") # Marine disposal sites
  load_format("data0002") # Grid

  # -----
  uid <- data0013$SCAT_Class_EN == "Man-Made Solid"
  dat <- data0013[uid, ]

  # -----
  uid  <- st_intersects(dat, data0002) %>%
          unlist() %>%
          unique()

  # -----
  hardened_shoreline <- data0002
  hardened_shoreline$hardened_shoreline <- NA
  hardened_shoreline$hardened_shoreline[uid] <- 1
  hardened_shoreline <- filter(hardened_shoreline, !is.na(hardened_shoreline))
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_hardened_shoreline")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(hardened_shoreline)
  # --------------------------------------------------------------------------------



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_hardened_shoreline.yml")

  # -----
  st_write(obj = hardened_shoreline,
           dsn = "./data/data-integrated/st_hardened_shoreline.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}

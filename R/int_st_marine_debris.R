#' Marine debris
#'
#' Integrated data for marine debris in the Scotian Shelf bioregion
#'
#' @keywords marine disposal sites
#' @keywords marine debris
#' @keywords ocean dumping
#' @keywords stressors
#'
#' @export
#'
#' @details This function imports formatted data and prepares the data layer that will be used for the cumualtive effects assessment
#'

st_marine_debris <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Method
  # -------------------
  #
  # - 2km buffer around sites
        buf <- 2000
  # - Active sites = full intensity score (1). Inactive sites = partial intensity score (0.5).
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0012") # Marine disposal sites
  load_format("data0002") # Grid

  # -----
  dat <- st_buffer(data0012, buf)

  # -----
  active <- dat$Status == "Active"
  inactive <- dat$Status == "Inactive"

  # -----
  dat$marine_debris           <- 0.0
  dat$marine_debris[inactive] <- 0.5
  dat$marine_debris[active]   <- 1.0

  # -----
  marine_debris  <- st_intersection(data0002, dat) %>%
                    group_by(ID) %>%
                    summarise(marine_debris = sum(marine_debris)) %>%
                    st_drop_geometry()

  # -----
  marine_debris <- left_join(data0002, marine_debris, by = "ID") %>%
                   filter(!is.na(marine_debris))

  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_marine_debris")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(marine_debris)

  # -----
  meta$dataDescription$observations$total <- nrow(dat)
  # --------------------------------------------------------------------------------



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_marine_debris.yml")

  # -----
  st_write(obj = marine_debris,
           dsn = "./data/data-integrated/st_marine_debris.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}

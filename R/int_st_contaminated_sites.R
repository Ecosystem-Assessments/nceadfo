#' Contaminated sites
#'
#' Integrated data for contaminated sites in the Scotian Shelf bioregion
#'
#' @keywords inorganic pollution
#' @keywords contaminated sites
#' @keywords stressors
#'
#' @export
#'
#' @details This function imports formatted data and prepares the data layer that will be used for the cumualtive effects assessment
#'

st_contaminated_sites <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Method
  # -------------------
  #
  # - Active sites
  # - Sites within 200m from the coastline
        dist_sites <- 200
  # - 2km buffer around sites
        buf <- 2000
  # - Volume as intensity measurement
  # - Area weighted sum in each study area grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0011") # Contaminated sites
  basemap("aoi") # Area of interest
  load_format("data0002") # Grid

  # -----
  uid <- data0011$Site.Status == "Active"
  dat <- data0011[uid, ]

  # -----
  uid <- st_buffer(aoi, dist_sites) %>%
         st_intersects(dat) %>%
         unlist()
  dat <- dat[uid, ]

  # -----
  dat <- st_buffer(dat, buf)

  # -----
  dat <- select(dat, volume = Estimated.Cubic.Meters.Contaminated) %>%
                filter(!is.na(volume)) %>%
                filter(volume > 0)

  # -----
  contaminated_sites  <- st_intersection(data0002, dat) %>%
                         mutate(area = as.numeric(st_area(.)) * 1e-6,
                                area_prop = area / (pi * buf^2 * 1e-6),
                                intensite = volume * area_prop) %>%
                         group_by(ID) %>%
                         summarise(contaminated_sites = sum(intensite)) %>%
                         st_drop_geometry()

  # -----
  contaminated_sites <- left_join(data0002, contaminated_sites, by = "ID") %>%
                        filter(!is.na(contaminated_sites))

  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_contaminated_sites")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(contaminated_sites)

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
  write_yaml(meta, "./data/data-metadata/int_st_contaminated_sites.yml")

  # -----
  st_write(obj = contaminated_sites,
           dsn = "./data/data-integrated/st_contaminated_sites.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}

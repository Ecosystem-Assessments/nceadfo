#' Predict species distribution
#'
#' @export
make_eDrivers <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                    DESCRIPTION
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data used for the shiny application called `eDrivers`
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  out <- "data/data-eDrivers/"
  chk_create(out)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                IMPORT & FORMAT DATA
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grid
  cellsize <- 1000
  grd <- sf::st_read("data/aoi/aoi.gpkg") |>
         sf::st_transform(3857) |>
         stars::st_rasterize(dx = cellsize, dy = cellsize)
  
  # Get raw stressors 
  rawDrivers <- dir("data/stressors/raw/2019_2021", full.names = TRUE) |>
                lapply(stars::read_stars) |>
                lapply(stars::st_warp, dest = grd) |>
                lapply(function(x) round(x,4))
         
  # Get transformed stressors 
  drivers <- dir("data/stressors/transformed/2019_2021", full.names = TRUE) |>
             lapply(stars::read_stars) |>
             lapply(stars::st_warp, dest = grd) |>
             lapply(function(x) round(x,4))
  
  # Hotspots
  hotspots <- lapply(
    drivers, 
    function(x) {
      dat <- as.data.frame(x)
      dat[,3] <- ifelse(dat[,3] <= 0, NA, dat[,3])
      th <- quantile(dat[,3], probs = .8, na.rm = TRUE)
      dat[,3] <- ifelse(dat[,3] > th, 1, NA)
      dat <- stars::st_as_stars(dat)
      sf::st_crs(dat) <- sf::st_crs(3857)
      dat
    }
  )
  
  
  # Stressor names 
  nm <- lapply(drivers, names) |>
        unlist() |>
        tools::file_path_sans_ext() 

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                   RASTER STACKS
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rawDrivers <- lapply(rawDrivers, as, "Raster")
  drivers <- lapply(drivers, as, "Raster")
  hotspots <- lapply(hotspots, as, "Raster")
  
  # Transform into raster stacks
  rawDrivers <- raster::stack(rawDrivers)
  drivers <- raster::stack(drivers)
  hotspots <- raster::stack(hotspots)

  # Names 
  names(rawDrivers) <- names(drivers) <- names(hotspots) <- nm

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                 MASK RASTER STACKS
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # # Save raster values to memory
  # rawDrivers <- readAll(rawDrivers)
  # drivers <- readAll(drivers)
  # hotspots <- readAll(hotspots)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                   EXPORT STACKS
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export drivers and hotspots list
  save(rawDrivers, file = here::here(out, "rawDrivers.RData"))
  save(drivers, file = here::here(out, "drivers.RData"))
  save(hotspots, file = here::here(out, "hotspots.RData"))


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                  EXPORT MATRICES
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Drivers data as matrix and remove NAs
  library(magrittr)
  library(raster)
  dr <- as.matrix(drivers)
  id0 <- apply(dr, 1, function(x) !all(is.na(x)))
  dr <- dr[id0, ]
  dr <- dr %>%
        '*'(1000) %>%
        round() %>%
        ifelse(. == 0, NA, .) %>%
        as.data.frame()

  save(dr, file = here::here(out,'dr.RData'))

  # Hotspots data as matrix and remove NAs
  hot <- as.matrix(hotspots)
  id0 <- apply(hot, 1, function(x) !all(is.na(x)))
  hot <- hot[id0, ]
  hot <- hot %>%
         ifelse(. == 0, NA, .) %>%
         as.data.frame()
  hot <- as.data.frame(hot)

  save(hot, file = here::here(out,'hot.RData'))

  # Create list with all data 
  drivers_ss <- list(
    rawDrivers = rawDrivers,
    drivers = drivers,
    hotspots = hotspots,
    dr = dr,
    hot = hot
  )
  save(drivers_ss, file = here::here(out, "drivers_ss.RData"))
}
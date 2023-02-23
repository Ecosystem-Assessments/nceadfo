#' Function to export cumulative drivers and species (richness)
#'
#' @export

out_footprint <- function() {
  library(stars)
  # drivers 
  out <- here::here("data","drivers","transformed")
  r <- dir(out, full.names = TRUE)
  r <- lapply(
    r, 
    function(x) {
      dir(x, recursive = TRUE, full.names = TRUE) |>
      lapply(stars::read_stars)
    }
  )
  
  # Cumulative drivers 
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  r <- lapply(r, cumul) |>
       lapply(function(x) x[aoi]) # Mask data
         
  # Export 
  out <- here::here("output","footprint")
  chk_create(out)
  years <- c("2010_2015","2016_2021")
  nm <- glue::glue("cumulative_drivers-{years}.tif")
  for(i in 1:length(r)) {
    stars::write_stars(
      r[[i]],
      dsn = here::here(out, nm[i]),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }

  # Species 
  # NOTE: mask of aoi not done along with SDMs, do it here
  # Get aoi
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  out <- here::here("output","footprint")
  chk_create(out)
  nm <- glue::glue("cumulative_species.tif")

  dat <- c(
    here::here("data","data-biotic","marine_species","random_forest_regression_binary"),
    here::here("data","data-biotic","marine_mammals","binary"),
    here::here("data","data-biotic","sea_birds","binary")
  ) |>
  dir(full.names = TRUE) |>
  lapply(stars::read_stars) |>
  cumul()
  
  dat <- dat[aoi] # Mask data 
  
  stars::write_stars(
    dat,
    dsn = here::here(out, nm),
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

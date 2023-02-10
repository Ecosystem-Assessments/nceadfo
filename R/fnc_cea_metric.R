#' Simple wrapper function to create a raster stack with network cea metric per species
#'
#' @export
cea_metric <- function(path, metric) {
  # Load taxa names
  txNames <- here::here("data","format_modules","txNames.RData")
  load(txNames)

  # List of files
  files <- dir(here::here(path, metric), full.names = TRUE)

  # Import in list
  out <- list()
  for(i in 1:length(files)) out[[i]] <-  raster::raster(files[i])

  # Read all
  out[[i]] <- raster::readAll(out[[i]])

  # Stack
  out <- raster::stack(out)

  # Set names
  names(out) <- txNames

  # Return
  return(out)
}

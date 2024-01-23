#' Script to assess species-scale cumulative effects
#'
#' @export

cea <- function() {
  library(stars)
  # Function to export stars objects
  export_stars <- function(dat, out, n) {
    # Create output
    out <- paste0(out, "/")
    rcea::chk_create(out)
    nm <- names(dat)

    # Export
    for (i in 1:n) {
      stars::write_stars(
        adrop(dat[i]),
        paste0(out, "/", nm[i], ".tif")
      )
    }
  }

  # Load files for analysis
  load("data/FormatData/driversRaster.RData")
  load("data/FormatData/biotic.RData")
  load("data/FormatData/species_sensitivity.RData")
  n <- length(biotic)

  # Specify and create output folder
  per <- names(drivers)
  output <- list(
    here::here("output", "cea_species", per[1]),
    here::here("output", "cea_species", per[2])
  )
  # output <- "~/scratch/output/cea/"
  lapply(output, rcea::chk_create)


  # Cumulative effects assessment
  for (i in seq_len(length(per))) {
    halpern <- rcea::cea(drivers[[i]], biotic, species_sensitivity, "stars")
    export_stars(halpern, output[[i]], n = n)
  }
}

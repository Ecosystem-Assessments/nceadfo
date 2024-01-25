#' Script to assess habitat-scale cumulative effects
#'
#' @export

hcea <- function() {
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
  load("data/FormatData/habitats.RData")
  load("data/FormatData/habitats_sensitivity.RData")
  n <- length(habitats)

  # Specify and create output folder
  per <- names(drivers)
  output <- list(
    here::here("output", "cea_habitats", per[1]),
    here::here("output", "cea_habitats", per[2])
  )
  # output <- "~/scratch/output/cea/"
  lapply(output, rcea::chk_create)


  # Cumulative effects assessment
  for (i in seq_len(length(per))) {
    halpern <- rcea::cea(drivers[[i]], habitats, habitats_sensitivity, "stars")
    export_stars(halpern, output[[i]], n = n)
  }
}

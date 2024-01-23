#' Function to assess difference between assessments in both periods
#'
#' @export

out_difference <- function() {
  library(tidyverse)
  library(magrittr)
  library(raster)
  input <- here::here("output", "cea_full")
  out <- here::here("output", "cea_difference")
  chk_create(out)

  # Evaluate difference between periods
  # CEA
  cea <- dir(input, pattern = "^cea_", full.names = TRUE) |>
    lapply(stars::read_stars)
  diff <- cea[[2]] - cea[[1]]
  stars::write_stars(diff, here::here(out, "cea_species_difference.tif"))

  # NCEA
  # Net
  dat <- dir(input, pattern = "^ncea_2", full.names = TRUE) |>
    lapply(stars::read_stars)
  diff <- dat[[2]] - dat[[1]]
  stars::write_stars(diff, here::here(out, "cea_network_difference.tif"))

  # Direct
  dat <- dir(input, pattern = "^ncea_direct", full.names = TRUE) |>
    lapply(stars::read_stars)
  diff <- dat[[2]] - dat[[1]]
  stars::write_stars(diff, here::here(out, "cea_network_direct_difference.tif"))

  # Indirect
  dat <- dir(input, pattern = "^ncea_indirect", full.names = TRUE) |>
    lapply(stars::read_stars)
  diff <- dat[[2]] - dat[[1]]
  stars::write_stars(diff, here::here(out, "cea_network_indirect_difference.tif"))
}

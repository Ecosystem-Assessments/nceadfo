#' Script to extract full cumulative effects from species and network-scale assessments
#'
#' @export

out_cea_full <- function() {
  # Output
  out <- here::here("output", "cea_full")
  rcea::chk_create(out)
  per <- dir(here::here("output", "cea_species"))
  nper <- length(per)

  for (i in seq_len(nper)) {
    # Species-scale cumulative effects assessment
    cea <- fnc_cumul(here::here("output", "cea_species", per[i]))
    stars::write_stars(cea, here::here(out, glue::glue("cea_{per[i]}.tif")))

    # Habitat-scale cumulative effects assessment
    cea <- fnc_cumul(here::here("output", "cea_habitats", per[i]))
    stars::write_stars(cea, here::here(out, glue::glue("cea_habitats_{per[i]}.tif")))

    # Network-scale cumulative effects assessment - net effets
    ncea_net <- fnc_cumul(here::here("output", "ncea", per[i], "net"))
    stars::write_stars(ncea_net, here::here(out, glue::glue("ncea_{per[i]}.tif")))

    # Network-scale cumulative effects assessment - direct effets
    ncea_direct <- fnc_cumul(here::here("output", "ncea", per[i], "direct"))
    stars::write_stars(ncea_direct, here::here(out, glue::glue("ncea_direct_{per[i]}.tif")))

    # Network-scale cumulative effects assessment - indirect effets from species
    ncea_indirect <- fnc_cumul(here::here("output", "ncea", per[i], "indirect"))
    stars::write_stars(ncea_indirect, here::here(out, glue::glue("ncea_indirect_{per[i]}.tif")))

    # Joint cumulative effects assessment - indirect effets from habitats
    nceahab_indirect <- fnc_cumul(here::here("output", "cea_indirect_habitats", per[i]))
    stars::write_stars(nceahab_indirect, here::here(out, glue::glue("nceahab_indirect_habitats_{per[i]}.tif")))

    # Joint cumulative effects assessment - full effects on species (ncea + indirect_habitats)
    nceahab_species <- fnc_cumul(here::here("output", "nceahab_species", per[i]))
    stars::write_stars(nceahab_species, here::here(out, glue::glue("nceahab_species_{per[i]}.tif")))

    # Joint cumulative effects assessment - full effects on species + effects on habitats (VCs: species & habitats)
    # See `out_nceahab.R`
  }
}

fnc_cumul <- function(folder) {
  # Load empty raster for easy masking
  load(here::here("data", "FormatData", "emptyRaster.RData"))
  r <- r + 1

  # Load and cumul for each species
  library(stars)
  files <- dir(folder, full.names = TRUE)
  dat <- lapply(files, function(x) {
    stars::read_stars(x) |>
      stars::st_apply(c("x", "y"), sum, na.rm = TRUE) |>
      sf::st_set_crs(sf::st_crs(r))
  })

  # Cumul for all species together
  dat <- do.call("c", dat) |>
    stars::st_redimension() |>
    stars::st_apply(c(1, 2), sum, na.rm = TRUE)

  # Set values outside St. Lawrence to NA
  stars::st_dimensions(r) <- stars::st_dimensions(dat)
  dat <- dat * r

  # Return
  dat
}

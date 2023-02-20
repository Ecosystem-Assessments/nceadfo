#' Export figures
#'
#' @export
figures <- function() {
  out <- list()
  out$out <- here::here("figures")
  out$biotic_cnt <- here::here(out$out,"biotic","continuous")
  out$biotic_bin <- here::here(out$out,"biotic","binary")
  out$abiotic <- here::here(out$out, "abiotic")
  out$drivers <- here::here(out$out,"drivers")
  out$cea_species <- here::here(out$out,"cea_species")
  out$cea_network <- here::here(out$out,"cea_network")
  out$cea <- here::here(out$out, "cea")
  out$footprint <- here::here(out$out, "footprint")
  out$exposure <- here::here(out$out, "exposure")
  out$diff <- here::here(out$out, "cea_difference")
  lapply(out, chk_create)
  
  # ---
  plotDat <- function(dat, out, suffix = "", type = "regular") {
    nm <- tools::file_path_sans_ext(names(dat))
    png(
      here::here(out, glue::glue("{nm}{suffix}.png")), 
      res = param$figures$resolution, 
      width = param$figures$width, 
      height = param$figures$height, 
      units = "mm", 
      pointsize = param$figures$pointsize
    )
    if (type == "regular") plot_nceadfo(dat)
    if( type == "dual") plot_nceadfo_dual(dat)
    dev.off()
  }
  
  # Species distribution continuous
  dir(
    c("data/data-biotic/marine_mammals/continuous",
      "data/data-biotic/sea_birds/continuous", 
      "data/data-biotic/marine_species/random_forest_regression_smoothing"), 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$biotic_cnt)

  # Species distribution binary
  dir(
    c("data/data-biotic/marine_mammals/binary",
      "data/data-biotic/sea_birds/binary", 
      "data/data-biotic/marine_species/random_forest_regression_binary"), 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$biotic_bin)

  # Abiotic  
  dir("data/data-abiotic", full.names = TRUE, pattern = ".tif$") |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$abiotic)
  
  # Drivers 
  dr <- here::here("data","cea_modules","drivers")
  per <- dir(dr)
  for(i in 1:length(per)) {
    here::here(dr, per[i]) |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$drivers)
  }

  # Footprint
  dir(here::here("output","footprint"), full.names = TRUE) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$footprint)

  # Exposure
  dir(here::here("output","exposure"), full.names = TRUE) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$exposure)

  # Species-scale cumulative effects assessment - species
  dr <- here::here("output","cea_species")
  per <- dir(dr)
  outsp <- here::here(out$cea_species, per)
  lapply(outsp, chk_create)
  for(i in 1:length(per)) {
    here::here(dr, per[i]) |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, outsp[i])
  }
  
  # Network-scale cumulative effects assessment - species
  dr <- here::here("output","cea_network")
  per <- dir(dr)
  outsp <- here::here(out$cea_network, per)
  lapply(outsp, chk_create)
  for(i in 1:length(per)) {
    here::here(dr, per[i], "cea") |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, outsp[i])
  }
  
  # Species-scale and network-scale cumulative effects assessment
  dr <- here::here("output","cea")
  dir(dr, full.names = TRUE) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$cea)
  
  # Period differences for both assessments
  dr <- here::here("output","cea_difference")
  dir(dr, full.names = TRUE) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$diff, type = "dual")  
}

#' Export figures
#'
#' @export
figures <- function() {
  out <- list()
  out$out <- here::here("figures")
  out$biotic_cnt <- here::here(out$out, "biotic", "continuous")
  out$biotic_bin <- here::here(out$out, "biotic", "binary")
  out$abiotic <- here::here(out$out, "abiotic")
  out$drivers <- here::here(out$out, "drivers")
  out$cea_species <- here::here(out$out, "cea_species")
  out$cea_network <- here::here(out$out, "cea_network")
  out$cea_habitats <- here::here(out$out, "cea_habitats")
  out$nceahab_species <- here::here(out$out, "nceahab_species")
  out$cea <- here::here(out$out, "cea")
  out$footprint <- here::here(out$out, "footprint")
  out$exposure <- here::here(out$out, "exposure")
  out$diff <- here::here(out$out, "cea_difference")
  lapply(out, chk_create)

  # Mask
  data(aoi)
  tmp <- stars::read_stars("data/drivers/transformed/2010_2015/fisheries_intensity.e2b7e6c4.DD.tif")
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi <- stars::st_rasterize(aoi["val_ras"], template = tmp)

  # ---------------------------------------------------------------------------
  plotDat <- function(dat, out, suffix = "", type = "regular", main = NULL, sub = NULL) {
    nm <- tools::file_path_sans_ext(names(dat))
    png(
      here::here(out, glue::glue("{nm}{suffix}.png")),
      res = param$figures$resolution,
      width = param$figures$width,
      height = param$figures$height,
      units = "mm",
      pointsize = param$figures$pointsize
    )

    # Mask
    dat <- dat * aoi

    # Plot
    if (type == "regular") plot_nceadfo(dat, mainTitle = main, subTitle = sub)
    if (type == "dual") plot_nceadfo_dual(dat, mainTitle = main, subTitle = sub)
    dev.off()
  }

  # ---------------------------------------------------------------------------
  # Species distribution continuous
  dir(
    c(
      "data/data-biotic/marine_mammals/continuous",
      "data/data-biotic/sea_birds/continuous",
      "data/data-biotic/marine_species/random_forest_regression_smoothing"
    ),
    full.names = TRUE
  ) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$biotic_cnt, sub = "Occurrence probability")

  # ---------------------------------------------------------------------------
  # Species distribution binary
  dat <- dir(
    c(
      "data/data-biotic/marine_mammals/binary",
      "data/data-biotic/sea_birds/binary",
      "data/data-biotic/marine_species/random_forest_regression_binary"
    ),
    full.names = TRUE
  ) |>
    lapply(stars::read_stars)
  nm <- lapply(dat, names) |> unlist()
  dat <- lapply(dat, function(x) setNames(x, "vals")) |>
    lapply(function(x) dplyr::mutate(x, vals = ifelse(vals == 0, NA, vals)))
  for (i in 1:length(dat)) names(dat[[i]]) <- nm[i]
  names(dat) <- nm
  lapply(dat, plotDat, out$biotic_bin)

  # ---------------------------------------------------------------------------
  # Abiotic
  dat <- dir("data/data-abiotic", full.names = TRUE, pattern = ".tif$")
  nm <- tools::file_path_sans_ext(basename(dat)) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("\\.", " ") |>
    stringr::str_to_sentence()
  dat <- lapply(dat, stars::read_stars)
  for (i in 1:length(dat)) plotDat(dat[[i]], out$abiotic, sub = nm[i])
  # lapply(plotDat, out$abiotic)

  # ---------------------------------------------------------------------------
  # Drivers
  dr <- here::here("data", "cea_modules", "drivers")
  per <- dir(dr)
  for (i in 1:length(per)) {
    here::here(dr, per[i]) |>
      dir(full.names = TRUE, pattern = ".tif$") |>
      lapply(stars::read_stars) |>
      lapply(plotDat, out$drivers, sub = "Normalized intensity")
  }

  # ---------------------------------------------------------------------------
  # Cumulative stressors
  dir(here::here("output", "footprint"), pattern = "drivers", full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$footprint, sub = "Cumulative drivers")

  # ---------------------------------------------------------------------------
  # Cumulative stressors
  dir(here::here("output", "footprint"), pattern = "species", full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$footprint, sub = "Species richness")

  # ---------------------------------------------------------------------------
  # Exposure
  dir(here::here("output", "exposure"), full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$exposure, sub = "Cumulative exposure")

  # ---------------------------------------------------------------------------
  # Species-scale cumulative effects assessment - species
  dr <- here::here("output", "cea_species")
  per <- dir(dr)
  outsp <- here::here(out$cea_species, per)
  lapply(outsp, chk_create)
  for (i in 1:length(per)) {
    dat <- here::here(dr, per[i]) |>
      dir(full.names = TRUE, pattern = ".tif$") |>
      lapply(stars::read_stars)
    nm <- lapply(dat, names) |>
      unlist() |>
      tools::file_path_sans_ext()
    dat <- lapply(dat, function(x) {
      stars::st_redimension(x) |>
        stars::st_apply(c(1, 2), sum, na.rm = TRUE)
    })
    for (j in seq_len(length(dat))) {
      sf::st_crs(dat[[j]]) <- sf::st_crs(aoi)
      dat[[j]] <- stars::st_set_dimensions(dat[[j]], which = 1, point = FALSE)
      dat[[j]] <- stars::st_set_dimensions(dat[[j]], which = 2, point = FALSE)
      names(dat[[j]]) <- nm[j]
    }
    lapply(dat, plotDat, outsp[i], sub = "Cumulative effects score")
  }

  # ---------------------------------------------------------------------------
  # Network-scale cumulative effects assessment - network
  dr <- here::here("output", "ncea")
  per <- dir(dr)
  outsp <- here::here(out$cea_network, per)
  lapply(outsp, chk_create)
  for (i in 1:length(per)) {
    dat <- here::here(dr, per[i], "net") |>
      dir(full.names = TRUE, pattern = ".tif$") |>
      lapply(stars::read_stars)
    nm <- lapply(dat, names) |>
      unlist() |>
      tools::file_path_sans_ext()
    dat <- lapply(dat, function(x) {
      stars::st_redimension(x) |>
        stars::st_apply(c(1, 2), sum, na.rm = TRUE)
    })
    for (j in seq_len(length(dat))) names(dat[[j]]) <- nm[j]
    lapply(dat, plotDat, outsp[i], sub = "Cumulative effects score")
  }

  # ---------------------------------------------------------------------------
  # Habitat-scale cumulative effects assessment - habitats
  dr <- here::here("output", "cea_habitats")
  per <- dir(dr)
  outsp <- here::here(out$cea_habitats, per)
  lapply(outsp, chk_create)
  for (i in 1:length(per)) {
    dat <- here::here(dr, per[i]) |>
      dir(full.names = TRUE, pattern = ".tif$") |>
      lapply(stars::read_stars)
    nm <- lapply(dat, names) |>
      unlist() |>
      tools::file_path_sans_ext()
    dat <- lapply(dat, function(x) {
      stars::st_redimension(x) |>
        stars::st_apply(c(1, 2), sum, na.rm = TRUE)
    })
    for (j in seq_len(length(dat))) {
      sf::st_crs(dat[[j]]) <- sf::st_crs(aoi)
      dat[[j]] <- stars::st_set_dimensions(dat[[j]], which = 1, point = FALSE)
      dat[[j]] <- stars::st_set_dimensions(dat[[j]], which = 2, point = FALSE)
      names(dat[[j]]) <- nm[j]
    }
    lapply(dat, plotDat, outsp[i], sub = "Cumulative effects score")
  }

  # ---------------------------------------------------------------------------
  # NCEAHAB species
  dr <- here::here("output", "nceahab_species")
  per <- dir(dr)
  outsp <- here::here(out$nceahab_species, per)
  lapply(outsp, chk_create)

  for (i in 1:length(per)) {
    here::here(dr, per[i]) |>
      dir(full.names = TRUE, pattern = ".tif$") |>
      lapply(stars::read_stars) |>
      lapply(plotDat, outsp[i], sub = "Cumulative effects score")
  }

  # ---------------------------------------------------------------------------
  # Species-scale and network-scale cumulative effects assessment
  dr <- here::here("output", "cea_full")
  dir(dr, full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$cea, sub = "Cumulative effects score")

  # ---------------------------------------------------------------------------
  # Period differences for both assessments
  dr <- here::here("output", "cea_difference")
  dat <- dir(dr, full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$diff, type = "dual", sub = "Cumulative effects change")
}

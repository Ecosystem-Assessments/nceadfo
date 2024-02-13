#' Script to combine effects to species and to habiats into a single spatial assessment of cumulative effects
#'
#' @export

out_nceahab <- function() {
  # Output
  per <- dir(here::here("output", "cea_species"))
  nper <- length(per)

  # Inputs
  input <- list()
  input$cea <- here::here("output", "cea_habitats", per)
  input$nceahab_species <- here::here("output", "nceahab_species", per)

  for (i in seq_len(nper)) {
    # --------------------------------------------------------
    # Effects to habitats
    habitats <- here::here(input$cea[i]) |>
      fnc_cumul2()

    # Normalize between 0 and 1
    maxVal <- lapply(habitats, function(x) max(x, na.rm = TRUE)) |>
      unlist() |>
      max()
    habitats <- habitats / maxVal

    # --------------------------------------------------------
    # Effects to species
    species <- here::here(input$nceahab_species[i]) |>
      fnc_cumul2()

    # Normalize between 0 and 1
    maxVal <- lapply(species, function(x) max(x, na.rm = TRUE)) |>
      unlist() |>
      max()
    species <- species / maxVal

    # --------------------------------------------------------
    # Combine to get cumultive effets to species that considers:
    # For species:
    #   Direct effects of stressors
    #   Indirect effects of stressors through species interactions
    #   Indirect effects of stressors through habitats
    # For habitats:
    #   Direct effects of stressors
    #
    # 2 versions:
    #   1. Normalize effects at the scale of individual VCs
    #     - Each species and habitat normalized between 0 and 1
    #     - Many more species than habitats.
    #     - Species should drive spatial distribution of effects
    #   2. Normalize effects at the scale of VC groups, i.e. intra-group normalization
    #     - Sum of species and habitats normalized between 0 and 1, respectively
    #     - Control for the imbalance between the number of species and habitats
    x <- merge(species) |>
      stars::st_apply(c(1, 2), sum, na.rm = TRUE)
    y <- merge(habitats) |>
      stars::st_apply(c(1, 2), sum, na.rm = TRUE)
    stars::st_dimensions(x) <- stars::st_dimensions(y)

    # 1st version: No normalization
    nceahab <- x + y

    # 2nd version: Intra-group normalization
    nceahab_norm <- (x / max(x[[1]], na.rm = TRUE)) + (y / max(y[[1]], na.rm = TRUE))

    # Mask on area of interest
    load(here::here("data", "FormatData", "emptyRaster.RData"))
    aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
    r[[1]][] <- NA
    aoi$val_ras <- 1
    aoi <- sf::st_transform(aoi, crs = sf::st_crs(r))
    aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = r)
    stars::st_dimensions(aoi_mask) <- stars::st_dimensions(nceahab)
    nceahab <- nceahab * aoi_mask
    nceahab_norm <- nceahab_norm * aoi_mask

    # Export
    out <- here::here("output", "cea_full")
    rcea::chk_create(out)
    stars::write_stars(nceahab, here::here(out, glue::glue("nceahab_{per[i]}.tif")))
    stars::write_stars(nceahab_norm, here::here(out, glue::glue("nceahab_normalized_{per[i]}.tif")))
  }
}

fnc_cumul2 <- function(folder) {
  # Mask on area of interest
  load(here::here("data", "FormatData", "emptyRaster.RData"))
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  r[[1]][] <- NA
  aoi$val_ras <- 1
  aoi <- sf::st_transform(aoi, crs = sf::st_crs(r))
  aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = r)

  # Load and cumul for each species
  library(stars)
  files <- dir(folder, full.names = TRUE)
  nm <- basename(files) |>
    tools::file_path_sans_ext()
  dat <- lapply(files, function(x) {
    stars::read_stars(x) |>
      stars::st_apply(c("x", "y"), sum, na.rm = TRUE) |>
      sf::st_set_crs(sf::st_crs(r))
  })
  dat <- do.call("c", dat)
  names(dat) <- nm

  # Set values outside St. Lawrence to NA
  stars::st_dimensions(aoi_mask) <- stars::st_dimensions(dat)
  dat <- dat * aoi_mask

  # Return
  dat
}

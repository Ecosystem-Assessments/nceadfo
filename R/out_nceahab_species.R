#' Script to assess effects to species that consider direct effects from stressors, indirect effects from species interactions, and indirect effects from habitats
#'
#' @export

out_nceahab_species <- function() {
  # Output
  per <- dir(here::here("output", "cea_species"))
  nper <- length(per)
  out <- here::here("output", "nceahab_species", per)
  lapply(out, rcea::chk_create)

  # Inputs
  input <- list()
  input$ncea <- here::here("output", "ncea", per, "net")
  input$nceahab_indirect <- here::here("output", "cea_indirect_habitats", per)

  for (i in seq_len(nper)) {
    # --------------------------------------------------------
    # Net effects per species
    net <- here::here(input$ncea[i]) |>
      fnc_cumul2()

    # Normalize between 0 and 2
    maxVal <- lapply(net, function(x) max(x, na.rm = TRUE)) |>
      unlist() |>
      max()
    net <- (net / maxVal) * 2

    # --------------------------------------------------------
    # Indirect effects to species arising from habitats
    hab <- here::here(input$nceahab_indirect[i]) |>
      fnc_cumul2()

    # Normalize between 0 and 1
    maxVal <- lapply(hab, function(x) max(x, na.rm = TRUE)) |>
      unlist() |>
      max()
    hab <- hab / maxVal

    # --------------------------------------------------------
    # Combine to get cumultive effets to species that considers:
    # Direct effects of stressors
    # Indirect effects of stressors through species interactions
    # Indirect effects of stressors through habitats
    nceahab_species <- net + hab

    # Export by species
    nm <- names(nceahab_species)
    for (j in seq_len(length(nceahab_species))) {
      stars::write_stars(
        adrop(nceahab_species[j]),
        here::here(out[i], glue::glue("{nm[j]}.tif")),
        quiet = TRUE
      )
    }
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

#' Assessing indirect effects of environmental stressors to individual species arising from the direct effects of stressors to habitats and species habitat use:
#'
#' @param biotic rasters of binary species distribution
#' @param habitats rasters of binary habitat distribution
#' @param habitats_cea rasters of cumulative effects to habitats (Halpern method)
#' @param potential_habitats matrix [species, habitats] with potential habitat use identified as 0 and 1. This is done to consider the vertical use within the water column.
#'
#' @export

# DEMAIN
# Apply on all species
# Make sure that habitats and species are all there and that they are in the same order (stopifnot)
# Export individual raster per species, with habitats as bands

out_indirect_species_habitats_ <- function() {
  out_indirect_species_habitats("2010_2015")
  out_indirect_species_habitats("2016_2021")
}

out_indirect_species_habitats <- function(per = "2010_2015") {
  # Biotic data
  load(here::here("data", "FormatData", "biotic.RData"))

  # Habitats
  load(here::here("data", "FormatData", "Habitats.RData"))

  # Mask on area of interest
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  tmp <- habitats[1]
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi <- sf::st_transform(aoi, crs = sf::st_crs(tmp))
  aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = tmp)

  # Habitats cumulative effects assessment using Halpern methodology
  input <- here::here("output", "cea_habitats", per) |>
    dir(full.names = TRUE)
  habitats_cea <- lapply(input, function(x) {
    dat <- stars::read_stars(x) |>
      rcea::cea_extract(cumul_fun = "vc")
    stars::st_dimensions(dat) <- stars::st_dimensions(aoi_mask)
    names(dat) <- tools::file_path_sans_ext(basename(x))
    dat * aoi_mask
  })
  habitats_cea <- do.call("c", habitats_cea)

  # Matrix of potential habitat use by species
  potential_hab <- read.csv(
    here::here("data", "cea_modules", "species_habitats", "species_habitats_potential.csv")
  ) |>
    dplyr::arrange(species)
  rownames(potential_hab) <- potential_hab$species
  potential_hab <- dplyr::select(potential_hab, -species)

  # Make sure that all species and habitats are in the corresponding objects and similarly ordered
  # Species
  potential_hab <- potential_hab[names(biotic), ]
  stopifnot(all(rownames(potential_hab) == names(biotic)))

  # Habitats
  stopifnot(all(names(habitats) == names(habitats_cea)))
  stopifnot(all(names(habitats) == colnames(potential_hab)))

  # Assess indirect effects
  indirect <- cea_full <- list()
  for (i in seq_len(length(biotic))) {
    tmp <- fnc_indirect_species_habitats(
      biotic = biotic[i],
      habitats = habitats,
      habitats_cea = habitats_cea,
      potential_habitats = potential_hab[i, ]
    )
    indirect[[i]] <- tmp[[1]]
    # cea_full[[i]] <- tmp[[2]] # For full assessment, not necessary here
  }

  # Export
  out <- here::here("output", "cea_indirect_habitats", per)
  chk_create(out)
  lapply(indirect, function(x) {
    stars::write_stars(
      x,
      dsn = here::here(out, glue::glue("{names(x)}.tif")),
    )
  })
}

fnc_indirect_species_habitats <- function(biotic, habitats, habitats_cea, potential_habitats) {
  # Rasters to data.frame
  bio <- as.data.frame(biotic)
  hab <- as.data.frame(habitats)
  cea <- as.data.frame(habitats_cea)
  pot <- as.matrix(potential_habitats)

  # Remove coordinates
  xy <- bio[, c("x", "y")]
  bio <- dplyr::select(bio, -x, -y)
  hab <- dplyr::select(hab, -x, -y)
  cea <- dplyr::select(cea, -x, -y)

  # Remove effects to habitats that are not used by focal species
  cea <- sweep(cea, 2, pot[1, ], `*`)
  cea <- cbind(xy, cea) |>
    stars::st_as_stars()
  stars::st_dimensions(cea) <- stars::st_dimensions(habitats)

  # Number of potential habitats
  hab <- sweep(hab, 2, pot[1, ], `*`) |>
    rowSums(na.rm = TRUE)
  hab <- cbind(xy, hab) |>
    stars::st_as_stars()
  stars::st_dimensions(hab) <- stars::st_dimensions(habitats)

  # Divide effects by number of potential habitats
  cea <- cea / hab

  # Multiply by species presence
  cea <- cea * biotic

  # # Combine for full indirect effects to species arising to direct effects to habitats
  # cea_full <- rcea::cea_extract(cea, cumul_fun = "full")
  # stars::st_dimensions(cea_full) <- stars::st_dimensions(aoi_mask)
  # cea_full <- cea_full * aoi_mask

  # Merge habitats into band
  cea <- merge(cea)
  names(cea) <- names(biotic)

  # Return
  list(
    cea # ,
    # cea_full
  )
}

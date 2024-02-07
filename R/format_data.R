#' Script to format data in preparation for the assessment
#'
#' @param assessment name of assessment, one of "original", "nceamm_pam", "nceamm_wsdb", "nceamm_pam_wsdb" (temporary)
#'
#' @export

format_data <- function(assessment = "original") {
  if (assessment == "original") {
    out <- here::here("data", "FormatData")
  } else {
    out <- here::here("data", glue::glue("FormatData_{assessment}"))
  }
  rcea::chk_create(out)
  modules <- here::here("data", "cea_modules")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Species list
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  if (assessment == "original") {
    spn <- "species_list.csv"
  } else {
    spn <- "species_list_nceamm.csv"
  }
  sp <- vroom::vroom(here::here(modules, spn)) |>
    dplyr::rename(species = scientific_name) |>
    dplyr::arrange(species) |>
    dplyr::mutate(file = glue::glue("{shortname}-{aphiaID}.tif"))
  save(sp, file = here::here(out, "SpeciesList.RData"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Simulated trophic sensitivities
  # (Adapted from Beauchesne et al. in review hopefully)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  sensitivity <- vroom::vroom(here::here(modules, "trophic_sensitivity", "trophic_sensitivity.csv"))
  trophic_sensitivity <- sensitivity
  save(trophic_sensitivity, file = here::here(out, "TrophicSensitivity.RData"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Biotic data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  spn <- glue::glue("species_{assessment}") |> as.character()
  biotic <- here::here(modules, spn, sp$file) |>
    lapply(raster::raster)

  # Check if some distributions are empty
  # It should not happen, but if it does I want to catch it and remove the species from the list
  uid <- lapply(biotic, function(x) {
    all(raster::values(x) == 0, na.rm = TRUE)
  }) |>
    unlist()
  biotic <- biotic[!uid]

  # Update species list
  sp <- sp[!uid, ]
  save(sp, file = here::here(out, "SpeciesList.RData"))

  # Create raster stack
  biotic <- raster::stack(biotic)
  names(biotic) <- sp$shortname

  # Taxa names
  txNames <- names(biotic) |>
    tools::file_path_sans_ext()

  # Transform data to matrix
  bioticData <- raster::as.matrix(biotic)
  colnames(bioticData) <- txNames

  # Identify cells with biotic data
  # idBiotic <- apply(bioticData, 1, function(x) !all(is.na(x)))
  # WARNING: Do this only with the marine species data
  marinesp <- read.csv(here::here("data", "data-biotic", "marine_species", "species_list.csv"))
  uid <- tools::file_path_sans_ext(sp$file) %in% marinesp$shortname
  idBiotic <- apply(bioticData[, sp$shortname[uid]], 1, function(x) !all(is.na(x)))

  # Select only relevant data
  bt <- bioticData[idBiotic, ]

  # Add phytoplankton and zooplankton everywhere
  message("WARNING: We are assuming that phytoplankton and zooplankton are everywhere in the study area. A better assessment with proper data would be desirable.")
  bt <- cbind(bt, zooplankton = 1, phytoplankton = 1)

  # Transform biotic data as logical
  bt <- apply(bt, 2, as.logical)

  # Empty raster
  r <- biotic[[1]]
  names(r) <- ""
  raster::values(r)[!is.na(raster::values(r))] <- 0

  # Transform rasters as stars objects
  r <- stars::st_as_stars(r)
  biotic <- stars::st_as_stars(biotic) |>
    split()

  # Export
  save(txNames, file = here::here(out, "txNames.RData"))
  save(bt, file = here::here(out, "bt.RData"))
  save(biotic, file = here::here(out, "biotic.RData"))
  save(r, file = here::here(out, "emptyRaster.RData"))
  save(idBiotic, file = here::here(out, "idBiotic.RData"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Metaweb
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  metaweb <- vroom::vroom(
    here::here(modules, "metaweb", "metaweb.csv")
  ) |>
    as.data.frame()
  rownames(metaweb) <- metaweb[, 1]
  metaweb <- metaweb[, -1]
  stopifnot(all(colnames(metaweb) == rownames(metaweb)))

  # Change species names to match species list
  cn <- colnames(metaweb)
  cn <- gsub(" ", "_", cn) |>
    tolower()
  colnames(metaweb) <- rownames(metaweb) <- cn

  # # All species in biotic data must be in metaweb
  stopifnot(all(colnames(bt) %in% cn))

  # Diag = 0 (no cannibalism)
  diag(metaweb) <- 0

  # Take only species in biotic dataset
  # uid <- colnames(metaweb) %in% colnames(bt)
  uid <- lapply(colnames(bt), function(x) which(x == colnames(metaweb))) |> unlist()
  metaweb <- metaweb[uid, uid]
  stopifnot(all(colnames(bt) == colnames(metaweb)))

  # Export
  save(metaweb, file = here::here(out, "metaweb.RData"))
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Species-specific vulnerabilities
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  species_sensitivity <- vroom::vroom(
    here::here(modules, "species_sensitivity", "species_sensitivity.csv")
  ) |>
    dplyr::mutate(species = gsub(" ", "_", species)) |>
    dplyr::mutate(species = tolower(species)) |>
    dplyr::arrange(species) |>
    as.data.frame()
  rownames(species_sensitivity) <- species_sensitivity$species
  species_sensitivity <- dplyr::select(species_sensitivity, -species)
  stopifnot(all(sp$shortname %in% rownames(species_sensitivity)))

  # Make vulnerability to population density the same as that of direct human impact. One is coastal, the other is modelling at pourpoints along the coast, but both for population density
  # Also change SBT name because there is no distinction between positive and negative in this project
  species_sensitivity <- dplyr::mutate(
    species_sensitivity,
    PopulationDensity = DirectHumanImpact,
  )

  # Add phytoplankton and zooplankton with no vulnerability
  message("WARNING: We are assuming that phytoplankton and zooplankton are insensitive to all drivers. A better assessment with proper data would be desirable.")
  species_sensitivity <- rbind(species_sensitivity, zooplankton = 0, phytoplankton = 0)

  # Take only species in biotic dataset
  uid <- rownames(species_sensitivity) %in% colnames(bt)
  species_sensitivity <- species_sensitivity[uid, ]
  stopifnot(all(colnames(bt) == rownames(species_sensitivity)))

  # Export
  # Saved on next step to select only drivers for which we have data
  # save(species_sensitivity, file = here::here(out,'species_sensitivity.RData'))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Drivers data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  drivers <- here::here(modules, "drivers") |>
    dir(full.names = TRUE) |>
    lapply(dir, full.names = TRUE, recursive = TRUE)
  for (i in 1:length(drivers)) drivers[[i]] <- lapply(drivers[[i]], raster::raster)
  drivers <- lapply(drivers, raster::stack)

  # Periods
  per <- here::here(modules, "drivers") |>
    dir()
  names(drivers) <- per

  # Remove periods from driver names
  nm <- stringr::str_split(names(drivers[[1]]), "\\.") |>
    lapply(function(x) x[1]) |>
    unlist()
  for (i in 1:length(drivers)) names(drivers[[i]]) <- nm

  # Only keep drivers for which we have vulnerability assessment
  uid <- which(names(drivers[[1]]) %in% colnames(species_sensitivity))
  for (i in 1:length(drivers)) drivers[[i]] <- drivers[[i]][[names(drivers[[i]])[uid]]]

  # Sort drivers
  nm <- sort(names(drivers[[1]]))
  for (i in 1:length(drivers)) drivers[[i]] <- drivers[[i]][[nm]]

  # Transform data to matrix
  driversData <- lapply(drivers, raster::as.matrix)

  # Select only relevant data
  # Locations with information on taxa distribution
  dr <- driversData
  for (i in 1:length(dr)) dr[[i]] <- dr[[i]][idBiotic, ]

  # All NAs to 0 (for calculations)
  for (i in 1:length(dr)) dr[[i]][is.na(dr[[i]])] <- 0

  # Make sure that the order of the vulnerability db column is the same as
  # the order of the columns in the drivers db
  stopifnot(all(nm %in% colnames(species_sensitivity)))
  species_sensitivity <- species_sensitivity[, nm]
  stopifnot(all(colnames(species_sensitivity) == nm))

  # Transform as stars object
  drivers <- lapply(
    drivers,
    function(x) {
      stars::st_as_stars(x) |>
        split()
    }
  )
  names(drivers) <- per

  # Export
  save(dr, file = here::here(out, "drivers.RData"))
  save(drivers, file = here::here(out, "driversRaster.RData"))
  save(species_sensitivity, file = here::here(out, "species_sensitivity.RData"))


  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # # Evaluate metaweb motif triplets
  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # # Triads
  # triads <- motif_census_triplet(metaweb)
  #
  # # Select only motifs of interest
  # motifs <- c('exploitative competition','linear chain','apparent competition','omnivory')
  # uid <- triads$name_uni %in% motifs
  # triads <- triads[uid, ]
  #
  # # Modify index for positions i,j,k (currently starts at 0)
  # triads$i <- triads$i + 1
  # triads$j <- triads$j + 1
  # triads$k <- triads$k + 1
  #
  # # Modify position numbers
  # # Species:
  # #   1: exploitative competition bottom
  # #   2: exploitative competition top
  # #   3: linear chains bottom
  # #   4: linear chains middle
  # #   5: linear chains top
  # #   9: apparent competition bottom
  # #  10: apparent competition top
  # #  11: omnivory bottom
  # #  12: omnivory middle
  # #  13: omnivory top
  #
  # mid <- data.frame(motifcensus = c(1,2,3,4,5,9,10,11,12,13),
  #                   id = c(1,3,1,2,3,1,3,1,2,3))
  #
  # # Import in the table
  # triads <- triads %>%
  #           left_join(., mid, by = c("pos_i" = "motifcensus")) %>%
  #           rename(pi = id) %>%
  #           left_join(., mid, by = c("pos_j" = "motifcensus")) %>%
  #           rename(pj = id) %>%
  #           left_join(., mid, by = c("pos_k" = "motifcensus")) %>%
  #           rename(pk = id)
  #
  # # Transform duplicated positions
  # triads$pj[triads$pj == triads$pi] <- 2
  # triads$pj[triads$pj == triads$pk] <- 2
  # triads$pk[triads$pk == triads$pi] <- 2
  #
  # # Export
  # save(triads, file = './Data/FormatData/triads.RData')
}

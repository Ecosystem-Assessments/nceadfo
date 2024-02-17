#' Predict species metaweb
#'
#' @export
make_metaweb <- function() {
  path <- here::here("data", "data-metaweb")
  tmp <- file.exists(here::here(path, "metaweb.csv"))
  if (!tmp) {
    # Load required data
    dat <- importdat(c("d8094d1b", "893b37e8", "7c150fc3", "7a5323bb"))

    # Format and combine interactions catalogs
    ## St. Lawrence original
    stl_sp <- dat[["species_interactions_catalog-d8094d1b.csv"]] |>
      dplyr::select(taxon, taxonomy)

    stl_int <- dat[["species_interactions_catalog-d8094d1b.csv"]] |>
      dplyr::select(taxon, consumer, resource)

    df_int <- function(dat, field) {
      temp <- lapply(dat[, "consumer"], function(x) stringr::str_split(x, " \\| "))
      for (i in 1:length(temp)) {
        temp[[i]] <- data.frame(
          V1 = dat$taxon[i],
          V2 = temp[[i]][[1]]
        )
      }
      temp <- purrr::discard(temp, function(x) all(x == "")) |>
        dplyr::bind_rows() |>
        dplyr::filter(V2 != "")
    }

    stl_preyof <- df_int(stl_int, "consumer") |> dplyr::rename(predator = V2, prey = V1)
    stl_predof <- df_int(stl_int, "resource") |> dplyr::rename(predator = V1, prey = V2)
    stl_int <- dplyr::bind_rows(stl_predof, stl_preyof)

    ## Atlantic update
    atl_int <- dat[["rglobi_atlantic-7a5323bb-interactions.csv"]]
    atl_sp <- dat[["rglobi_atlantic-7a5323bb-species.csv"]] |>
      dplyr::select(-aphiaID, -ScientificName) |>
      dplyr::rename(taxon = species)
    taxonomy <- dplyr::select(atl_sp, Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # Column with taxonomy separated by "|"
    taxo <- function(tx) {
      as.character(tx) |>
        paste(collapse = " | ")
    }
    atl_sp <- dplyr::select(atl_sp, taxon) |>
      dplyr::mutate(taxonomy = apply(taxonomy, 1, taxo))

    # ------
    # Combine species
    # Remove stl taxa from atl
    atl_sp <- atl_sp[!atl_sp$taxon %in% stl_sp$taxon, ]
    species <- dplyr::bind_rows(stl_sp, atl_sp) |>
      dplyr::distinct() |>
      dplyr::arrange(taxon)

    # Combine interactions
    int <- dplyr::bind_rows(stl_int, atl_int) |>
      dplyr::distinct()
    res <- dplyr::group_by(int, predator) |>
      dplyr::summarize(resource = paste0(prey, collapse = " | "))
    con <- dplyr::group_by(int, prey) |>
      dplyr::summarize(consumer = paste0(predator, collapse = " | "))

    # Catalogue
    S0_catalog <- species |>
      dplyr::left_join(res, by = c("taxon" = "predator")) |>
      dplyr::left_join(con, by = c("taxon" = "prey"))

    # Last bit of clean up: remove species that have no consumer or resource
    iid <- apply(S0_catalog[, c("consumer", "resource")], 1, function(x) all(is.na(x)))
    S0_catalog <- S0_catalog[!iid, ]

    # Change NAs for ""
    S0_catalog <- S0_catalog |>
      dplyr::mutate(consumer = ifelse(is.na(consumer), "", consumer)) |>
      dplyr::mutate(resource = ifelse(is.na(resource), "", resource))

    # ------------------------------------------------------------
    # Prepare species lists
    species <- dat[["species_list_nw_atlantic-893b37e8.csv"]] |>
      dplyr::rename(taxon = SPEC) |>
      dplyr::select(-Freq, -ScientificName)
    mmb <- dat[["species_list_marine_mammals_birds-7c150fc3.csv"]] |>
      dplyr::rename(taxon = ScientificName) |>
      dplyr::mutate(Species = taxon)
    species <- dplyr::bind_rows(species, mmb) |>
      unique()
    taxonomy <- dplyr::select(species, Kingdom, Phylum, Class, Order, Family, Genus, Species)

    # Column with taxonomy separated by "|"
    taxo <- function(tx) {
      as.character(tx) |>
        paste(collapse = " | ")
    }
    species$taxonomy <- apply(taxonomy, 1, taxo)

    # Phyto & Zoo
    load("./data/data-metaweb/TaxonomyPhytoplankton.RData")
    load("./data/data-metaweb/TaxonomyZooplankton.RData")
    taxoPhyto <- taxoPhyto[!taxoPhyto$taxon %in% species$taxon, ]
    taxoZoo <- taxoZoo[!taxoZoo$taxon %in% species$taxon, ]
    species <- dplyr::bind_rows(species, taxoPhyto, taxoZoo)


    # INSERT MISSING TAXA IN CATALOGUE
    # Missing taxa
    uid <- !species$taxon %in% S0_catalog[, "taxon"]
    S0_add <- data.frame(
      taxon = species$taxon[uid],
      taxonomy = species$taxonomy[uid],
      resource = "",
      consumer = "",
      row.names = species$taxon[uid],
      stringsAsFactors = F
    ) |>
      as.matrix()

    # Add to catalogue and create full
    S0 <- rbind(S0_catalog, S0_add)
    rownames(S0) <- S0$taxon

    # Weight values for 2-way similarity measurements
    wt <- 0.5

    # 1st is for similarity measured from set of resources and taxonomy, for consumers
    similarity.consumers <- iEat::similarity_taxon(S0 = S0, wt = wt, taxa = "consumer")

    # 2nd is for similarity measured from set of consumers and taxonomy, for resources
    similarity.resources <- iEat::similarity_taxon(S0 = S0, wt = wt, taxa = "resource")

    # Export similarity
    path <- here::here("data", "data-metaweb")
    if (!file.exists(path)) dir.create(path)
    save(similarity.consumers, file = here::here(path, "Similarity_consumers.RData"))
    save(similarity.resources, file = here::here(path, "Similarity_resources.RData"))

    # PREDICT BINARY INTERACTIONS
    S0 <- dplyr::select(S0, taxon, resource, consumer) |>
      dplyr::rename(target = resource, source = consumer)

    library(magrittr)
    metaweb <- iEat::iEat(
      S0 = S0,
      S1 = species$taxon,
      # S2 = species$taxon,
      sourceSim = similarity.consumers,
      targetSim = similarity.resources,
      K = 7,
      minSim = 0.3,
      minWt = 1,
      predict = "full algorithm"
    )

    # CREATE FOOD WEB MATRIX
    metaweb <- iEat::iEat_to_foodWeb(metaweb)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # 5. COMBINE phytoplankton and zooplankton species
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    #         Phytoplankton
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Taxa
    phyto <- taxoPhyto$taxon

    # Consumers
    consPhyto <- colSums(metaweb[phyto, ])

    # Resources (theoretically this should all be 0s)
    resPhyto <- rowSums(metaweb[, phyto])

    # Binaries
    consPhyto <- ifelse(consPhyto > 0, 1, 0)
    resPhyto <- ifelse(resPhyto > 0, 1, 0)

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    #          Zooplankton
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Taxa
    zoo <- taxoZoo$taxon

    # Consumers
    consZoo <- colSums(metaweb[zoo, ])

    # Resources (theoretically this should all be 0s)
    resZoo <- rowSums(metaweb[, zoo])

    # Binaries
    consZoo <- ifelse(consZoo > 0, 1, 0)
    resZoo <- ifelse(resZoo > 0, 1, 0)

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    #        Add to metaweb
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Taxa names
    sp <- colnames(metaweb)
    sp <- c(sp, "Zooplankton", "Phytoplankton")

    # Add phytoplankton and zooplankton
    metaweb <- cbind(metaweb, resZoo, resPhyto)
    metaweb <- rbind(metaweb, c(consZoo, 0, 0), c(consPhyto, 1, 0))

    # Change colnames
    colnames(metaweb) <- rownames(metaweb) <- sp

    # Remove phytoplankton and zooplankton taxa
    uid <- !sp %in% c(phyto, zoo)
    metaweb <- metaweb[uid, uid]

    # Export
    utils::write.csv(metaweb, here::here(path, "metaweb.csv"))

    # Export in modules as well
    out <- here::here("data", "cea_modules", "metaweb")
    chk_create(out)
    file.copy(here::here(path, "metaweb.csv"), here::here(out, "metaweb.csv"), overwrite = TRUE)
  }

  # Update for NCEAMM 2024
  # Removing impossible interactions with marine mammals
  # Original metaweb used for assessment
  metaweb <- vroom::vroom(here::here(path, "metaweb.csv"))

  # Get taxonomy of all species in the metaweb
  input <- here::here("data", "data-integrated")
  sp <- vroom::vroom(
    here::here(input, "/species_list_marine_mammals_birds-7c150fc3/species_list_marine_mammals_birds-7c150fc3.csv")
  ) |>
    dplyr::filter(Class == "Mammalia")

  # Metaweb:
  #   column = consumer
  #   row = resource
  # Whale species should not have 1 in their respective rows, unless its another Mammalia (for simplicity here)
  # This means that rows == Mammalia and columns != Mammalia should all be = 0
  message("In `make_metaweb.R`, we are removing all instances of marine mammals being the resource of another taxon unless it is a taxon member of the Mammalia class. This is done for simplicity, as potential predators of marine mammals are not included in the assessment. If they were (e.g. humans, sharks), this part of the code should be adjusted.")

  # Identify marine mammals from metaweb
  # Rows with Mammalia: this is where marine mammals are considered resources
  uid_rows <- which(data.frame(metaweb)[, 1] %in% sp$ScientificName)

  # Columns without Mammalia: this is where species other than Mammalia are considered consumers
  uid_cols <- which(!colnames(metaweb) %in% sp$ScientificName)[-1]

  # Set to 0
  metaweb[uid_rows, uid_cols] <- 0

  # Export
  utils::write.csv(metaweb, here::here(path, "metaweb_update.csv"))

  # Export in modules as well
  out <- here::here("data", "cea_modules", "metaweb")
  chk_create(out)
  file.copy(here::here(path, "metaweb_nceamm2024.csv"), here::here(out, "metaweb_nceamm2024.csv"), overwrite = TRUE)
}

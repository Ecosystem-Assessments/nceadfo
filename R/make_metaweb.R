#' Predict species metaweb
#'
#' @export
make_metaweb <- function() {
  # Load interactions catalog & species list
  dat <- importdat(c("d8094d1b", "893b37e8"))
  S0_catalog <- dat[["species_interactions_catalog-d8094d1b.csv"]] |>
    dplyr::rename(
      nonconsumer = non.consumer,
      nonresource = non.resource
    )
  species <- dat[["species_list_nw_atlantic-893b37e8.csv"]] |>
    dplyr::rename(taxon = SPEC)
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
    nonresource = "",
    consumer = "",
    nonconsumer = "",
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
}

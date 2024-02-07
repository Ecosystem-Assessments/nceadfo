#' Evaluate overlap of species and habitat distributions
#'
#' @export
make_overlap <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load spatial data
  library(raster)
  input <- here::here("data", "cea_modules")
  habitats <- dir(here::here(input, "habitats"), pattern = ".tif", full.names = TRUE) |>
    lapply(raster::raster) |>
    raster::stack() |>
    as.data.frame()
  biotic <- dir(here::here(input, "species"), pattern = ".tif", full.names = TRUE) |>
    lapply(raster::raster) |>
    raster::stack() |>
    as.data.frame()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Traits data
  env <- here::here("data", "data-raw", "species_traits-c86f711c", "species_traits-c86f711c.csv") |>
    vroom::vroom() |>
    dplyr::mutate(bathydemersal = as.numeric(as.logical(benthic) + as.logical(demersal))) |>
    dplyr::select(species, bathydemersal, pelagic)

  # Join with species
  sp <- vroom::vroom(here::here(input, "species_list.csv")) |>
    dplyr::select(shortname, aphiaID, scientific_name) |>
    dplyr::left_join(env, by = c("scientific_name" = "species")) |>
    dplyr::rename(bathydemersal_sp = bathydemersal, pelagic_sp = pelagic) |>
    dplyr::mutate(species = glue::glue("{shortname}.{aphiaID}"))
  stopifnot(all(sp$scientific_name %in% env$species))
  stopifnot(all(sp$species %in% names(biotic)))
  sp <- dplyr::select(sp, species, bathydemersal_sp, pelagic_sp)

  # Classify habitats
  classify <- data.frame(
    habitats = c(
      "BITDL",
      "MFITDL",
      "RITDL",
      "SSHLW",
      "HSHLW",
      "MSHLW",
      "SSHLF",
      "HSHLF",
      "MSHLF",
      "CYN",
      "SDEEP",
      "HDEEP",
      "MDEEP",
      "SALTMARSH",
      "SEAG",
      "ALGAL",
      "KELP",
      "HMUSSEL",
      "DPBIO",
      "SPELAGIC",
      "DPELAGIC"
    ),
    bathydemersal_hab = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0),
    pelagic_hab = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1)
  )

  # Habitats
  hab <- vroom::vroom(here::here(input, "habitats_list.csv")) |>
    # dplyr::select(HabitatCODE, Habitat) |> # For habitat short names
    dplyr::select(habitats = HabitatCODE) |>
    dplyr::left_join(classify, by = "habitats")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Overlap
  dat <- expand.grid(names(habitats), names(biotic), stringsAsFactors = FALSE) |>
    dplyr::rename(habitats = Var1, species = Var2) |>
    dplyr::mutate(overlap = 0)
  for (i in seq_len(nrow(dat))) {
    dat$overlap[i] <- sum(
      habitats[, dat$habitats[i]] == 1 &
        biotic[, dat$species[i]] == 1,
      na.rm = TRUE
    )
  }

  # Add environment traits for species and habitats and set overlap value to 0 if no correspondance
  dat <- dplyr::left_join(dat, hab, by = "habitats") |>
    dplyr::left_join(sp, by = "species") |>
    dplyr::mutate(
      environment =
        (bathydemersal_sp == 1 & bathydemersal_hab == 1) |
          (pelagic_sp == 1 & pelagic_hab == 1),
      overlap = overlap * environment
    ) |>
    dplyr::select(habitats, species, overlap)




  # Percent overlap?
  # WARNING: species may be in more than one habitat per cell
  # WARNING: it would be relevant to consider habitat use in the water column, because at the moment benthic species will be considered in pelagic habitats
  dat <- dat |>
    dplyr::group_by(species) |>
    dplyr::mutate(percent_overlap = overlap / max(overlap)) |>
    dplyr::ungroup()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export
  modules <- here::here("data", "cea_modules", "species_habitats_overlap")
  chk_create(modules)
  write.csv(dat, file = here::here(modules, "species_habitats_overlap.csv"), row.names = FALSE)
}

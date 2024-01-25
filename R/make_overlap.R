#' Evaluate overlap of species and habitat distributions
#'
#' @export
make_overlap <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load data
  input <- here::here("data", "cea_modules")
  habitats <- dir(here::here(input, "habitats"), pattern = ".tif", full.names = TRUE) |>
    lapply(raster::raster) |>
    raster::stack() |>
    as.data.frame()
  biotic <- dir(here::here(input, "species"), pattern = ".tif", full.names = TRUE) |>
    lapply(raster::raster) |>
    raster::stack() |>
    as.data.frame()

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

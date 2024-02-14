#' Script to extract cumulative effects scores / km2 for each species from the species and network-scale assessments
#'
#' @export

out_cea_km2 <- function() {
  # Output
  out <- here::here("output", "cea_km2")
  rcea::chk_create(out)
  per <- dir(here::here("output", "cea_species"))
  nper <- length(per)

  # Function to assess area of species and habitat distributions
  area_units <- function(dat) {
    size <- sf::st_area(dat)
    size[[1]] <- set_units(size[[1]], km^2)
    dat <- dat * size
    df <- as.data.frame(dat) |>
      dplyr::select(-x, -y) |>
      colSums(na.rm = TRUE) |>
      units::set_units(km^2)
    data.frame(
      name = names(df),
      area_km2 = as.numeric(df),
      row.names = NULL
    )
  }

  # Function to assess cea per km2
  ceakm2 <- function(cea_input, area) {
    files <- dir(cea_input, full.names = TRUE)
    nm <- basename(files) |>
      stringr::str_replace(".tif", "")

    dat <- lapply(files, function(x) {
      stars::read_stars(x) |>
        split() |>
        as.data.frame() |>
        dplyr::select(-x, -y) |>
        colSums(na.rm = TRUE)
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(name = nm)

    # Join together and divide by area
    cekm <- dplyr::left_join(area, dat, by = "name") |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .x / area_km2)) |>
      dplyr::select(-area_km2)
  }

  # Species and habitats area
  load(here::here("data", "FormatData", "biotic.RData"))
  load(here::here("data", "FormatData", "habitats.RData"))
  biotic <- mask_aoi(biotic) |> area_units()
  habitats <- mask_aoi(habitats) |> area_units()

  # Effects per km^2
  for (i in seq_len(nper)) {
    # Species-scale assessment
    ceakm2(
      cea_input = here::here("output", "cea_species", per[i]),
      area = biotic
    ) |>
      write.csv(
        file = here::here(out, glue::glue("cea_km2_{per[i]}.csv")),
        row.names = FALSE
      )

    # Habitat-scale assessment
    ceakm2(
      cea_input = here::here("output", "cea_habitats", per[i]),
      area = habitats
    ) |>
      write.csv(
        file = here::here(out, glue::glue("cea_habitats_km2_{per[i]}.csv")),
        row.names = FALSE
      )

    # Indirect effects to species arising from habitats
    ceakm2(
      cea_input = here::here("output", "cea_indirect_habitats", per[i]),
      area = biotic
    ) |>
      write.csv(
        file = here::here(out, glue::glue("cea_indirect_habitats_km2_{per[i]}.csv")),
        row.names = FALSE
      )
  }

  # NOTE: The network-scale assessment performed with `rcea::ncea` already provides these results.
  #       We only need to reformat and export them
  for (i in seq_len(nper)) {
    folder <- here::here("output", "ncea", per[i], "cekm") # WARNING: Rename once folder name is ok
    files <- dir(folder, full.names = TRUE)
    net <- stringr::str_detect(files, "_net.csv")
    direct <- stringr::str_detect(files, "_direct.csv")
    indirect <- stringr::str_detect(files, "_indirect.csv")

    # Load & retransform in cea/km2, as it is currently in degrees
    loaddat <- function(uid) {
      lapply(files[uid], read.csv) |>
        data.table::rbindlist() |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ .x * 0.01)
        ) |>
        dplyr::rename(name = vc)
    }
    net <- loaddat(net)
    direct <- loaddat(direct)
    indirect <- loaddat(indirect)

    # Export
    write.csv(net, file = here::here(out, glue::glue("ncea_km2_{per[i]}.csv")), row.names = FALSE)
    write.csv(direct, file = here::here(out, glue::glue("ncea_direct_km2_{per[i]}.csv")), row.names = FALSE)
    write.csv(indirect, file = here::here(out, glue::glue("ncea_indirect_km2_{per[i]}.csv")), row.names = FALSE)
  }
}

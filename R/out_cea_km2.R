#' Script to extract cumulative effects scores / km2 for each species from the species and network-scale assessments
#'
#' @export

out_cea_km2 <- function() {
  # Output
  out <- here::here("output", "cea_km2")
  rcea::chk_create(out)
  per <- dir(here::here("output", "cea_species"))
  nper <- length(per)

  # Species distributions
  load(here::here("data", "FormatData", "bt.RData"))
  hr <- data.frame(
    species = colnames(bt),
    area = colSums(bt, na.rm = TRUE)
  )
  rownames(hr) <- NULL

  # Species-scale cumulative effects assessment
  for (i in seq_len(nper)) {
    folder <- here::here("output", "cea_species", per[i])
    files <- dir(folder, full.names = TRUE)
    nm <- basename(files) |>
      stringr::str_replace(".tif", "")

    dat <- lapply(files, function(x) {
      stars::read_stars(x) |>
        split() |>
        as.data.frame() |>
        dplyr::select(-x, -y) |>
        # colSums(na.rm = TRUE)
        colSums(na.rm = TRUE)
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(species = nm)

    # Join together and divide by area
    cekm <- dplyr::left_join(hr, dat, by = "species") |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .x / area)) |>
      dplyr::select(-area)

    # Export
    write.csv(cekm, file = here::here(out, glue::glue("cea_km2_{per[i]}.csv")), row.names = FALSE)
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
        dplyr::rename(species = vc)
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

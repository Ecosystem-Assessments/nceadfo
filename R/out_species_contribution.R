#' Script to extract overall species contribution to indirect effects
#'
#' @export

out_species_contribution <- function() {
  # Output
  out <- here::here("output", "cea_species_contribution")
  rcea::chk_create(out)

  # Get species list from parameters
  spList <- read.csv(here::here("data", "cea_modules", "species_list.csv"))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Species contributions to indirect effect for a single period
  folder <- here::here("output", "ncea", "2016_2021", "species_contribution")
  files <- dir(folder, full.names = TRUE)
  dat <- lapply(files, read.csv) |>
    data.table::rbindlist() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ .x * 0.01),
      dplyr::across(dplyr::where(is.numeric), ~ round(.x * 1e6, 8))
    ) |>
    dplyr::mutate(
      vc_from = tools::file_path_sans_ext(vc_from),
      vc = tools::file_path_sans_ext(vc)
    )

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Network format species_contribution
  meta <- dat |>
    dplyr::rowwise() |>
    dplyr::mutate(contribution = sum(
      dplyr::across(!vc_from & !vc),
      na.rm = TRUE
    )) |>
    dplyr::select(vc, vc_from, contribution) |>
    tidyr::pivot_wider(names_from = "vc_from", values_from = "contribution", values_fill = 0) |>
    dplyr::arrange(vc)

  # As matrix
  meta <- meta[, meta$vc]
  meta <- as.matrix(meta)

  ## Insert missing species
  # Add missing species to network
  uid <- !spList$shortname %in% colnames(meta)
  miss <- matrix(
    data = 0,
    ncol = sum(uid),
    nrow = nrow(meta),
    dimnames = list(c(), spList$shortname[uid])
  )
  meta <- cbind(meta, miss)
  for (i in 1:ncol(miss)) meta <- rbind(meta, 0)
  rownames(meta) <- colnames(meta)
  nm <- sort(colnames(meta))
  meta <- meta[nm, nm]

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Species overal contribution to indirect effects
  sp_all <- dat |>
    dplyr::group_by(vc_from) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(Total = rowSums(dplyr::across(dplyr::where(is.numeric)))) |>
    dplyr::arrange(dplyr::desc(Total))

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
  # Export
  write.csv(dat, file = here::here(out, "species_contribution.csv"), row.names = FALSE)
  write.csv(sp_all, file = here::here(out, "species_contribution_total.csv"), row.names = FALSE)
  write.csv(meta, file = here::here(out, "species_contribution_network.csv"), row.names = FALSE)
}

#' @eval get_name("a5b97616")
#'
#' @eval get_description("a5b97616")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: a5b97616
#'
#' @examples
#' \dontrun{
#' dp_a5b97616()
#' }
dp_a5b97616 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "a5b97616"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # Data is local only
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Unzip raw data
    archive::archive_extract(
      here::here(path, "raw", "whaleSights_tif.zip"),
      dir = here::here(path, "raw")
    )

    # Load rasters
    dat <- here::here(path, "raw", "sights") |>
      dir(pattern = "\\.tif", full.names = TRUE) |>
      lapply(masterload)

    # Remove
    unlink(here::here(path, "raw", "sights"), recursive = TRUE)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # WARNING: In order for filters to work, names of column should be:
    #             year      = year
    #             longitude = longitude
    #             latitude  = latitude
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Adjust species names
    name <- data.frame(
      name = tools::file_path_sans_ext(unlist(lapply(dat, names)))
    )

    modif <- data.frame(
      name = c(
        "DOLPHIN-ATLANTIC WHITE-SIDED_sights_KDE",
        "DOLPHIN-COMMON_sights_KDE",
        "DOLPHIN-WHITE-BEAKED_sights_KDE",
        "PORPOISE-HARBOUR_sights_KDE",
        "SEAL-GREY_sights_KDE",
        "WHALE-BLUE_sights_KDE",
        "WHALE-FIN_sights_KDE",
        "WHALE-HUMPBACK_sights_KDE",
        "WHALE-KILLER_sights_KDE",
        "WHALE-LONG-FINNED PILOT_sights_KDE",
        "WHALE-MINKE_sights_KDE",
        "WHALE-SEI_sights_KDE",
        "WHALE-SPERM_sights_KDE"
      ),
      to = c(
        "lagenorhynchus_acutus-137100",
        "delphinus_delphis-137094",
        "lagenorhynchus_albirostris-137101",
        "phocoena_phocoena-137117",
        "halichoerus_grypus-137080",
        "balaenoptera_musculus-137090",
        "balaenoptera_physalus-137091",
        "megaptera_novaeangliae-137092",
        "orcinus_orca-137102",
        "globicephala_melas-137097",
        "balaenoptera_acutorostrata-137087",
        "balaenoptera_borealis-137088",
        "physeter_macrocephalus-137119"
      )
    )
    name <- dplyr::left_join(name, modif, by = "name")
    stopifnot(!any(is.na(name$modif)))
    for (i in seq_len(length(dat))) names(dat[[i]]) <- name$to[i]

    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = timestamp(),
      data_bbox = sf::st_bbox(dat[[1]]) # ,
      # data_timespan = sort(unique(dat$year))
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS SPECIFIED BY USER
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # on.exit(sf::sf_use_s2(TRUE), add = TRUE)
    # sf::sf_use_s2(FALSE)
    dat <- lapply(dat, dp_parameters, bbox = bbox, bbox_crs = bbox_crs)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    name <- lapply(dat, names) |>
      tools::file_path_sans_ext() |>
      unlist()
    fm <- here::here(path, glue::glue("{nm}-{name}"))
    for (i in 1:length(fm)) masterwrite(dat[[i]], fm[i])

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}

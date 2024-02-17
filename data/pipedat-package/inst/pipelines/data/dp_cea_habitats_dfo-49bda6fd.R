#' @eval get_name("49bda6fd")
#'
#' @eval get_description("49bda6fd")
#'
#' @eval dp_params()
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 49bda6fd
#'
#' @examples
#' \dontrun{
#' dp_49bda6fd()
#' }
dp_49bda6fd <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "49bda6fd"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # DOWNLOAD DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!exist$raw) {
    # Data must be added manually as we are not using the publicly available data for this pipeline
  }
  # _________________________________________________________________________________________ #

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw <- here::here(path, "raw")
    out <- here::here(raw, "files")
    archive::archive_extract(
      archive = here::here(raw, "CE_Input_Data.zip"),
      dir = out
    )

    # Habitat data
    hab <- c("bh", "sm", "eg", "al", "kp", "hm", "db", "sp", "dp") |>
      lapply(function(x) {
        sf::st_read(
          here::here(out, "CE_InputData_ForDavidB.gdb"),
          layer = x,
          quiet = TRUE
        ) |>
          sf::st_cast("MULTIPOLYGON")
      }) |>
      dplyr::bind_rows() |>
      dplyr::group_by(HabitatCODE) |>
      dplyr::group_split() |>
      lapply(function(x) {
        name <- unique(x$HabitatCODE)
        sf::st_union(x) |>
          st_as_sf() |>
          dplyr::mutate(HabitatCODE = name)
      }) |>
      dplyr::bind_rows()

    # Stressor data table
    stressors <- sf::st_read(
      here::here(out, "CE_InputData_ForDavidB.gdb"),
      layer = "master_stressor_table",
      quiet = TRUE
    )

    # Habitat sensitivity data table
    vsscores <- sf::st_read(
      here::here(out, "CE_InputData_ForDavidB.gdb"),
      layer = "vscores_habitats",
      quiet = TRUE
    )

    # Fishing severity data table
    fishsev <- sf::st_read(
      here::here(out, "CE_InputData_ForDavidB.gdb"),
      layer = "fishing_severity",
      quiet = TRUE
    )

    # Habitat list
    hab_list <- readxl::read_excel(
      here::here(out, "HabitatLayerMaritimesSummary_April2023.xlsx")
    ) |>
      dplyr::select(HabitatCODE, Habitat, Definition)

    # Delete files from disk
    unlink(out, recursive = TRUE)
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
      data_bbox = sf::st_bbox(hab)
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue::glue("{nm}"))
    sf::st_write(hab, glue::glue("{fm}-habitats.gpkg"), quiet = TRUE)
    masterwrite(stressors, glue::glue("{fm}-stressors"))
    masterwrite(vsscores, glue::glue("{fm}-habitat_sensitivity"))
    masterwrite(fishsev, glue::glue("{fm}-fishing_severity"))
    masterwrite(hab_list, glue::glue("{fm}-habitat_list"))

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}

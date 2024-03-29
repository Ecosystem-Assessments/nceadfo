#' @eval get_name("99bb2d51")
#'
#' @eval get_description("99bb2d51")
#'
#' @eval dp_params()
#' @eval di_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname integration_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 99bb2d51
#'
#' @examples
#' \dontrun{
#' di_99bb2d51()
#' }
di_99bb2d51 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, grid = NULL, ...) {
  # Output folders and other objects used
  uid <- "99bb2d51"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid)
  path <- make_output(uid)

  if (!exist$integrated) {
    # WARNING: For R CMD CHECK
    DAUID <- DGUID <- Geographic.code <- Population..2016 <-
      Population.and.dwelling.counts..5...Population..2021..1. <-
      area <- area2 <- direct_human_impact <- population <-
      propArea <- propPop <- NULL
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    raw_id <- get_rawid(uid) # String with data to import
    pipedat(raw_id, bbox, bbox_crs, timespan)
    dat <- importdat(raw_id)
    bound2016 <- dat[["census_boundary_2016-c676dc2b.geojson"]]
    bound2021 <- dat[["census_boundary_2021-b9024b04.geojson"]]
    pop2016 <- dat[["census_population_2016-d147406d.csv"]]
    pop2021 <- dat[["census_population_2021-d96dec16.csv"]]
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # ANALYZE / FORMAT DATA
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Transform data in meters
    bound2016 <- sf::st_transform(bound2016, crs = 32198)
    bound2021 <- sf::st_transform(bound2021, crs = 32198)
    aoi <- sf::st_read("data/aoi/aoi.gpkg") 
    grid_poly <- sf::st_make_grid(aoi, cellsize = 0.01, crs = 4326)
    grid_poly <- grid_poly[aoi] |>
                 sf::st_sf() |>
                 dplyr::mutate(uid = 1:dplyr::n()) |>
                 sf::st_transform(crs = 32198) 

    # Select only population data
    pop2016 <- dplyr::select(
      pop2016,
      DAUID = Geographic.code,
      population = Population..2016
    ) |>
      dplyr::mutate(DAUID = as.character(DAUID))

    pop2021 <- dplyr::select(
      pop2021,
      DGUID,
      population = Population.and.dwelling.counts..5...Population..2021..1.
    )

    # Join population data with dissemination area geometries
    bound2016 <- dplyr::left_join(bound2016, pop2016, by = "DAUID") |>
      dplyr::select(population)
    bound2021 <- dplyr::left_join(bound2021, pop2021, by = "DGUID") |>
      dplyr::select(population)

    # Select grid cells that are less than 2km from the coast and apply a 10km buffer
    # Use dissemination area geometries as coastline
    grid_buffer <- function(bound, grid_poly) {
      iid <- sf::st_buffer(bound, 2000) |>
        sf::st_intersects(grid_poly) |>
        unlist() |>
        unique() |>
        sort()
      sf::st_buffer(grid_poly[iid, ], 10000)
    }
    coast2016 <- grid_buffer(bound2016, grid_poly)
    coast2021 <- grid_buffer(bound2021, grid_poly)

    # Select only dissemination areas that intersect with buffered grid cells
    bound_subset <- function(coast, bound) {
      iid <- sf::st_intersects(coast, bound) |>
        unlist() |>
        unique() |>
        sort()
      bound[iid, ]
    }
    bound2016 <- bound_subset(coast2016, bound2016)
    bound2021 <- bound_subset(coast2021, bound2021)

    # Intersection of each grid cell with overlapping dissemination areas and
    # recalculating the population as a function of the proportional area of
    # the dissimation area overlapping with grid cell
    prop_population <- function(coast, bound) {
      bound <- dplyr::mutate(bound, area = sf::st_area(bound)) |>
        sf::st_intersection(coast)

      dplyr::mutate(
        bound,
        area2 = sf::st_area(bound),
        propArea = area2 / area,
        propPop = as.numeric(population * propArea)
      ) |>
        sf::st_set_geometry(NULL) |>
        dplyr::select(uid, propPop) |>
        dplyr::group_by(uid) |>
        dplyr::summarise(direct_human_impact = sum(propPop)) |>
        dplyr::filter(direct_human_impact > 0)
    }
    dhi <- list()
    dhi[[1]] <- prop_population(coast2016, bound2016)
    dhi[[2]] <- prop_population(coast2021, bound2021)

    # -----
    dat <- lapply(  
      dhi, 
      function(x) {
        dplyr::left_join(grid_poly, x, by = "uid") |>
        dplyr::select(-uid) |>
        stars::st_rasterize() |>
        masteringrid()
      }
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "integration",
      pipeline_id = uid,
      integration_data = raw_id
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- here::here(path, glue::glue("{nm}-{c(2016,2021)}"))
    for (i in 1:length(fm)) masterwrite(dat[[i]], fm[i])

    # Metadata & bibtex
    mt <- here::here(path, nm)
    masterwrite(meta, mt)
    masterwrite(bib, mt)
    # _________________________________________________________________________________________ #
  }
}

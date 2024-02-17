#' Prepare metadata for STAC catalog
#'
#' @export
make_stac <- function() {
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # WARNING:
  #   This script constitutes a bit of a break in reproducibility.
  #   The whole script could be run in the repo https://github.com/inSilecoInc/nceammStack
  #   However, I want this compendium to be able to prepare everything for the application.
  #   In that sense, the script is located here and the resulting `json` files will be copied in nceammStack.
  #   An additional script in nceammStack could be prepared to fetch this script and run it there instead.
  #   For the time being, let't keep it like this.
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # -------------------------------------------------------------------------------------------------------------------
  # Create collections
  auth <- dir(pattern = "pof-stac-insileco-")
  googleCloudStorageR::gcs_auth(auth)

  # Credentials for GDAL (driver vsigs_streaming)
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = auth)

  # Bucket list
  bucket <- "scotian-shelf-cogs-private"
  assets_objects <- googleCloudStorageR::gcs_list_objects(bucket = bucket)

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # WARNING:
  #   Creating STAC only for marine mammals
  #   Remove this code to create STAC with all species in the assessment

  # Species list
  mm <- here::here("data", "data-biotic", "marine_mammals_pam_wsdb", "binary") |>
    dir() |>
    tools::file_path_sans_ext() |>
    stringr::str_replace("-", "\\.") |>
    tools::file_path_sans_ext()

  # Also include all_species assessment
  mm <- c(mm, "all_species")

  # Add extensions and paths (precautionary for filtering, likely unnecessary)
  mm <- glue::glue("{mm}_cog.tif") |>
    as.character()

  # Filter assests_objects
  uid <- lapply(mm, \(x) which(stringr::str_detect(assets_objects$name, x))) |>
    unlist() |>
    sort()
  assets_objects <- assets_objects[uid, ]
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  assets_objects <- tidyr::separate(
    data = assets_objects,
    col = name,
    into = c("collection", "species"),
    extra = "drop",
    sep = "[/]",
    remove = FALSE
  ) |>
    dplyr::mutate(species = sub("_cog.tif", "", species))

  # -------------------------------------------------------------------------------------------------------------------
  # Detect empty rasters to remove them from the collection
  future::plan("multicore", workers = 10)
  assets_objects$not_empty <- furrr::future_map(
    assets_objects$name,
    \(p) {
      terra::rast(paste0("/vsigs_streaming/", bucket, "/", p)) |>
        sum() |>
        terra::values(na.rm = TRUE) |>
        nrow() > 0
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    unlist()

  # Keep all assets not empty
  assets_filled <- dplyr::filter(assets_objects, not_empty)

  # -------------------------------------------------------------------------------------------------------------------
  # Get footprint for each item
  get_footprint <- function(gcs_rasters_path) {
    r <- gcs_rasters_path |>
      (\(p) {
        paste0("/vsigs_streaming/", bucket, "/", p)
      })() |>
      terra::rast() |>
      sum()

    return(list(
      geometry = terra::as.polygons(r > 0) |>
        sf::st_as_sf() |>
        sf::st_convex_hull() |>
        sf::st_union() |>
        geojsonsf::sfc_geojson() |>
        jsonlite::fromJSON(),
      bbox = sf::st_bbox(r) |> as.numeric()
    ))
  }

  footprints <- furrr::future_map(
    assets_filled$name, get_footprint,
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )

  # -------------------------------------------------------------------------------------------------------------------
  # Create and write items JSON
  # Create item JSON
  items <- assets_filled |>
    purrr::pmap(
      ~ with(list(...), {
        list(
          stac_version = "1.0.0",
          type = "Feature",
          id = species,
          collection = collection,
          properties = list(
            datetime = format(Sys.time(), "%Y-%m-%d"),
            start_datetime = NA,
            end_datetime = NA
          ),
          assets = list(list(
            href =  paste0("/vsigs_streaming/", bucket, "/", name),
            type = "image/tiff; application=geotiff; profile=cloud-optimized",
            role = c("data")
          )) |> setNames(species),
          links = list(
            list(
              rel = "root",
              href = "/",
              type = "application/json"
            ),
            list(
              rel = "parent",
              href = "./collection.json",
              type = "application/json"
            ),
            list(
              rel = "self",
              href = paste0("./", collection, "/", species, ".json"),
              type = "application/json"
            ),
            list(
              rel = "collection",
              href = "./collection.json",
              type = "application/json"
            )
          )
        )
      }),
      .progress = TRUE
    ) |>
    # Merge spatial footprint
    purrr::list_merge(!!!footprints)

  # -------------------------------------------------------------------------------------------------------------------
  # Prepare `collection.json` for NCEAMM outputs
  collections_objects <- assets_filled |>
    dplyr::select(collection, species) |>
    dplyr::filter(collection != "distribution") |>
    dplyr::nest_by(collection)

  stressors_collections <- collections_objects |>
    purrr::pmap(
      ~ with(list(...), {
        list(
          id = collection,
          title = glue::glue("{stringr::str_to_title(collection)} effects of stressors on marine species"),
          stac_version = "1.0.0",
          type = "Collection",
          description = glue::glue("This catalog contains outputs of {collection} effects of stressors on several marine species of the Scotian Shelf."),
          license = "proprietary",
          keywords = c("cumulative effect", "scotian shelf", glue::glue("{collection} effect"), "marine species"),
          providers = list(
            list(
              name = "inSileco Inc.",
              roles = c("producer"),
              url = "https://insileco.io"
            )
          ),
          extent = list(
            spatial = list(
              bbox = list(
                c(-71.23955, 45.14545, -55.80289, 52.64670)
              )
            ),
            temporal = list(
              interval = list(
                c(
                  "2024-01-01T00:00:00Z",
                  "2024-01-01T00:00:00Z"
                )
              )
            )
          ),
          links = list(
            list(
              rel = "root",
              href = "./collection.json",
              type = "application/json"
            ),
            list(
              rel = "self",
              href = "./collection.json",
              type = "application/json"
            ),
            list(
              rel = "parent",
              href = "../",
              type = "application/json"
            )
          ) |> append(purrr::map(data$species, \(sp) {
            list(
              rel = "item",
              href = paste0("./", collection, "/", sp),
              type = "application/json"
            )
          }))
        )
      }),
      .progress = TRUE
    )

  # -------------------------------------------------------------------------------------------------------------------
  # Prepare `collection.json` for species distributions
  collections_objects <- assets_filled |>
    dplyr::select(collection, species) |>
    dplyr::filter(collection == "distribution") |>
    dplyr::nest_by(collection)

  distribution_collections <- collections_objects |>
    purrr::pmap(
      ~ with(list(...), {
        list(
          id = collection,
          title = glue::glue("Species {collection}"),
          stac_version = "1.0.0",
          type = "Collection",
          description = glue::glue("This catalog contains outputs of species {collection} on several marine species."),
          license = "proprietary",
          keywords = c(glue::glue("{collection}"), "marine species"),
          providers = list(
            list(
              name = "inSileco Inc.",
              roles = c("producer"),
              url = "https://insileco.io"
            )
          ),
          extent = list(
            spatial = list(
              bbox = list(
                c(-71.23955, 45.14545, -55.80289, 52.64670)
              )
            ),
            temporal = list(
              interval = list(
                c(
                  "2024-01-01T00:00:00Z",
                  "2024-01-01T00:00:00Z"
                )
              )
            )
          ),
          links = list(
            list(
              rel = "root",
              href = "./collection.json",
              type = "application/json"
            ),
            list(
              rel = "self",
              href = "./collection.json",
              type = "application/json"
            ),
            list(
              rel = "parent",
              href = "../",
              type = "application/json"
            )
          ) |> append(purrr::map(data$species, \(sp) {
            list(
              rel = "item",
              href = paste0("./", collection, "/", sp),
              type = "application/json"
            )
          }))
        )
      }),
      .progress = TRUE
    )

  # -------------------------------------------------------------------------------------------------------------------
  # Rename items list based on expected filepath
  outdir <- file.path("stac", "collections")
  # Set names based on expected file path
  names(items) <- file.path(outdir, assets_filled$collection, paste0(assets_filled$species, ".json"))

  collections <- c(distribution_collections, stressors_collections)
  names(collections) <- file.path(
    outdir,
    purrr::map_chr(collections, \(j) j$id),
    "collection.json"
  )

  # -------------------------------------------------------------------------------------------------------------------
  # Write items jsons
  unique(dirname(c(names(items), names(collections)))) |>
    purrr::walk(\(x) dir.create(path = x, recursive = TRUE, showWarnings = FALSE))

  # Write jsons items in each subfolder
  purrr::walk2(items, names(items), \(x, y){
    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE) |>
      writeLines(con = y)
  })

  # Write jsons collections in each subfolder
  purrr::walk2(collections, names(collections), \(x, y){
    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE) |>
      writeLines(con = y)
  })
}

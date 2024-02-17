#' Prepare data for cloud bucket
#'
#' @export
make_bucket <- function() {
  # Respect the following structure:
  #   ./direct/
  #   ./distribution/
  #   ./indirect/
  #   ./net/
  #
  # For each folder
  #   - Same number of files and same names, i.e. one for each species
  #   - Each filename ends with `_cog.tif`

  # Outputs
  out <- list()
  out$direct <- here::here("bucket", "direct")
  out$indirect <- here::here("bucket", "indirect")
  out$net <- here::here("bucket", "net")
  out$distribution <- here::here("bucket", "distribution")
  lapply(out, rcea::chk_create)

  # Assessment results
  input <- list()
  input$direct <- here::here("output_nceamm_pam", "ncea", "2016_2021", "direct")
  input$indirect <- here::here("output_nceamm_pam", "ncea", "2016_2021", "indirect")
  input$net <- here::here("output_nceamm_pam", "ncea", "2016_2021", "net")
  for (i in names(input)) fs::dir_copy(input[[i]], here::here("bucket"))

  # Species distribution
  load(here::here("data", "FormatData_nceamm_pam", "biotic.RData"))
  sp <- dir(input$direct) |>
    basename() |>
    tools::file_path_sans_ext()
  stopifnot(all(sp %in% names(biotic)))
  for (i in seq_len(length(sp))) {
    mask_aoi(biotic[sp[i]]) |>
      as("Raster") |>
      terra::rast() |>
      terra::writeRaster(
        filename = here::here(out$distribution, glue::glue("{sp[i]}.tif")),
        filetype = "COG",
        gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
        overwrite = TRUE
      )
  }

  # Check that all species are in all folders
  stopifnot(all(basename(dir(out$direct)) == basename(dir(out$indirect))))
  stopifnot(all(basename(dir(out$direct)) == basename(dir(out$net))))
  stopifnot(all(basename(dir(out$direct)) == basename(dir(out$distribution))))

  # Get full assessments for direct, indirect and net effects
  direct_collection <- dir(out$direct, full.names = TRUE)
  indirect_collection <- dir(out$indirect, full.names = TRUE)
  net_collection <- dir(out$net, full.names = TRUE)

  rs_ref <- terra::rast(direct_collection[1])
  rs_ref[rs_ref >= 0] <- 0
  ncea_sum <- list(
    direct = rs_ref,
    indirect = rs_ref,
    net = rs_ref
  )

  for (p in direct_collection) {
    r <- terra::rast(p)
    ncea_sum$direct <- terra::mosaic(ncea_sum$direct, r, fun = "sum")
  }

  for (p in indirect_collection) {
    r <- terra::rast(p)
    ncea_sum$indirect <- terra::mosaic(ncea_sum$indirect, r, fun = "sum")
  }

  for (p in net_collection) {
    r <- terra::rast(p)
    ncea_sum$net <- terra::mosaic(ncea_sum$net, r, fun = "sum")
  }

  # Write on disk
  for (p in names(ncea_sum)) {
    terra::writeRaster(
      ncea_sum[[p]],
      filename = here::here(out[[p]], "all_species.tif"),
      filetype = "COG",
      gdal = c("COMPRESS=LZW", "TILED=YES", "OVERVIEW_RESAMPLING=AVERAGE"),
      overwrite = TRUE
    )
  }

  # Adjust file names
  lapply(out, function(x) {
    from <- dir(x, full.names = TRUE)
    to <- glue::glue("{tools::file_path_sans_ext(from)}_cog.tif")
    file.rename(from, to)
  })

  # Upload cogs
  # bucket_in <- "scotian-shelf-cogs-private"
  # system(glue::glue("gsutil -m cp -R bucket/* gs://{bucket_in}"))
}

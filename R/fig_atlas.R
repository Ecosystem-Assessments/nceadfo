#' Export figures for atlas
#'
#' @export
figures <- function() {
  out <- list()
  out$out <- here::here("figures")
  out$biotic_cnt <- here::here(out$out,"biotic","continuous")
  out$biotic_bin <- here::here(out$out,"biotic","binary")
  out$abiotic <- here::here(out$out, "abiotic")
  out$drivers <- here::here(out$out,"drivers")
  lapply(out, chk_create)
  
  # ---
  plotDat <- function(dat, out, suffix = "") {
    nm <- tools::file_path_sans_ext(names(dat))
    png(
      here::here(out, glue::glue("{nm}{suffix}.png")), 
      res = param$figures$resolution, 
      width = param$figures$width, 
      height = param$figures$height, 
      units = "mm", 
      pointsize = param$figures$pointsize
    )
    plot_nceadfo(dat)
    dev.off()
  }
  
  # Species distribution continuous
  dir(
    c("data/data-biotic/marine_mammals/continuous",
      "data/data-biotic/sea_birds/continuous", 
      "data/data-biotic/marine_species/random_forest_regression_smoothing"), 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$biotic_cnt)

  # Species distribution binary
  dir(
    c("data/data-biotic/marine_mammals/binary",
      "data/data-biotic/sea_birds/binary", 
      "data/data-biotic/marine_species/random_forest_regression_binary"), 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$biotic_bin)

  # Abiotic  
  dir("data/data-abiotic", full.names = TRUE, pattern = ".tif$") |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$abiotic)
  
  # Drivers 
  per <- dir("data/drivers/transformed")
  for(i in 1:length(per)) {
    dir(glue::glue("data/drivers/transformed/{per[i]}"), full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$drivers, suffix = glue::glue("-{per[i]}"))
  }
}


#' Export figures for atlas
#'
#' @export
fig_atlas <- function() {
  # Species 
  out <- list()
  out$out <- here::here("figures","biotic")
  out$cnt <- here::here(out$out, "continuous")
  out$bin <- here::here(out$out, "binary")
  out$atlas <- here::here("figures", "atlas","biotic")
  chk <- file.exists(out$cnt) & file.exists(out$bin)
  stopifnot(chk)
  chk_create(out$atlas)
  cnt <- dir(out$cnt, full.names = TRUE)
  bin <- dir(out$bin, full.names = TRUE)
  
  # Names 
  nm <- basename(cnt) |>
        tools::file_path_sans_ext() |>
        stringr::str_split("-", simplify = TRUE) |>
        as.data.frame() |>
        dplyr::rename(species = V1, aphiaID = V2) |>
        dplyr::mutate(species = gsub("_"," ",species)) |>
        dplyr::mutate(species = stringr::str_to_sentence(species))
        
  # Figures 
  for(i in 1:length(cnt)) {
    # Load images 
    i1 <- magick::image_read(cnt[i])
    i2 <- magick::image_read(bin[i])

    # Combine images 
    img <- magick::image_append(c(i1,i2))

    # Add border 
    ht <- magick::image_info(img)$height 
    hts <- 300
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 
    
    # Add text 
    img <- magick::image_annotate(
      img,
      nm$species[i],
      location = "+75+100",
      size = 125,
      font = "Palatino",
      style = "Italic",
      weight = 1000,
      decoration = "underline",
      color = NULL,
    )
    img <- magick::image_annotate(
      img,
      nm$aphiaID[i],
      location = "+75+250",
      size = 75,
      font = "Palatino",
      style = "Italic",
      weight = 200,
      color = "#494949",
    )
    
    # Resize 
    img <- magick::image_resize(img, "30%x30%")
    
    # Export  
    magick::image_write(
      img,
      path = here::here(out$atlas, basename(cnt[i])),
      format = "png",
      quality = NULL,
      depth = NULL,
      density = NULL,
      comment = NULL,
      flatten = FALSE,
      defines = NULL,
      compression = NULL
    )
    rm(img)
    gc()
  }
}

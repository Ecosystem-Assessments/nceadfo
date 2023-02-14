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
  out$cea_species <- here::here(out$out,"cea_species")
  out$cea_network <- here::here(out$out,"cea_network")
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
  dr <- here::here("data","cea_modules","drivers")
  per <- dir(dr)
  for(i in 1:length(per)) {
    here::here(dr, per[i]) |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$drivers)
  }

  # Species-scale cumulative effects assessment
  dr <- here::here("output","cea_species")
  per <- dir(dr)
  outsp <- here::here(out$cea_species, per)
  lapply(outsp, chk_create)
  for(i in 1:length(per)) {
    here::here(dr, per[i]) |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, outsp[i])
  }
  
  # # Network-scale cumulative effects assessment
  dr <- here::here("output","cea_network")
  per <- dir(dr)
  outsp <- here::here(out$cea_network, per)
  lapply(outsp, chk_create)
  for(i in 1:length(per)) {
    here::here(dr, per[i], "cea") |>
    dir(full.names = TRUE, pattern = ".tif$") |>
    lapply(stars::read_stars) |>
    lapply(plotDat, outsp[i])
  }
}


#' Export figures for atlas
#'
#' @export
fig_atlas <- function(type = c("species","drivers")) {
  # Parameters 
  hts <- 300
  img_resize <- "30%x30%"
  img1_loc <- "+2675+300"
  img2_loc <- "+5825+300"
  
  # Functions
  nm_title <- function(img, chr) {
    magick::image_annotate(
      img,
      chr,
      location = "+75+100",
      size = 125,
      font = "Palatino",
      style = "Italic",
      weight = 1000,
      decoration = "underline",
      color = NULL,
    )
  }

  nm_sub <- function(img, chr) {
    magick::image_annotate(
      img,
      chr,
      location = "+75+250",
      size = 75,
      font = "Palatino",
      style = "Italic",
      weight = 200,
      color = "#494949",
    )
  }

  nm_sub2 <- function(img, chr, location) {
    magick::image_annotate(
      img,
      chr,
      location = location,
      size = 70,
      font = "Palatino",
      style = "Italic",
      weight = 200,
      color = "#494949",
    )
  }  
  
  img_write <- function(img, path) {
    magick::image_write(
      img,
      path = path,
      format = "png",
      quality = NULL,
      depth = NULL,
      density = NULL,
      comment = NULL,
      flatten = FALSE,
      defines = NULL,
      compression = NULL
    )
  }

  # ----------------------------------------------------------------------------------------
  if ("species" %in% type) {
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
      img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
             magick::image_crop(glue::glue("0x{ht+hts}")) 
      
      # Add text 
      img <- nm_title(img, nm$species[i])
      img <- nm_sub(img, nm$aphiaID[i])
      
      # Resize 
      img <- magick::image_resize(img, img_resize)
      
      # Export  
      img_write(img, here::here(out$atlas, basename(cnt[i])))
      rm(img)
      gc()
    }
  }
  
  # ----------------------------------------------------------------------------------------
  if ("drivers" %in% type) {
    out <- list()
    out$out <- here::here("figures","drivers")
    out$atlas <- here::here("figures","atlas","drivers")
    chk <- file.exists(out$out)
    stopifnot(chk)
    chk_create(out$atlas)
    dr <- dir(out$out, full.names = TRUE)
    drList <- read.csv("data/cea_modules/drivers_list.csv") |>
              dplyr::select(-period) |>
              dplyr::distinct()
    # Names 
    nm <- basename(dr) |>
          tools::file_path_sans_ext() |>
          stringr::str_split("-", simplify = TRUE) |>
          as.data.frame() |>
          dplyr::rename(drivers = V1, period = V2) |>
          dplyr::mutate(path = here::here(out$out, glue::glue("{drivers}-{period}.png"))) |>
          dplyr::mutate(period2 = gsub("_"," - ",period)) |>
          dplyr::left_join(drList, by = "drivers")

    # Figures 
    for(i in 1:nrow(drList)) {
      uid1 <- nm$drivers == drList$drivers[i] & nm$period == "2010_2015"
      uid2 <- nm$drivers == drList$drivers[i] & nm$period == "2016_2021"
      
      # Load images 
      i1 <- magick::image_read(nm$path[uid1])
      i2 <- magick::image_read(nm$path[uid2])

      # Combine images 
      img <- magick::image_append(c(i1,i2))

      # Add border 
      ht <- magick::image_info(img)$height 
      img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
             magick::image_crop(glue::glue("0x{ht+hts}")) 
      
      # Add text       
      img <- nm_title(img, nm$group[uid1])
      img <- nm_sub(img, nm$description[uid1])
      img <- nm_sub2(img, nm$period2[uid1], img1_loc)
      img <- nm_sub2(img, nm$period2[uid2], img2_loc)
      
      # Resize 
      img <- magick::image_resize(img, img_resize)
      
      # Export  
      img_write(img, here::here(out$atlas, glue::glue("{nm$drivers[uid1]}.png")))
      rm(img)
      gc()
    }
  
  
  
  
  
  
  }


}

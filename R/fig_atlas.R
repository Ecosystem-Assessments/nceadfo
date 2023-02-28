#' Export figures for atlas
#'
#' @export
fig_atlas <- function(type = c("aoi","footprint","cea","difference","metanetwork","contribution","species","drivers")) {
  # Parameters 
  hts <- 300
  img_resize <- "30%x30%"

  # Text position
  x1 <- 3250
  x2 <- 100
  y1 <- 300
  yG <- 2830
  t1_1 <- glue::glue("+{x1}+{y1}")
  t2_1 <- glue::glue("+{x2}+{y1}")
  t1_2 <- glue::glue("+{x1}+{y1 + yG}")
  t2_2 <- glue::glue("+{x2}+{y1 + yG}")
  t1_3 <- glue::glue("+{x1}+{y1 + (yG*2)}")
  t2_3 <- glue::glue("+{x2}+{y1 + (yG*2)}")

  
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
      gravity = "northeast",
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
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("aoi" %in% type) {
    out <- list()
    out$figs <- here::here("figures","aoi")
    out$atlas <- here::here("figures","atlas","aoi")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    
    # Simply copy figure
    file.copy(
      from = here::here(out$figs,"aoi.png"),
      to = here::here(out$atlas,"aoi.png")
    )
  }

  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("footprint" %in% type) {
    out <- list()
    out$figs <- here::here("figures","footprint")
    out$atlas <- here::here("figures","atlas","footprint")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    figs <- dir(out$figs, full.names = TRUE)
    
    # -----------------
    # Species-scale cea
    i1 <- magick::image_read(here::here(out$figs, "cumulative_drivers-2010_2015.png"))
    i2 <- magick::image_read(here::here(out$figs, "cumulative_drivers-2016_2021.png"))
    img <- magick::image_append(c(i1,i2))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Cumulative drivers")
    img <- nm_sub2(img, "2010-2015", t1_1)
    img <- nm_sub2(img, "2016-2021", t2_1)

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "cumulative_drivers.png"))
    rm(img)
    gc()
    
    # -----------------
    # Species richness
    img <- magick::image_read(here::here(out$figs, "cumulative_species.png"))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Species richness")

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "species_richness.png"))
    rm(img)
    gc()
  }
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("cea" %in% type) {
    out <- list()
    out$figs <- here::here("figures","cea")
    out$atlas <- here::here("figures","atlas","cea")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    figs <- dir(out$figs, full.names = TRUE)
    
    # -----------------
    # Species-scale cea
    i1 <- magick::image_read(here::here(out$figs, "cea_species-2010_2015.png"))
    i2 <- magick::image_read(here::here(out$figs, "cea_species-2016_2021.png"))
    img <- magick::image_append(c(i1,i2))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Species-scale cumulative effects")
    img <- nm_sub2(img, "2010-2015", t1_1)
    img <- nm_sub2(img, "2016-2021", t2_1)

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "cea_species.png"))
    rm(img)
    gc()
    
    # -----------------
    # Network-scale cea
    i1 <- magick::image_read(here::here(out$figs, "cea_network-2010_2015.png"))
    i2 <- magick::image_read(here::here(out$figs, "cea_network-2016_2021.png"))
    img <- magick::image_append(c(i1,i2))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Network-scale cumulative effects")
    img <- nm_sub2(img, "2010-2015", t1_1)
    img <- nm_sub2(img, "2016-2021", t2_1)

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "cea_network.png"))
    rm(img)
    gc()
  }
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("difference" %in% type) {
    out <- list()
    out$figs <- here::here("figures","cea_difference")
    out$atlas <- here::here("figures","atlas","difference")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    figs <- dir(out$figs, full.names = TRUE)
    
    # -----------------
    # Species-scale cea
    img <- magick::image_read(here::here(out$figs, "cea_species_difference.png"))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Species-scale cumulative effects")
    img <- nm_sub(img, "Difference between 2016-2021 and 2010-2015")

    # Resize 
    # img <- magick::image_resize(img, img_resize)
    img1 <- magick::image_resize(img, img_resize)

    # # Export  
    # img_write(img, here::here(out$atlas, "cea_species_difference.png"))
    # rm(img)
    # gc()
    
    # -----------------
    # Network-scale cea
    img <- magick::image_read(here::here(out$figs, "cea_network_difference.png"))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Network-scale cumulative effects")
    img <- nm_sub(img, "Difference between 2016-2021 and 2010-2015")

    # Resize 
    # img <- magick::image_resize(img, img_resize)
    img2 <- magick::image_resize(img, img_resize)

    # # Export  
    # img_write(img, here::here(out$atlas, "cea_network_difference.png"))
    # rm(img)
    # gc()
    
    # Combine periods and export
    img <- magick::image_append(c(img1,img2))
    img_write(img, here::here(out$atlas, "cea_difference.png"))
    rm(img)
    gc()
  }
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("metanetwork" %in% type) {
    out <- list()
    out$figs <- here::here("figures","metanetwork")
    out$atlas <- here::here("figures","atlas","metanetwork")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    figs <- dir(out$figs, full.names = TRUE)
    
    # -----------------
    i1 <- magick::image_read(here::here(out$figs, "metanetwork-Direct-2016_2021.png"))
    i2 <- magick::image_read(here::here(out$figs, "metanetwork-Indirect-2016_2021.png"))
    
    # First image
    img <- magick::image_append(c(i1,i2))
    
    # Add border
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Metanetwork of direct & indirect effects")
    img <- nm_sub2(img, "Direct effects - 2016-2021", t1_1)
    img <- nm_sub2(img, "Indirect effects - 2016-2021", t2_1)

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "metanetwork.png"))
    rm(img)
    gc()
    
    # Also copy individual ones, they are most likely to be in the report indivudally
    file.copy(
      from = here::here(out$figs, "metanetwork-Total-2010_2015.png"), 
      to = here::here(out$atlas, "metanetwork-2010_2015.png")
    )
    file.copy(
      from = here::here(out$figs, "metanetwork-Total-2016_2021.png"), 
      to = here::here(out$atlas, "metanetwork-2016_2021.png")
    )
  }
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("contribution" %in% type) {
    out <- list()
    out$figs <- here::here("figures","contribution")
    out$atlas <- here::here("figures","atlas","contribution")
    chk <- file.exists(out$figs)
    stopifnot(chk)
    chk_create(out$atlas)
    figs <- dir(out$figs, full.names = TRUE)
    
    # -----------------
    i1 <- magick::image_read(here::here(out$figs, "contribution_group-2010_2015.png"))
    i2 <- magick::image_read(here::here(out$figs, "contribution_group-2016_2021.png"))
    img <- magick::image_append(c(i1,i2))

    # Add border 
    ht <- magick::image_info(img)$height 
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
           magick::image_crop(glue::glue("0x{ht+hts}")) 

    # Add text       
    img <- nm_title(img, "Contribution of drivers to cumulative effects")
    img <- nm_sub(img, "Mean contribution across taxa")
    img <- nm_sub2(img, "2010-2015", t1_1)
    img <- nm_sub2(img, "2016-2021", t2_1)

    # Resize 
    img <- magick::image_resize(img, img_resize)

    # Export  
    img_write(img, here::here(out$atlas, "contribution.png"))
    rm(img)
    gc()
    
    # Also copy individual ones, they are most likely to be in the report indivudally
    file.copy(
      from = here::here(out$figs, "contribution_group-2010_2015.png"), 
      to = here::here(out$atlas, "contribution_group-2010_2015.png")
    )
    file.copy(
      from = here::here(out$figs, "contribution_group-2016_2021.png"), 
      to = here::here(out$atlas, "contribution_group-2016_2021.png")
    )
  }
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  if ("species" %in% type) {
    # Species 
    out <- list()
    out$out <- here::here("figures","biotic")
    out$cnt <- here::here(out$out, "continuous")
    out$bin <- here::here(out$out, "binary")
    out$cea_sp1 <- here::here("figures","cea_species","2010_2015")
    out$cea_sp2 <- here::here("figures","cea_species","2016_2021")
    out$cea_net1 <- here::here("figures","cea_network","2010_2015")
    out$cea_net2 <- here::here("figures","cea_network","2016_2021")
    out$atlas <- here::here("figures", "atlas","biotic")
  
    # Checks
    chk <- file.exists(out$cnt) & 
           file.exists(out$bin) &
           file.exists(out$cea_sp1) &
           file.exists(out$cea_sp2) &
           file.exists(out$cea_net1) &
           file.exists(out$cea_net2)
    stopifnot(chk)
  
    # Create and get files
    chk_create(out$atlas)
    cnt <- dir(out$cnt, full.names = TRUE)
    bin <- dir(out$bin, full.names = TRUE)
    sp1 <- dir(out$cea_sp1, full.names = TRUE)
    sp2 <- dir(out$cea_sp2, full.names = TRUE)
    net1 <- dir(out$cea_net1, full.names = TRUE)
    net2 <- dir(out$cea_net2, full.names = TRUE)
  
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
      i3 <- magick::image_read(sp1[i])
      i4 <- magick::image_read(sp2[i])
      i5 <- magick::image_read(net1[i])
      i6 <- magick::image_read(net2[i])
      
      # Stack and combine images 
      s1 <- magick::image_append(c(i1,i2))
      s2 <- magick::image_append(c(i3,i4))
      s3 <- magick::image_append(c(i5,i6))
      img <- magick::image_append(c(s1,s2,s3), stack = TRUE)


      # Add border 
      ht <- magick::image_info(img)$height 
      img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
             magick::image_crop(glue::glue("0x{ht+hts}")) 
      
      # Add text 
      img <- nm_title(img, nm$species[i])
      img <- nm_sub(img, nm$aphiaID[i])      
      img <- nm_sub2(img, "Continuous distribution", t1_1)
      img <- nm_sub2(img, "Binary distribution", t2_1)
      img <- nm_sub2(img, "Species-scale cumulative effects - 2010-2015", t1_2)
      img <- nm_sub2(img, "Species-scale cumulative effects - 2016-2021", t2_2)
      img <- nm_sub2(img, "Network-scale cumulative effects - 2010-2015", t1_3)
      img <- nm_sub2(img, "Network-scale cumulative effects - 2016-2021", t2_3)

      
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
      img <- nm_sub(img, nm$fullname[uid1])
      img <- nm_sub2(img, nm$period2[uid1], t1_1)
      img <- nm_sub2(img, nm$period2[uid2], t2_1)
  
      # Resize 
      img <- magick::image_resize(img, img_resize)
  
      # Export  
      img_write(img, here::here(out$atlas, glue::glue("{nm$drivers[uid1]}.png")))
      rm(img)
      gc()
    }
  }
}

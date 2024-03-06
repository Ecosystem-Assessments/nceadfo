#' Export figures for report for nceahab project
#'
#' @export
fig_report_nceahab <- function() {
  # Parameters
  hts <- 300
  img_resize <- "30%x30%"

  # Text position
  x1 <- 3250
  x2 <- 100
  y1 <- 100
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
  out <- list()
  out$figs <- here::here("figures", "cea")
  out$nceahab <- here::here("figures", "nceahab")
  chk <- file.exists(out$figs)
  stopifnot(chk)
  chk_create(out$nceahab)
  figs <- dir(out$figs, full.names = TRUE)

  # # -----------------
  # # Figure 1. Network-scale & habitat-scale cea
  # i1 <- magick::image_read(here::here(out$figs, "ncea_2016_2021.png"))
  # i2 <- magick::image_read(here::here(out$figs, "cea_habitats_2016_2021.png"))
  # img <- magick::image_append(c(i1, i2))

  # # Add border
  # # ht <- magick::image_info(img)$height
  # # img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
  # #   magick::image_crop(glue::glue("0x{ht+hts}"))

  # # Add text
  # # img <- nm_title(img, "Network-scale & habitat-scale cumulative effects")
  # img <- nm_sub2(img, "Ecological network (A)", t1_1)
  # img <- nm_sub2(img, "Habitats (B)", t2_1)

  # # Resize
  # img <- magick::image_resize(img, img_resize)

  # # Export
  # img_write(img, here::here(out$nceahab, "ncea_ceahab.png"))
  # rm(img)
  # gc()

  # # -----------------
  # # Figure 2. Species cumulative effects
  # i1 <- magick::image_read(here::here(out$figs, "nceahab_species_2016_2021.png")) |>
  #   magick::image_resize(glue::glue("303%x303%"))
  # i2 <- magick::image_read(here::here(out$figs, "ncea_direct_2016_2021.png"))
  # i3 <- magick::image_read(here::here(out$figs, "ncea_indirect_2016_2021.png"))
  # i4 <- magick::image_read(here::here(out$figs, "nceahab_indirect_habitats_2016_2021.png"))
  # i5 <- magick::image_append(c(i2, i3, i4), stack = TRUE)
  # img <- magick::image_append(c(i1, i5))

  # # Resize
  # img <- magick::image_resize(img, "33%x33%")

  # # # Add border
  # # ht <- magick::image_info(img)$height
  # # img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
  # #   magick::image_crop(glue::glue("0x{ht+hts}"))

  # # Add text
  # # img <- nm_title(img, "Joint cumulative effects on species")
  # img <- nm_sub2(img, "Joint cumulative effects (A)", "+1200+100")
  # img <- nm_sub2(img, "Direct effects: stressors (B)", "+35+840")
  # img <- nm_sub2(img, "Indirect effects: interactions (C)", "+35+1777")
  # img <- nm_sub2(img, "Indirect effects: habitats (D)", "+35+2714")
  # # 940
  # # Resize
  # img <- magick::image_resize(img, img_resize)

  # # Export
  # img_write(img, here::here(out$nceahab, "nceahab_species.png"))
  # rm(img)
  # gc()

  # -----------------
  # Figure 3. Ecosystem-scale assessment
  i1 <- magick::image_read(here::here(out$figs, "nceahab_2016_2021.png"))
  i2 <- magick::image_read(here::here(out$figs, "nceahab_normalized_2016_2021.png"))
  img <- magick::image_append(c(i1, i2))

  # # Add border
  # ht <- magick::image_info(img)$height
  # img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
  #   magick::image_crop(glue::glue("0x{ht+hts}"))

  # Add text
  # img <- nm_title(img, "Ecosystem-scale cumulative effects")
  img <- nm_sub2(img, "Ecosystem-scale effects (A)", t1_1)
  img <- nm_sub2(img, "Ecosystem-scale effects\n(normalized; B)", t2_1)

  # Resize
  img <- magick::image_resize(img, img_resize)

  # Export
  img_write(img, here::here(out$nceahab, "nceahab.png"))
  rm(img)
  gc()
}

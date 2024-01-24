#' Export figures for atlas
#'
#' @export
fig_webinar <- function() {
  # Function
  plotDat <- function(dat, suffix = "", type = "regular", main = NULL, sub = NULL, land = TRUE, legend = TRUE) {
    out <- here::here("figures", "webinar")
    chk_create(out)
    nm <- tools::file_path_sans_ext(names(dat))
    png(
      here::here(out, glue::glue("{nm}{suffix}.png")),
      res = param$figures$resolution,
      width = param$figures$width,
      height = param$figures$height,
      units = "mm",
      pointsize = param$figures$pointsize
    )
    if (type == "regular") plot_nceadfo_simple(dat, mainTitle = main, subTitle = sub, land = land, legend = legend)
    if (type == "dual") plot_nceadfo_dual_simple(dat, mainTitle = main, subTitle = sub)
    dev.off()
  }

  # Area of interest
  aoi <- sf::st_read("data/aoi/aoi.gpkg")

  # Mask
  tmp <- stars::read_stars("data/drivers/transformed/2010_2015/fisheries_intensity.e2b7e6c4.DD.tif")
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = tmp)

  # "Footprint fnc"
  foot <- function(fold) {
    dat <- dir(fold, full.names = TRUE) |>
      lapply(stars::read_stars)
    dat <- cumul(dat)
    dat <- dat * aoi_mask
  }
  foot2 <- function(fold) {
    dat <- lapply(fold, stars::read_stars)
    dat <- cumul(dat)
    dat <- dat * aoi_mask
  }
  # ------------------------------------------------------------------------------------------
  # Species
  mm <- foot(here::here("data", "data-biotic", "marine_mammals", "binary"))
  ms <- foot(here::here("data", "data-biotic", "marine_species", "random_forest_regression_binary"))
  sb <- foot(here::here("data", "data-biotic", "sea_birds", "binary"))
  sp <- foot(here::here("data", "cea_modules", "species"))
  names(mm) <- "marine_mammals"
  names(ms) <- "marine_species"
  names(sb) <- "sea_birds"
  names(sp) <- "species"
  plotDat(mm, sub = "Number of species")
  plotDat(ms, sub = "Number of species")
  plotDat(sb, sub = "Number of species")
  plotDat(sp, sub = "Number of species")
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # Drivers
  drList <- read.csv(here::here("data", "cea_modules", "drivers_list.csv")) |>
    dplyr::select(-period) |>
    dplyr::distinct()
  out <- here::here("data", "cea_modules", "drivers", "2016_2021")

  ## Names of drivers to select groups
  files <- dir(out, full.names = TRUE)
  nm <- dir(out) |>
    stringr::str_replace("-2016_2021.tif", "")

  cl <- foot2(files[nm %in% drList$drivers[drList$group == "Climate"]])
  co <- foot2(files[nm %in% drList$drivers[drList$group == "Coastal"]])
  fs <- foot2(files[nm %in% drList$drivers[drList$group == "Fisheries"]])
  sh <- foot2(files[nm %in% drList$drivers[drList$group == "Marine traffic"]])
  dr <- foot2(files)
  names(cl) <- "climate"
  names(co) <- "coastal"
  names(fs) <- "fisheries"
  names(sh) <- "marine_traffic"
  names(dr) <- "drivers"
  plotDat(cl, sub = "Sum of normalized intensities")
  plotDat(co, sub = "Sum of normalized intensities")
  plotDat(fs, sub = "Sum of normalized intensities")
  plotDat(sh, sub = "Sum of normalized intensities")
  plotDat(dr, sub = "Sum of normalized intensities")
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # CEA simple & complete
  dat <- here::here("output", "cea_full", "ncea_2016_2021.tif") |> stars::read_stars()
  dat <- dat * aoi_mask
  plotDat(dat, land = FALSE, legend = FALSE)
  plotDat(dat, suffix = "full", sub = "Cumulative effects score")
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # Change in CEA
  dat <- here::here("output", "cea_difference", "cea_network_difference.tif") |> stars::read_stars()
  dat <- dat * aoi_mask
  plotDat(dat, type = "dual", sub = "Cumulative effects change")
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # Species-specific sensitivities
  out <- here::here("figures", "webinar")
  load(here::here("data", "format_modules", "species_sensitivity.RData"))
  dat <- t(species_sensitivity)
  xlim <- ncol(dat)
  ylim <- nrow(dat)
  png(
    here::here(out, "species_specific_sensitivity.png"),
    res = param$figures$resolution,
    width = xlim * 2,
    height = param$figures$height,
    units = "mm",
    pointsize = param$figures$pointsize
  )
  par(mar = c(0, 1, 1, 0), family = "serif", bg = "#00000000")
  graphicsutils::plot0(x = c(0, xlim), y = c(0, ylim))
  mtext("Environmental\ndrivers", side = 2, srt = 90, cex = 1, line = -1)
  mtext("Species", side = 3, cex = 1, line = 0)
  for (i in 1:xlim) lines(x = c(i, i), y = c(0, ylim), lty = 2, col = "#00000033", lwd = 0.5)
  for (i in 1:ylim) lines(x = c(0, xlim), y = c(i, i), lty = 2, col = "#00000033", lwd = 0.5)
  for (i in 1:xlim) {
    for (j in 1:ylim) {
      points(x = i, y = j, pch = 21, cex = dat[j, i], col = "#00000000", bg = "#394F6877")
    }
  }
  dev.off()
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # Stressor contribution
  file.copy(
    from = here::here("figures", "contribution", "contribution_group-2016_2021.png"),
    to = here::here("figures", "webinar", "contribution_group-2016_2021.png")
  )
  # ------------------------------------------------------------------------------------------
}

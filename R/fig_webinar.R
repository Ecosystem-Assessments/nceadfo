#' Export figures for atlas
#'
#' @export
fig_webinar <- function() {
  # Function
  plotDat <- function(dat, suffix = "", type = "regular", main =  NULL, sub = NULL, land = TRUE, legend = TRUE) {
    out <- here::here("figures","webinar")
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
    if( type == "dual") plot_nceadfo_dual_simple(dat, mainTitle = main, subTitle = sub, land = land, legend = legend)
    dev.off()
  }
  
  # Area of interest
  aoi <- sf::st_read("data/aoi/aoi.gpkg")

  # "Footprint fnc"
  foot <- function(fold) {
    dat <- dir(fold, full.names = TRUE) |>
           lapply(stars::read_stars) 
    dat <- cumul(dat)
    dat <- dat[aoi]            
  }
  foot2 <- function(fold) {
    dat <- lapply(fold, stars::read_stars) 
    dat <- cumul(dat)
    dat <- dat[aoi]            
  }
  # ------------------------------------------------------------------------------------------
  # Species 
  mm <- foot(here::here("data","data-biotic","marine_mammals","binary"))
  ms <- foot(here::here("data","data-biotic","marine_species","random_forest_regression_binary"))
  sb <- foot(here::here("data","data-biotic","sea_birds","binary"))
  names(mm) <- "marine_mammals"
  names(ms) <- "marine_species"
  names(sb) <- "sea_birds"
  plotDat(mm, sub = "Number of species")
  plotDat(ms, sub = "Number of species")
  plotDat(sb, sub = "Number of species")
  # ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # Drivers
  drList <- read.csv(here::here("data","cea_modules","drivers_list.csv")) |>
            dplyr::select(-period) |>
            dplyr::distinct()
  out <- here::here("data","cea_modules","drivers","2016_2021")
  
  ## Names of drivers to select groups
  files <- dir(out, full.names = TRUE)
  nm <- dir(out) |>
        stringr::str_replace("-2016_2021.tif","")
  
  
  cl <- foot2(files[nm %in% drList$drivers[drList$group == "Climate"]])
  co <- foot2(files[nm %in% drList$drivers[drList$group == "Coastal"]])
  fs <- foot2(files[nm %in% drList$drivers[drList$group == "Fisheries"]])
  sh <- foot2(files[nm %in% drList$drivers[drList$group == "Marine traffic"]])
  names(cl) <- "climate"
  names(co) <- "coastal"
  names(fs) <- "fisheries"
  names(sh) <- "marine_traffic"
  plotDat(cl, sub = "Sum of normalized intensities")
  plotDat(co, sub = "Sum of normalized intensities")
  plotDat(fs, sub = "Sum of normalized intensities")
  plotDat(sh, sub = "Sum of normalized intensities")

# ------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------
  # CEA 
  dat <- here::here("output","cea","cea_network-2016_2021.tif") |> stars::read_stars()
  plotDat(dat, land = FALSE, legend = FALSE)
  # ------------------------------------------------------------------------------------------
}

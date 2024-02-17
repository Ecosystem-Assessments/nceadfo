# Internals
#' @importFrom exactextractr exact_extract
#' @importFrom fs path path_package
#' @importFrom glue glue glue_sql
#' @importFrom units set_units
#' @importFrom whisker whisker.render
#' @importFrom yaml yaml.load_file write_yaml read_yaml
NULL

# ------------------------------------------------------------------------------
# Gracieuseté de Kevin Cazelles: https://github.com/KevCaz
# my simple(r) version of use template
use_template <- function(template, save_as = stdout(), pkg = "ceanav", ...) {
  template <- readLines(
    path_package(package = pkg, template)
  )
  # NB by default whisker forward the parent envi and I used this
  writeLines(whisker::whisker.render(template, ...), save_as)
}

# ------------------------------------------------------------------------------
# Création d'un hyperlien en format markdown à partir de deux vecteurs
rep_hyperlien <- function(texte, url) {
  nl <- length(texte)
  hyperlien <- character(nl)

  for (i in 1:nl) {
    if (!is.null(url[i])) {
      hyperlien[i] <- paste0("[", texte[i], "](", url[i], ")")
    } else {
      hyperlien[i] <- texte[i]
    }
  }

  # Return
  hyperlien
}

# ------------------------------------------------------------------------------
# Clean global environment
clean <- function() {
  objs <- ls(envir = globalenv())
  rm(list = objs, pos = ".GlobalEnv")
}


# ------------------------------------------------------------------------------
#' Check if folder exists and create if not
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}

# ------------------------------------------------------------------------------
#' Function to export raster objects
export_raster <- function(dat, out, nm) {
  raster::writeRaster(
    x = dat,
    filename = here::here(out, glue::glue("{nm}.tif")),
    overwrite = TRUE
  )
  unlink(here::here(out, glue::glue("{nm}.tif.aux.xml")))
}


# ------------------------------------------------------------------------------
# Function to mask on area of interest
mask_aoi <- function(dat) {
  # Area of interest
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  tmp <- dat
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi_mask <- stars::st_rasterize(aoi["val_ras"], template = tmp)
  stars::st_dimensions(aoi_mask) <- stars::st_dimensions(dat)
  dat * aoi_mask
}

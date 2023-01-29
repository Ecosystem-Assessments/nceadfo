#' Export figures for atlas
#'
#' @export
fig_atlas <- function() {
  out <- list()
  out$biotic <- "figures/biotic"
  out$abiotic <- "figures/abiotic"
  out$stressors <- "figures/stressors"
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
  
  # Species distribution
  dir(
    c("data/data-biotic/marine_mammals/continuous",
      "data/data-biotic/sea_birds/continuous", 
      "data/data-biotic/marine_species/random_forest_regression_smoothing"), 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$biotic)

  # Abiotic  
  dir("data/data-abiotic/", full.names = TRUE) |>
  lapply(stars::read_stars) |>
  lapply(plotDat, out$abiotic)
  
  # Stressors 
  per <- dir("data/stressors/transformed")
  for(i in 1:length(per)) {
    dir(glue::glue("data/stressors/transformed/{per[i]}"), full.names = TRUE) |>
    lapply(stars::read_stars) |>
    lapply(plotDat, out$stressors, suffix = glue::glue("-{per[i]}"))
  }
}

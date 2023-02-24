#' Export figures for atlas
#'
#' @export
fig_webinar <- function() {
  # Function
  plotDat <- function(dat, suffix = "", type = "regular", main =  NULL, sub = NULL, legend = TRUE) {
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
    if (type == "regular") plot_nceadfo_simple(dat, mainTitle = main, subTitle = sub, legend = legend)
    if( type == "dual") plot_nceadfo_dual_simple(dat, mainTitle = main, subTitle = sub, legend = legend)
    dev.off()
  }

  # TO FILL OUT, NOT TONIGHT! 

  # CEA 
  dat <- here::here("output","cea","cea_network-2016_2021.tif") |> stars::read_stars()
  plotDat(dat, legend = FALSE)
}

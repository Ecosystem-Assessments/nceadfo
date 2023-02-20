#' plot nceadfo data maps
#'
#' base plot functions for nceadfo project
#'
#' @param dat object of class stars
#' @param main main title
#' @param type secondary title / data type (see metadata files)
#' @param subtitle subtitle
#' @param unit_data units of data
#' @param references data citation used for integrated data
#' @param city logical, if TRUE name of cities are added to graph
#' @param minUp numeric, minimum upper side to write as a function of bbox extent (legend)
#' @param ... further specifications, see \link{plot} and details.
#'
#' @examples
#'  load_integrated("navigation")
#'  plot_nceadfo(navigation[,10], "Navigation", "Recherche gouvernementale")
#'
#' @export

plot_nceadfo <- function(dat, ...) {
  UseMethod("plot_nceadfo", dat)
}

#' @method plot_nceadfo stars
#' @name plot_nceadfo
#' @export
# plot_nceadfo.stars <- function(dat, main = NULL, type = NULL, subtitle = NULL, unit_data = NULL, references = NULL, city = TRUE, ...) {
plot_nceadfo.stars <- function(dat) {
  # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  # png(glue('./figures/delete.png'), res = param$figures$resolution, width = param$figures$width, height = param$figures$height, units = "mm", pointsize = param$figures$pointsize)
  
  # ------------------
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  can <- sf::st_read("data/basemap/canada.gpkg", quiet = TRUE)
  usa <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE)    


  # ------------------
  global_parameters()
  bbox <- param$bbox
  pal <- colorRampPalette(viridis::viridis(100))

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  image(dat, col = viridis::viridis(100))
  plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
  plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
  plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  box()
}


#' Plot to show differences with dual legend color centered on 0
#' @describeIn plot_nceadfo plot with dual legend color centered on 0
#' @export
plot_nceadfo_dual <- function(dat) {
  # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  # png(glue('./figures/delete.png'), res = param$figures$resolution, width = param$figures$width, height = param$figures$height, units = "mm", pointsize = param$figures$pointsize)
  
  # ------------------
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  can <- sf::st_read("data/basemap/canada.gpkg", quiet = TRUE)
  usa <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE)    


  # ------------------
  global_parameters()
  bbox <- param$bbox
  red <- "#744242"
  blue <- "#036e95"
  pal1 <- colorRampPalette(c(graphicsutils::lighten(red,80), graphicsutils::darken(red,50)))
  pal2 <- colorRampPalette(c(graphicsutils::lighten(blue,80), graphicsutils::darken(blue,50)))
  
  # ------------------
  names(dat) <- "diff"
  plus <- dat |> dplyr::mutate(diff = ifelse(diff < 0, NA, diff))
  minus <- dat |> dplyr::mutate(diff = ifelse(diff > 0, NA, -diff))

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  image(plus, col = pal1(100))
  image(minus, col = pal2(100), add = TRUE)
  plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
  plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
  plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  box()
}
# 
# 
#   # ------------------
#   # Inserts location
#   # rect(fluvial[1], fluvial[3], fluvial[2], fluvial[4], lty = 2, border = "#00000088")
#   # rect(montreal[1], montreal[3], montreal[2], montreal[4], lty = 2,
#   #      border = paste0(global_param$col$insert[1], "88"))
#   # rect(lacstpierre[1], lacstpierre[3], lacstpierre[2], lacstpierre[4], lty = 2,
#   #      border = paste0(global_param$col$insert[2], "88"))
#   # rect(quebec[1], quebec[3], quebec[2], quebec[4], lty = 2,
#   #      border = paste0(global_param$col$insert[3], "88"))
# 
#   # ------------------
#   # Legend
#   bin <- dat[,1,drop = TRUE] %>%
#          table() %>%
#          names()
#   minUp <- ifelse(is.null(type), .175, .23)
# 
#   if (length(bin) == 2 | length(bin) == 1) {
#     sbt <- "Presence"
#     cols <- global_param$col$integrated$palette[4]
#     plot_legend_bin(
#       col = cols,
#       subTitle = sbt,
#       cexSub = .5,
#       minUp = minUp,
#       showNA = showNA
#     )
#   } else {
#     maxDat <- max(dat[,1,drop = TRUE], na.rm = TRUE)
#     cols <- pal(101)[((dat[,1,drop = TRUE] / maxDat)*100)+1]
#     plot_legend_cont(
#       range = range(dat[,1,drop = TRUE], na.rm = TRUE),
#       pal = pal,
#       subTitle = unit_data,
#       cexSub = .4,
#       minUp = minUp,
#       showNA = showNA
#     )
#   }
# 
#   # ------------------
#   # Text
#   y <- bbox$ymax
#   if (!is.null(main)) {
#     y <- y - 10000
#     text(x = bbox$xmin + 1000,
#          y = y,
#          labels = main,
#          font = 2,
#          adj = c(0,.5),
#          cex = .8
#        )
#   }
# 
#   if (!is.null(type)) {
#     y <- y - 30000
#     text(
#       x = bbox$xmin + 1000,
#       y = y,
#       labels = type,
#       adj = c(0,.5),
#       font = 1,
#       cex = .6
#     )
#   }
# 
#   if (!is.null(subtitle)) {
#       if (!is.null(type)) {
#         y <- y - 25000
#       } else {
#         y <- y - 30000
#       }
#     text(
#       x = bbox$xmin + 1000,
#       y = y,
#       labels = subtitle,
#       adj = c(0,.5),
#       font = 3,
#       cex = .6
#     )
#   }
# 
#   # Add sources
#   if(!is.null(references)) {
#     refs <- stringr::str_split(references, ",") %>% unlist()
#     txt <- glue("Raw data : {references}. Details : Appendix 1.")      
#     mtext(text = txt,
#           side = 1,
#           font = 3,
#           adj = .98,
#           cex = .4,
#           line = -.45)
#   }
# 
# 
#   # ------------------
#   # Data
#   plotDat()
# 
#   # ------------------
#   # Cities
#   if (city) {
#     plot(st_geometry(cities), add = TRUE, pch = 21, col = "#3e3e3e", bg = "#9f9f9f", cex = .4)
#     for(i in 1:nrow(cities)) {
#       text(x = cities$X[i]+cities$offX[i],
#            y = cities$Y[i]+cities$offY[i],
#            labels = cities$city[i],
#            cex = .35,
#            col = global_param$col$integrated$textOff,
#            adj = c(cities$adjX[i], .5))
#     }
#   }
# 
#   # # ------------------------------------------------------------------------
#   # # Inserts
#   # # Place name
#   # name <- function(nm) {
#   #   xmin <- par("usr")[1]
#   #   xmax <- par("usr")[2]
#   #   ymin <- par("usr")[3]
#   #   ymax <- par("usr")[4]
#   #   x <- xmin + 2500
#   #   y <- ymax - 4000
#   #   text(x, y, nm, adj = c(0,.5), cex = .5, col = global_param$col$integrated$textOff)
#   # }
#   # 
#   # # ---------------------------
#   # # Montreal
#   # par(new = TRUE)
#   # par(fig = c(.525,.68,.05,.3), mar = c(0,0,0,0))
#   # # par(fig = c(.545,.745,.05,.25), mar = c(0,0,0,0))
#   # plot0(x = c(montreal$xmin, montreal$xmax), y = c(montreal$ymin+5000, montreal$ymax))
#   # box(col = paste0(global_param$col$insert[1], "88"))
#   # plotDat()
#   # name("Montréal")
#   # 
#   # # ---------------------------
#   # # Lac St-Pierre
#   # par(new = TRUE)
#   # par(fig = c(.7,.965,.05,.3), mar = c(0,0,0,0))
#   # # par(fig = c(.765,.965,.05,.25), mar = c(0,0,0,0))
#   # plot0(x = c(lacstpierre$xmin, lacstpierre$xmax), y = c(lacstpierre$ymin, lacstpierre$ymax))
#   # box(col = paste0(global_param$col$insert[2], "88"))
#   # plotDat()
#   # name("Lac St-Pierre")
#   # 
#   # 
#   # # ---------------------------
#   # # Québec
#   # par(new = TRUE)
#   # par(fig = c(.7,.965,.325,.575), mar = c(0,0,0,0))
#   # # par(fig = c(.765,.965,.275,.475), mar = c(0,0,0,0))
#   # plot0(x = c(quebec$xmin, quebec$xmax), y = c(quebec$ymin, quebec$ymax))
#   # box(col = paste0(global_param$col$insert[3], "88"))
#   # plotDat()
#   # name("Québec")
# 
# 
#   # dev.off()
# }

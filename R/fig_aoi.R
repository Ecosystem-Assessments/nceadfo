#' Figure of study area
#'
#' @export

fig_aoi <- function() {
  # ------------------------------------------------------------------------
  # Graph principal
  out <- here::here("figures","aoi")
  chk_create(out)
  png(
    here::here(out, "aoi.png"), 
    res = param$figures$resolution, 
    width = param$figures$width, 
    height = param$figures$height, 
    units = "mm", 
    pointsize = param$figures$pointsize
  )

  # ------------------
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  can <- sf::st_read("data/basemap/canada.gpkg", quiet = TRUE)
  canada <- sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)
  usa <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE)    

  # ------------------
  # Labels
  labs <- c("Study area")

  # ------------------
  global_parameters()
  bbox <- param$bbox

  # ------------------
  yGap <- 1
  par(family = 'serif', mar = c(.5, .5, .5, .5))
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin-yGap, bbox$ymax-yGap))
  plot(
    sf::st_geometry(aoi), 
    lwd = 2, 
    border = param$col$aoi, 
    col = glue::glue("{param$col$focus}33"), 
    add = TRUE
  )
  plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
  plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  box()
  
  # ------------------
  # Text
  rect(
    bbox$xmin-.15, 
    bbox$ymax+.25-yGap, 
    bbox$xmin+2.8, 
    bbox$ymax-.275-yGap,
    col = "#ffffffBB",
    border = "#000000"
  )
  text(
    x = bbox$xmin,
    y = bbox$ymax-yGap,
    labels = labs[1],
    font = 2,
    adj = c(0,.5),
    cex = .8
  )


  # 
  # # ------------------
  # # Cities
  # plot(st_geometry(cities), add = TRUE, pch = 21, col = "#3e3e3e", bg = "#9f9f9f", cex = .4)
  # for(i in 1:nrow(cities)) {
  #   text(x = cities$X[i]+cities$offX[i],
  #        y = cities$Y[i]+cities$offY[i],
  #        labels = cities$city[i],
  #        cex = .35,
  #        col = "#000000",
  #        adj = c(cities$adjX[i], .5))
  # }
  # 
  # # ---------------------------
  # # Grille
  # par(new = TRUE)
  # par(fig = c(.035,.25,.6,.825), mar = c(0,0,0,0))
  # # par(fig = c(.765,.965,.05,.25), mar = c(0,0,0,0))
  # plot0(x = c(lacstpierre$xmin, lacstpierre$xmax), y = c(lacstpierre$ymin, lacstpierre$ymax))
  # box(col = "#00000088")
  # plotDat(trans = "FF")
  # text(x = lacstpierre$xmin + 750, y = lacstpierre$ymax - 3000, labels = labs[4], cex = .5, font = 3, adj = c(0,.5))
  # 
  # ---------------------------
  # Canada
  par(new = TRUE)
  par(fig = c(.6,.965,.039,.36), mar = c(0,0,0,0))
  bbox <- sf::st_bbox(canada)
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  plot(sf::st_geometry(canada), col = glue::glue("{param$col$coastline}33"), add = TRUE)
  plot(sf::st_geometry(usa), col = glue::glue("{param$col$coastline}33"), add = TRUE)
  plot(sf::st_geometry(aoi),lwd = 1, border = param$col$focus,col = param$col$focus, add = TRUE)
  box(col = "#000000")
  
  # ---------------------------
  dev.off()
}

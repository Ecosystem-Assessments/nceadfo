#' plot nceadfo data maps
#'
#' base plot functions for nceadfo project
#'
#' @param dat object of class stars
#' @param mainTitle main title
#' @param subTitle subtitle
#' @param unit_data units of data
#' @param references data citation used for integrated data
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
plot_nceadfo.stars <- function(dat, mainTitle = NULL, subTitle = NULL, range = NULL) {
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
  
  # Legend
  if (isBin(dat)) {
    plot_legend_bin(
      col = viridis::viridis(100)[50],
      mainTitle = mainTitle,
      subTitle = "Presence"
    )
  } else {
    r <- c(floor(min(dat[[1]], na.rm = TRUE)), ceiling(max(dat[[1]], na.rm = TRUE)))
    plot_legend_cont(
      range = r,
      pal = pal,
      mainTitle = mainTitle,
      subTitle = subTitle
    )    
  }
  
  # Box
  box()
}


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
#' Plot to show differences with dual legend color centered on 0
#' @describeIn plot_nceadfo plot with dual legend color centered on 0
#' @export
plot_nceadfo_dual <- function(dat, mainTitle = NULL, subTitle = NULL) {
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
  pal2 <- colorRampPalette(c(graphicsutils::darken(blue,50), graphicsutils::lighten(blue,80)))
  
  # ------------------
  r <- c(floor(min(dat[[1]], na.rm = TRUE)), ceiling(max(dat[[1]], na.rm = TRUE)))
  rMax <- max(abs(r))

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  image(dat, col = pal1(100), zlim = c(0,rMax))
  image(dat, col = pal2(100), zlim = c(-rMax,0), add = TRUE)
  plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
  plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
  plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  
  # Legend
  plot_legend_dual(
    rMax = rMax,
    pal1 = pal1,
    pal2 = pal2,
    mainTitle = mainTitle,
    subTitle = subTitle
  )    
  box()
}

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# Function to evaluate if data is binary or continuous
isBin <- function(dat) {
  bin <- as.data.frame(dat) |>
         na.omit() 
  bin <- round(bin[,3],4) |>
         unique()
  ifelse(length(bin) <= 2, TRUE, FALSE)
}


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
#' Plot legend
#'
#' Function to create a legend
#'
#' @rdname plot_legend
#'
#' @export
#'
#' @param range numeric, vector with minimal and maximal values
#' @param pal character, vector of colors, or color palette
#' @param pal1 character, vector of colors, or color palette for first palette used in dual
#' @param pal2 character, vector of colors, or color palette for second palette used in dual
#' @param cex.text numeric, cex for legend text
#' @param mainTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param subTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param type character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param nTick numeric, number of ticks in the legend
#' @param minUp numeric, minimum upper side to write as a function of bbox extent
#' @param colText color of text
#'
#' @return Opens a graphical interface with the plot
#'
#' @keywords plot, legend
plot_legend_cont <- function(
  range = c(0,1),
  pal = NULL,
  cexMain = .75,
  cexSub = .5,
  minUp = .055,
  mainTitle = NULL,
  subTitle = NULL,
  n = 5,
  colText = "#dedede"
) {
  # Legends
  # Palette
  if(class(pal) == 'character') {
    pal <- colorRampPalette(pal)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .015*xR # minimum left side to write
  yinit <- ymax - minUp*yR # minimum upper side to write
  ygap <- .04*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR

  # Plot
   x <- seq(from = xinit, to = xinit + .17*xR, by = .0003*xR)
   z <- data.frame(y1 = ybarUp,
                  y2 = ybarDn,
                  x1 = x[1:length(x)-1],
                  x2 = x[2:length(x)],
                  col = pal(length(x)-1),
                  stringsAsFactors = F)
   for(k in 1:nrow(z)) {
    polygon(x = c(z$x1[k],z$x2[k],z$x2[k],z$x1[k],z$x1[k]),
            y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
            col = z$col[k],
            border = z$col[k])
   }

   # Add axis
   x <- seq(from = xinit, to = xinit + .17*xR, length.out = n)
   lines(x = c(xinit, xinit + .17*xR), y = rep(z$y2[1], 2), col = colText)
   for(i in 1:n) lines(x = rep(x[i],2), y = c(z$y2[1], z$y2[1]- .003*yR), col = colText)

  
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Labels
   if(range[2] <= 5) {
     lab <- round(seq(from = 0, to = max(range[2]), length.out = n), 2)     
   } else {
     lab <- round(seq(from = 0, to = max(range[2]), length.out = n))     
   }
   
   text(x = x,
        y =  rep(z$y2[1] - .01*yR, n),
        labels = lab,
        cex = cexSub*.75,
        adj = c(1, 1),
        srt = 45,
        col = colText
      )

  # Add titles
  yText <- ybarUp + .025*yR

  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = latex2exp::TeX(subTitle, italic = TRUE),
         cex = cexSub,
         adj = c(0,1),
         col = colText
       )
     yText <- yText + .035*yR
   }

  # Add main title
  if(!is.null(mainTitle)) {
  text(x = xinit,
       y = yText,
       labels = mainTitle,
       cex = cexMain,
       font = 2,
       adj = c(0,1),
       col = colText
     )
  }
}

# =================================================================
#' @rdname plot_legend
#' @export
plot_legend_bin <- function (
  col,
  cexMain = .75,
  cexSub = .5,
  minUp = .055,
  mainTitle = NULL,
  subTitle = NULL,
  colText = "#dedede"
) {

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .015*xR # minimum left side to write
  yinit <- ymax - minUp*yR # minimum upper side to write
  ygap <- .04*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR


  # Plot
  sq <- .05
  polygon(x = c(xinit, xinit, xinit + sq*xR, xinit + sq*xR, xinit),
          y = c(ybarUp,ybarDn,ybarDn,ybarUp,ybarUp),
          col = col,
          border = "#000000")


  # Add titles
  yText <- ybarUp + .025*yR

  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = latex2exp::TeX(subTitle, italic = TRUE),
         cex = cexSub,
         adj = c(0,1),
         col = colText)
     yText <- yText + .0224*yR
   }

  # Add main title
  if(!is.null(mainTitle)) {
  text(x = xinit,
       y = yText,
       labels = mainTitle,
       cex = cexMain,
       font = 2,
       adj = c(0,1),
       col = colText)
  }
}


# =================================================================
#' @rdname plot_legend
#' @export
plot_legend_dual <- function(
  rMax,
  pal1 = NULL,
  pal2 = NULL,
  cexMain = .75,
  cexSub = .5,
  minUp = .055,
  mainTitle = NULL,
  subTitle = NULL,
  n = 5,
  colText = "#dedede"
) {
  # Legends
  # Palette
  if(class(pal1) == 'character') {
    pal1 <- colorRampPalette(pal1)
    pal2 <- colorRampPalette(pal2)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .015*xR # minimum left side to write
  yinit <- ymax - minUp*yR # minimum upper side to write
  ygap <- .04*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR

  # Divide in two 
  wdPal <- .17*xR
  xmid <- xinit + (wdPal)/2
  xend <- xinit+wdPal
  
  # Palette 2 (negative)
   x <- seq(from = xinit, to = xmid, by = .0003*xR)
   z <- data.frame(y1 = ybarUp,
                  y2 = ybarDn,
                  x1 = x[1:length(x)-1],
                  x2 = x[2:length(x)],
                  col = pal2(length(x)-1),
                  stringsAsFactors = F)
   for(k in 1:nrow(z)) {
    polygon(x = c(z$x1[k],z$x2[k],z$x2[k],z$x1[k],z$x1[k]),
            y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
            col = z$col[k],
            border = z$col[k])
   }
   
   # Palette 1 (positive)
    x <- seq(from = xmid, to = xend, by = .0003*xR)
    z <- data.frame(y1 = ybarUp,
                   y2 = ybarDn,
                   x1 = x[1:length(x)-1],
                   x2 = x[2:length(x)],
                   col = pal1(length(x)-1),
                   stringsAsFactors = F)
    for(k in 1:nrow(z)) {
     polygon(x = c(z$x1[k],z$x2[k],z$x2[k],z$x1[k],z$x1[k]),
             y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
             col = z$col[k],
             border = z$col[k])
    }

   # Add axis
   x <- seq(from = xinit, to = xinit + wdPal, length.out = n)
   lines(x = c(xinit, xinit + wdPal), y = rep(z$y2[1], 2), col = colText)
   for(i in 1:n) lines(x = rep(x[i],2), y = c(z$y2[1], z$y2[1]- .003*yR), col = colText)

  
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Labels
   lab <- round(c(-rMax, -rMax/2, 0, rMax/2, rMax))
   text(x = x,
        y =  rep(z$y2[1] - .01*yR, n),
        labels = lab,
        cex = cexSub*.75,
        adj = c(1, 1),
        srt = 45,
        col = colText
      )

  # Add titles
  yText <- ybarUp + .025*yR

  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = latex2exp::TeX(subTitle, italic = TRUE),
         cex = cexSub,
         adj = c(0,1),
         col = colText
       )
     yText <- yText + .035*yR
   }

  # Add main title
  if(!is.null(mainTitle)) {
  text(x = xinit,
       y = yText,
       labels = mainTitle,
       cex = cexMain,
       font = 2,
       adj = c(0,1),
       col = colText
     )
  }
}

#' Function for simple plots for presentations
#' @name plot_nceadfo_simple
#' @export
plot_nceadfo_simple <- function(dat, legend = TRUE, land = TRUE, mainTitle = NULL, subTitle = NULL, range = NULL) {  
  # ------------------
  aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
  can <- sf::st_read("data/basemap/canada.gpkg", quiet = TRUE)
  usa <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE)    

  # ------------------
  global_parameters()
  bbox <- param$bbox
  pal <- colorRampPalette(viridis::viridis(100))

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5), bg = "#00000000")
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  image(dat, col = viridis::viridis(100))
  plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
  if (land) {
    plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
    plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  }
  
  if (legend) {
    # Legend
    if (isBin(dat)) {
      plot_legend_bin(
        col = viridis::viridis(100)[50],
        mainTitle = mainTitle,
        subTitle = "Presence"
      )
    } else {
      r <- c(floor(min(dat[[1]], na.rm = TRUE)), ceiling(max(dat[[1]], na.rm = TRUE)))
      plot_legend_cont(
        range = r,
        pal = pal,
        mainTitle = mainTitle,
        subTitle = subTitle
      )    
    }    
  }
}

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
#' Plot to show differences with dual legend color centered on 0
#' @describeIn plot_nceadfo plot with dual legend color centered on 0
#' @export
plot_nceadfo_dual_simple <- function(dat, mainTitle = NULL, subTitle = NULL) {
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
  pal2 <- colorRampPalette(c(graphicsutils::darken(blue,50), graphicsutils::lighten(blue,80)))
  
  # ------------------
  r <- c(floor(min(dat[[1]], na.rm = TRUE)), ceiling(max(dat[[1]], na.rm = TRUE)))
  rMax <- max(abs(r))

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5), bg = "#00000000")
  graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  image(dat, col = pal1(100), zlim = c(0,rMax))
  image(dat, col = pal2(100), zlim = c(-rMax,0), add = TRUE)
  plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
  plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
  plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)
  
  # Legend
  plot_legend_dual(
    rMax = rMax,
    pal1 = pal1,
    pal2 = pal2,
    mainTitle = mainTitle,
    subTitle = subTitle
  )    
}
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Species
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Humpback whale - Megaptera novaeangliae
# by Chris huh
# http://phylopic.org/image/ce70490a-79a5-47fc-afb9-834e45803ab4/
# License https://creativecommons.org/licenses/by-sa/3.0/
hump <- png::readPNG('./pubs/webinar/img/PhyloPic.ce70490a.Chris-huh.Balaenoptera-novaeangliae.png', native = T)

# Beluga whale - Delphinapterus leucas
# by Xavier Giroux-Bougard
# http://phylopic.org/image/f1367ab1-40cf-4e9a-a84b-37508f11a7c7/
bew <- png::readPNG('./pubs/webinar/img/PhyloPic.f1367ab1.Xavier-Giroux-Bougard.Delphinapterus_Delphinapterus-leucas_Monodontidae.png', native = T)

# Atlantic cod - Gadus morhua
# Milton Tan
# http://phylopic.org/image/bba1800a-dd86-451d-a79b-c5944cfe5231/
cod <- png::readPNG('./pubs/webinar/img/PhyloPic.bba1800a.Milton-Tan.Gadariae_Gadidae_Gadiformes_Gadinae_Gadus_Gadus-morhua_Zeiogadaria.png', native = T)

# Capelin - Mallotus villosus
# by xgirouxb
# http://phylopic.org/image/f1f91d08-b850-4600-ad64-622ce87f0199/
cap <- png::readPNG('./pubs/webinar/img/PhyloPic.f1f91d08.xgirouxb.Osmeridae_Osmeriformes_Osmerinae_Osmerini_Osmeroidea_Osmeroidei_Thaleichthys_Thaleichthys-pacificus.png', native = T)

# Krill - Meganyctiphanes norvegica
# by Steven Haddock • Jellywatch.org
# http://phylopic.org/image/44a3628d-aafd-45cc-97a6-1cb74bd43dec/
kri <- png::readPNG('./pubs/webinar/img/PhyloPic.44a3628d.Steven-Haddock-Jellywatch-org.Copepoda-Malacostraca_Eucarida_Eumalacostraca_Euphausiacea_Euphausiidae_Malacostraca.png', native = T)

# Copepoda
# by Joanna Wolfe
# http://phylopic.org/image/c5dbd85a-c4be-4990-a369-c830ad23cb22/
cop <- png::readPNG('./pubs/webinar/img/PhyloPic.c5dbd85a.Joanna-Wolfe.Calanoida_Copepoda_Epacteriscidae_Erebonectes_Gymnoplea_Neocopepoda.png', native = T)
cop[cop == 16777216] <-0


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Drivers
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
SST <- png::readPNG('./pubs/webinar/img/SST.png', native = T) # modified from https://fontawesome.com/icons/thermometer-full?style=solid
Shipping <- png::readPNG('./pubs/webinar/img/ship-solid.png', native = T) # https://fontawesome.com/icons/ship?style=solid
DD <- png::readPNG('./pubs/webinar/img/trawl.png', native = T)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Plot functions
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Species in line
xsp <- c(-1.4, -.6, 0.075, 0.675, 1.225, 1.675)
xdr <- c(-1,0,1)
ysp <- .6
ydr <- 1.4
speciesLine <- function(lab = 'names', focus = NULL) {
  # Colors
  colsp <- rep(col1, 6)

  # Focus
  if (!is.null(focus)) colsp[-focus] <- sapply(colsp[-focus], lighten, percentage = 85, USE.NAMES = FALSE)

  # Species
  cxy <- .6
  sh <- .75
  y <- ysp
  x <- xsp
  pchImage(x = x[1], y = y-.025, obj = hump, cex.x = 2.2*sh, cex.y = cxy+.1, col = colsp[1])
  pchImage(x = x[2], y = y, obj = bew, cex.x = 2*sh, cex.y = cxy, col = colsp[2])
  pchImage(x = x[3], y = y, obj = cod, cex.x = 1.55*sh, cex.y = cxy, col = colsp[3])
  pchImage(x = x[4], y = y, obj = cap, cex.x = 1.25*sh, cex.y = cxy, col = colsp[4])
  pchImage(x = x[5], y = y, obj = kri, cex.x = .85*sh, cex.y = cxy-.1, col = colsp[5])
  pchImage(x = x[6], y = y, obj = cop, cex.x = .7*sh, cex.y = cxy, col = colsp[6])

  if (!is.null(focus)) {
    uid <- focus
  } else {
    uid <- 1:6
  }

  if (!is.null(lab)) {
    # Names
    if(lab == 'names') {
      nm <- c('Baleine à bosse','Béluga','Morue','Capelan','Krill','Copépodes')
      sc <- c('\\textit{(Megaptera novaeangliae)}','\\textit{(Delphinapterus leucas)}',
              '\\textit{(Gadus morhua)}','\\textit{(Mallotus villotus)}',
              '(Euphausiacea)','(Copepoda)')
      text(x = x[uid], y = ysp-.25, labels = nm[uid], cex = .8, adj = c(.5,.5), col = colText1)
      text(x = x[uid], y = ysp-.35, labels = TeX(sc[uid]), cex = .6, adj = c(.5,.5), col = colText2)
    }

    # Math
    if(lab == 'math') {
      nm <- c("\\textit{$E_{bal}$}","$\\textit{E_{bel}$}",
              "\\textit{$E_{mor}$}","$\\textit{E_{cap}$}",
              "\\textit{$E_{kri}$}","\\textit{$E_{cop}$}")
      text(x = x[uid], y = ysp-.25, labels = TeX(nm[uid]), cex = .8, adj = c(.5,.5), col = colText1)
    }
  }
}

# Drivers in line
driversLine <- function(lab = 'names', focusSt = NULL, focusPt = NULL) {
  # Colors
  colst <- rep(colDr, 3)
  colpt <- rep(colDr, 5)

  # Focus
  if (!is.null(focusSt)) colst[-focusSt] <- sapply(colst[-focusSt], lighten, percentage = 85, USE.NAMES = FALSE)
  if (!is.null(focusPt)) colpt[-focusPt] <- sapply(colpt[-focusPt], lighten, percentage = 85, USE.NAMES = FALSE)


  # Drivers
  x <- xdr
  y <- ydr
  pchImage(x = x[1]+.05, y = y, obj = SST, cex.x = .55, cex.y = .7, col = colst[1])
  pchImage(x = x[2], y = y, obj = Shipping, cex.x = .55, cex.y = .65, col = colst[2])
  pchImage(x = x[3], y = y, obj = DD, cex.x = .55, cex.y = .65, col = colst[3])

  if (!is.null(focusSt)) {
    uid <- focusSt
  } else {
    uid <- 1:3
  }

  if (!is.null(lab)) {
    if(lab == 'names') {
      # Names
      nm <- c('Hausse de température','Navigation','Pêcheries')
      text(x = x[uid], y = 1.4+.25, labels = nm[uid], cex = .8, adj = c(.5,.5), col = colText1)
    }
    if(lab == 'math') {
      nm <- c("\\textit{$D_{tem}$}", "\\textit{$D_{nav}$}",
              "\\textit{$D_{pec}$}")
      text(x = x[uid], y = 1.4+.25, labels = TeX(nm[uid]), cex = .8, adj = c(.5,.5), col = colText1)
    }
  }

  # Effects
  lines(x = c(x[1], xsp[6]), y = c(y-.15,ysp+.15), col = colpt[1], lwd = 1, lty = 4)
  lines(x = c(x[2], xsp[1]), y = c(y-.15,ysp+.15), col = colpt[2], lwd = 1, lty = 4)
  lines(x = c(x[2], xsp[2]), y = c(y-.15,ysp+.15), col = colpt[3], lwd = 1, lty = 4)
  lines(x = c(x[3], xsp[3]), y = c(y-.15,ysp+.15), col = colpt[4], lwd = 1, lty = 4)
  lines(x = c(x[3], xsp[4]), y = c(y-.15,ysp+.15), col = colpt[5], lwd = 1, lty = 4)

  if (!is.null(focusPt)) {
    uid <- focusPt
  } else {
    uid <- 1:5
  }

  if (!is.null(lab)) {
    if (lab == 'math') {
    nm <- c("\\textit{$\\mu_{cop,tem}$}", "\\textit{$\\mu_{bal,nav}$}",
            "\\textit{$\\mu_{bel,nav}$}", "\\textit{$\\mu_{mor,pec}$}",
            "\\textit{$\\mu_{cap,pec}$}")
    xmu <- c(mean(c(xdr[1],xsp[6])),
             mean(c(xdr[2],xsp[1])),
             mean(c(xdr[2],xsp[2])),
             mean(c(xdr[3],xsp[3])),
             mean(c(xdr[3],xsp[4])))
    ymu <- mean(c(ydr,ysp))
    xGap <- c(-.1,-.3,.15,-.1,.2)
    yGap <- c(.1,0,-.05,.1,0)
    text(x = xmu[uid]+xGap[uid], y = ymu+yGap[uid], labels = TeX(nm[uid]), cex = .8, adj = c(.5,.5), col = colText1)
  }
 }
}

# Food web
species <- function(sp = NULL, fadeSp = NULL, stress = NULL) {
  # Colors
  sts <- c(colB, colB, colM, colM, col1, colP)
  colsSp <- rep(col1, 6)
  if (!is.null(sp)) colsSp[sp] <- col3
  if (!is.null(stress)) colsSp[stress] <- sts[stress]
  if (!is.null(fadeSp)) colsSp[fadeSp] <- sapply(colsSp[fadeSp], lighten, percentage = 75, USE.NAMES = FALSE)

  # Species
  cxy <- .6
  sh <- .75
  pchImage(x = -.51, .5, obj = hump, cex.x = 2.2*sh, cex.y = cxy+.1, col = colsSp[1])
  pchImage(x = .01, 1, obj = bew, cex.x = 2*sh, cex.y = cxy, col = colsSp[2])
  pchImage(x = .5, .66, obj = cod, cex.x = 1.55*sh, cex.y = cxy, col = colsSp[3])
  pchImage(x = 0, .04, obj = cap, cex.x = 1.25*sh, cex.y = cxy, col = colsSp[4])
  pchImage(x = -.3, -.66, obj = kri, cex.x = .85*sh, cex.y = cxy-.1, col = colsSp[5])
  pchImage(x = .3, -.66, obj = cop, cex.x = .7*sh, cex.y = cxy, col = colsSp[6])
}

interactions <- function(int = NULL, fadeInt = NULL) {
  # Colors
  colsInt <- rep(col1, 7)
  if (!is.null(int)) colsInt[int] <- col3
  if (!is.null(fadeInt)) colsInt[fadeInt] <- sapply(colsInt[fadeInt], lighten, percentage = 75, USE.NAMES = FALSE)

  # Interactions
  lines(x = c(0, 0), y = c(.15,.85), col = colsInt[1]) # bel to cap
  lines(x = c(-.1, -.4), y = c(.1,.4), col = colsInt[2]) # hump to cap
  lines(x = c(.1, .4), y = c(.1,.52), col = colsInt[3]) # cod to cap
  lines(x = c(.1, .4), y = c(.9325,.7275), col = colsInt[4]) # bel to cod
  lines(x = c(.045, .2375), y = c(-.1,-.52), col = colsInt[5]) # cap to cop
  lines(x = c(-.045, -.2375), y = c(-.1,-.52), col = colsInt[6]) # cap to kri
  lines(x = c(-.4725, -.325), y = c(.36,-.52), col = colsInt[7]) # hump to kri
}

drivers <- function(fadeDr = NULL) {
  # Colors
  colsDr <- rep(colDr, 3)
  if (!is.null(fadeDr)) colsDr[fadeDr] <- sapply(colsDr[fadeDr], lighten, percentage = 75, USE.NAMES = FALSE)

  # Drivers
  pchImage(x = -.4, 1.45, obj = SST, cex.x = .55, cex.y = .7, col = colsDr[1])
  pchImage(x = 0, 1.45, obj = Shipping, cex.x = .55, cex.y = .65, col = colsDr[2])
  pchImage(x = .4, 1.45, obj = DD, cex.x = .55, cex.y = .65, col = colsDr[3])
}

pathways <- function(pathRem = NULL, fadePath = NULL) {
  # Colors
  colsPt <- rep(colDr, 5)
  if (!is.null(pathRem)) colsPt[pathRem] <- '#00000000'
  if (!is.null(fadePath)) colsPt[fadePath] <- sapply(colsPt[fadePath], lighten, percentage = 75, USE.NAMES = FALSE)

  # Pathways of effect
  lines(x = c(-.4, .25), y = c(1.3,-.52), lty = 4, col = colsPt[1]) # SST to cop
  lines(x = c(0, 0), y = c(1.3,1.1), lty = 4, col = colsPt[2]) # SHP to bew
  lines(x = c(0, -.41), y = c(1.3,.6), lty = 4, col = colsPt[3]) # SHP to hump
  lines(x = c(.4, .485), y = c(1.3,.75), lty = 4, col = colsPt[4]) # FISH to cod
  lines(x = c(.4, .03), y = c(1.3,.1), lty = 4, col = colsPt[5]) # FISH to cap
}

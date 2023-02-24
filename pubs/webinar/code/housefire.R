# source('./pubs/webinar/code/housefire.R')
# ----------------------------------------------------------------------
# Param setup
# ----------------------------------------------------------------------
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Libraries & default parameters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
library(graphicsutils)
library(latex2exp)
library(tidyverse)
# library(deSolve)
# source('./code/0-param.R')
source('./pubs/webinar/code/functions.R')
figwd <- 140
fight <- 100
figres <- 250
bg <- '#f5f5f5'

# Params
xG <- .065
yG <- .075
tht <- .4
pht <- 1
off <- .55
colH1 <- '#00000044'
colH2 <- '#00000011'
col1 <- '#32506A'
colText1 <- '#444458'
colDr <- '#000000'
colP <- colB <- colM <- col3 <- '#db9318'


# Théorique
th <- data.frame(x = c(-1,-1,1,1), y = c(-1,-1+tht,-1+tht,-1))

# Piliers
x <- seq(-1,1,length.out = 6)
xP <- c(x[1]+xG, x[2]-xG,
        x[2]+xG, x[3]-xG,
        x[3]+xG, x[4]-xG,
        x[4]+xG, x[5]-xG,
        x[5]+xG, x[6]-xG)

yP <- c(-1+tht+yG, -1+tht+pht, -1+tht+pht, -1+tht+yG)
st <- data.frame(x = c(rep(xP[1], 2), rep(xP[2],2)), y = yP)
mw <- data.frame(x = c(rep(xP[3], 2), rep(xP[4],2)), y = yP)
se <- data.frame(x = c(rep(xP[5], 2), rep(xP[6],2)), y = yP)
tr <- data.frame(x = c(rep(xP[7], 2), rep(xP[8],2)), y = yP)
sp <- data.frame(x = c(rep(xP[9], 2), rep(xP[10],2)), y = yP)


# Toit
tt <- -1 + tht + yG + pht
ce <- data.frame(x = c(-1,1,0), y = c(tt, tt, 1))


theorique <- function(col = colH2, ch = TRUE) {
  encircle(x = th$x, y = th$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(th$x), y = mean(th$y), labels = 'Data modules', adj = c(.5,.5), col = colText1, cex = .85, font = 2)
  }
}

stress <- function(col = colH2, ch = TRUE) {
  encircle(x = st$x, y = st$y, off.set = .55, border = colH1, col = col)
  # text(x = mean(st$x), y = mean(st$y), labels = TeX('$D$'), adj = c(.5,.5), col = colText1, cex = 1)
  if (ch) {
    text(x = mean(st$x), y = mean(st$y), labels = 'Drivers', adj = c(.5,.5), col = colText1, cex = .85, font = 2)
  }
}

metaweb <- function(col = colText1, ch = TRUE) {
  encircle(x = mw$x, y = mw$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(mw$x), y = mean(mw$y), labels = 'Metaweb', adj = c(.5,.5), col = "#E7E7E7", cex = .85, font = 2)
  }
}

sensitivity <- function(col = colH2, ch = TRUE) {
  encircle(x = se$x, y = se$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(se$x), y = mean(se$y), labels = 'Species\nsensitivity', adj = c(.5,.5), col = colText1, cex = .85, font = 2)
  }
}

trophic <- function(col = colText1, ch = TRUE) {
  encircle(x = tr$x, y = tr$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(tr$x), y = mean(tr$y), labels = 'Trophic\nsensitivity', adj = c(.5,.5), col = "#E7E7E7", cex = .85, font = 2)
  }
}

cv <- function(col = colH2, ch = TRUE) {
  encircle(x = sp$x, y = sp$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(sp$x), y = mean(sp$y), labels = 'Species', adj = c(.5,.5), col = colText1, cex = .85, font = 2)
  }
}

cumeff <- function(col = colH2, ch = TRUE) {
  encircle(x = ce$x, y = ce$y, off.set = .55, border = colH1, col = col)
  if (ch) {
    text(x = mean(ce$x), y = mean(ce$y), labels = 'Cumulative effects', adj = c(.5,.5), col = colText1, cex = .85, font = 2)
  }
}

# layout
mat <- matrix(1:2, nrow = 1)

# Figure
png('./pubs/webinar/figures/datareq/1-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions(fadeInt = 1:7)
pathways(fadePath = NULL)
species(fadeSp = NULL)
drivers(fadeDr = NULL)

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
cumeff()
theorique()
dev.off()

# Figure
png('./pubs/webinar/figures/datareq/2-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions(fadeInt = 1:7)
pathways(fadePath = 1:5)
species(fadeSp = 1:6)
drivers()

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
cumeff()
theorique()
stress()
dev.off()

# Figure
png('./pubs/webinar/figures/datareq/3-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions(fadeInt = 1:7)
pathways(fadePath = 1:5)
species()
drivers(fadeDr = 1:3)

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
cumeff()
stress()
cv()
theorique()
dev.off()

# Figure
png('./pubs/webinar/figures/datareq/4-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions(fadeInt = 1:7)
pathways(fadePath = NULL)
species(fadeSp = 1:6)
drivers(fadeDr = 1:3)

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
stress()
cumeff()
cv()
theorique()
sensitivity()
dev.off()

# Figure
png('./pubs/webinar/figures/datareq/5-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions()
pathways(fadePath = 1:5)
species(fadeSp = 1:6)
drivers(fadeDr = 1:3)

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
stress()
cv()
sensitivity()
cumeff()
metaweb()
theorique()
dev.off()

# Figure
png('./pubs/webinar/figures/datareq/6-house.png', res = figres, width = figwd*.6 + figwd, height = fight, units = "mm")
layout(mat)
# Plot 1
par(mar = c(0, 1, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-.75, .75), y = c(-.8, 1.75))
interactions(fadeInt = NULL)
pathways(fadePath = NULL)
species(fadeSp = NULL, sp = 4)
drivers(fadeDr = NULL)

# Plot 2
par(mar = c(0,0,0,0), family = 'serif', bg = bg)
plot0()
stress()
cv()
sensitivity()
cumeff()
trophic()
metaweb()
theorique()
dev.off()

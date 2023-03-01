# source('pubs/webinar/code/motifs.R')
library(graphicsutils)
# library(latex2exp)
# library(tidyverse)
# library(deSolve)
source('./pubs/webinar/code/functions.R')

x <- seq(-.9,.9,length.out = 6)
y <- -.45
mainCex <- 1.25
familyText <- 'serif'
colHead <- '#444643'
platform <- c("#c7cbce", "#96a3a3", "#687677", "#222d3d", "#25364a", "#c77f20","#e69831", "#e3af16", "#e4be29", "#f2ea8b")
bg <- "#00000000"

mainCex <- 1.25
familyText <- 'serif'
colHead <- '#444643'
platform <- c("#c7cbce", "#96a3a3", "#687677", "#222d3d", "#25364a", "#c77f20","#e69831", "#e3af16", "#e4be29", "#f2ea8b")
colP <- colB <- colM <- col3 <- '#db9318'
col1 <- '#32506A'
colText1 <- '#444458'
colDr <- '#000000'


# Motif names
motifs <- c(
  'Tri-trophic\nfood chain',
  'Omnivory',
  'Exploitative\ncompetition',
  'Apparent\ncompetition',
  'Partiellement\nconnecté',
  'Déconnecté'
)

# Motifs
l <- data.frame(x1 = c(0,0,0,-.35,.35,.35,-.4,.4,0,-.4,.4,0,0,0,0,0,0,0),
                x2 = c(0,0,0,.35,-.35,.35,0,0,0,0,0,0,0,0,0,0,0,0),
                y1 = c(-1,0,0,.3,1,1,1,1,-1,-1,-1,1,0,0,0,0,0,0),
                y2 = c(0,1,0,-1,.3,-1,-1,-1,-1,1,1,1,1,0,0,0,0,0))

pt <- data.frame(x = c(0,0,0,.35,-.35,.35,-.4,0,.4,-.4,.4,0,0,0,0,0,0,0),
                 y = c(-1,0,1,-1,.3,1,1,-1,1,-1,-1,1,-1,0,1,-1,0,1),
                 col = colHead, bg = platform[6], stringsAsFactors = F, lwd = 2)
x <- rep(seq(-.9,.9,length.out = 6), each = 3)
x[13:18] <- x[13:18] + .2
scalingX <- .3
scalingY <- .4
pt$x <- (pt$x * scalingX) + x
pt$y <- (pt$y * scalingY) + y
l$x1 <- (l$x1 * scalingX) + x
l$x2 <- (l$x2 * scalingX) + x
l$y1 <- (l$y1 * scalingY) + y
l$y2 <- (l$y2 * scalingY) + y
# for(i in 1:nrow(l)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
# for(i in 1:nrow(l)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
# points(x = pt$x, y = pt$y, col = pt$col, bg = pt$bg, pch = 21, lwd = 4, cex = 3)

out <- here::here("pubs","webinar","figures","modules","simul")

# Only 4 main motifs
png(here::here(out,'11_motifs.png'), res = 250, width = 125, height = 50, units = "mm")
par(mar = c(0,0,0,0), bg = bg)
plot0(x = c(-1.1,.4), y = c(-1,1))
x <- seq(-.9,.9,length.out = 6)
text(x = x[1:4], y = .5, labels = motifs[1:4], family = familyText, cex = mainCex*.9, col = colText1, font = 2)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
points(x = pt$x[1:12], y = pt$y[1:12], col = pt$col[1:12], bg = pt$bg[1:12], pch = 21, lwd = 4, cex = 2)
dev.off()


# All motifs
png(here::here(out,'1_motifs.png'), res = 250, width = 135, height = 60, units = "mm")
par(mar = c(0,0,0,0), bg = bg)
plot0(x = c(-1.1,.4), y = c(-1.33,1))
x <- seq(-.9,.9,length.out = 6)
text(x = x[1:4], y = .5, labels = motifs[1:4], family = familyText, cex = mainCex, col = colText1, font = 2)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
points(x = pt$x[1:12], y = pt$y[1:12], col = pt$col[1:12], bg = pt$bg[1:12], pch = 21, lwd = 4, cex = 2)
dev.off()

# All motifs
png(here::here(out,'2_motifs.png'), res = 250, width = 135, height = 60, units = "mm")
par(mar = c(0,0,0,0), bg = bg)
plot0(x = c(-1.1,.4), y = c(-1.33,1))
x <- seq(-.9,.9,length.out = 6)
text(x = x[1:4], y = .5, labels = motifs[1:4], family = familyText, cex = mainCex, col = colText1, font = 2)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
points(x = pt$x[1:12], y = pt$y[1:12], col = pt$col[1:12], bg = pt$bg[1:12], pch = 21, lwd = 4, cex = 2)
text(x = x[1:4], y = -1.3, adj = c(.5,.5), labels = c(127, 511, 127, 127),family = familyText, cex = mainCex*1.5, col = colText1, font = 2)
dev.off()

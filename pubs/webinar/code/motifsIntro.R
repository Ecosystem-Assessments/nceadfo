# source('pubs/webinar/code/motifsIntro.R')
library(graphicsutils)
# library(latex2exp)
# library(tidyverse)
# library(deSolve)
source('./pubs/webinar/code/functions.R')
figwd <- 140
fight <- 100
figres <- 250
xT <- -1.25
yT <- 1.45
yG <- .14
cT <- .9
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

l <- l[1:12, ]
pt <- pt[1:12, ]
xR <- c(-.7,.7)
yR <- c(1,1.75)
y <- 1.35

x <- rep(seq(xR[1],xR[2],length.out = 4), each = 3)
scalingX <- .225
scalingY <- .125
pt$x <- (pt$x * scalingX) + x
pt$y <- (pt$y * scalingY) + y
l$x1 <- (l$x1 * scalingX) + x
l$x2 <- (l$x2 * scalingX) + x
l$y1 <- (l$y1 * scalingY) + y
l$y2 <- (l$y2 * scalingY) + y

plotMotifs <- function(uid) {
  x <- seq(xR[1],xR[2],length.out = 4)
  text(x = x[1:uid], y = 1.725, labels = motifs[1:uid], family = familyText, cex = cT, col = colText1, font = 2)

  for(i in 1:(uid*3)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 7)
  for(i in 1:(uid*3)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 1.25)
  points(x = pt$x[1:(uid*3)], y = pt$y[1:(uid*3)], col = pt$col[1:(uid*3)], bg = pt$bg[1:(uid*3)], pch = 21, lwd = 4, cex = 1.25)
}

# for(i in 1:nrow(l)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
# for(i in 1:nrow(l)) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
# points(x = pt$x, y = pt$y, col = pt$col, bg = pt$bg, pch = 21, lwd = 4, cex = 3)


# Only 4 main motifs
# png('./Figures/10_1-motifs.png', res = 250, width = 125, height = 50, units = "mm")
# par(mar = c(0,0,0,0), bg = bg)
# plot0(x = c(-1.1,.4), y = c(-1,1))
# x <- seq(xR[1],xR[2],length.out = 4)
# text(x = x[1:4], y = 1.7, labels = motifs[1:4], family = familyText, cex = mainCex*.9, col = colText1, font = 2)
# for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = colHead, lwd = 10)
# for(i in 1:12) lines(x = c(l$x1[i], l$x2[i]), y = c(l$y1[i], l$y2[i]), col = platform[6], lwd = 2)
# points(x = pt$x[1:12], y = pt$y[1:12], col = pt$col[1:12], bg = pt$bg[1:12], pch = 21, lwd = 4, cex = 2)
# dev.off()
#
#
out <- here::here("pubs","webinar","figures","modules","motifs")
chk_create(out)
png(here::here(out, "1_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
# pathways(fadePath = 1:5)
interactions()
species()
# drivers(fadeDr = 1:3)
text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
     labels = "- Sub-modules of n-species\n- Network desconstruction\n- Structural properties\n- 3-species motifs")
dev.off()


# 
# png(here::here(out, "2_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
# par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
# plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
# interactions()
# species()
# # text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
# # text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
# #      labels = "- Sous-modules de\n   n-espèces \n- Propriétés structurelles\n   des réseaux")
# dev.off()
# 
# 

png(here::here(out, "3_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
interactions(fadeInt = c(2,3,4,6,7))
species(fadeSp = c(1,3,5), sp = c(2,4,6))
plotMotifs(1)
# text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
# text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
#      labels = "- Sous-modules de\n   n-espèces \n- Propriétés structurelles\n   des réseaux")
dev.off()

png(here::here(out, "4_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
interactions(fadeInt = c(2,5,6,7))
species(fadeSp = c(1,5,6), sp = c(2,3,4))
plotMotifs(2)
# text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
# text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
#      labels = "- Sous-modules de\n   n-espèces \n- Propriétés structurelles\n   des réseaux")
dev.off()


png(here::here(out, "5_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
interactions(fadeInt = c(1,4:7))
species(fadeSp = c(2,5,6), sp = c(1,3,4))
plotMotifs(3)
# text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
# text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
#      labels = "- Sous-modules de\n   n-espèces \n- Propriétés structurelles\n   des réseaux")
dev.off()


png(here::here(out, "6_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
interactions(fadeInt = c(1:4,7))
species(fadeSp = 1:3, sp = c(4:6))
plotMotifs(4)
# text(x = xT, y = yT, adj = c(0,1), labels = "Motifs :", font = 4, cex = cT, col = colText1)
# text(x = xT, y = yT-yG, adj = c(0,1), cex = cT, col = colText1,
#      labels = "- Sous-modules de\n   n-espèces \n- Propriétés structurelles\n   des réseaux")
dev.off()

png(here::here(out, "7_motif.png"), res = figres, width = figwd, height = fight, units = "mm")
par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
pathways(fadePath = c(1,3))
interactions(fadeInt = c(2,5,6,7))
species(fadeSp = c(1,5,6), stress = c(2:4))
drivers(fadeDr = 1)
dev.off()


# 1 = hum
# 2 = bel
# 3 = cod
# 4 = cap
# 5 = kri
# 6 = cop

# 1 = bel cap
# 2 = hum cap
# 3 = cod cap
# 4 = bel cod
# 5 = cap cop
# 6 = cap kri
# 7 = hum kri

# ----------------------------------------------------------------------
# # Figure 1: motifs
# # ----------------------------------------------------------------------
# mot <- function(n, sp, int) {
#   fsp <- 1:6
#   fsp <- fsp[!fsp %in% sp]
#   fint <- 1:7
#   fint <- fint[!fint %in% int]
#   png(paste0('./Figures/8_',n,'-motifs.png'), res = figres, width = figwd, height = fight, units = "mm")
#   par(mar = c(0, 0, 0, 1), family = 'serif', bg = bg)
#   plot0(x = c(-1.25, 1.25), y = c(-.8, 1.75))
#   interactions(fadeInt = fint)
#   species(fadeSp = fsp)
#   dev.off()
# }

# mot(1, c(2,3,4), c(1,3,4))
# mot(2, c(2,4,6), c(1,5))
# mot(3, c(2,4,5), c(1,6))
# mot(4, c(1,2,4), c(1,2))
# mot(5, c(3,4,6), c(3,5))
# mot(6, c(3,4,5), c(3,6))
# mot(7, c(3,4,1), c(3,2))
# mot(8, c(1,4,6), c(2,5))
# mot(9, c(1,4,5), c(2,6,7))
# mot(10, c(4,5,6), c(5,6))

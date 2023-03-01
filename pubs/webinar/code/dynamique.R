# source('pubs/webinar/code/dynamique.R')
# ----------------------------------------------------------------------
# Param setup
# ----------------------------------------------------------------------
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Libraries & default parameters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
library(graphicsutils)
library(latex2exp)
library(tidyverse)
library(deSolve)
figwd <- 140
fight <- 100
figres <- 250
cexFig <- 1.25
bg <- "#00000000"
colP <- colB <- colM <- col3 <- '#db9318'
col1 <- '#32506A'
colText1 <- '#444458'
colDr <- '#000000'
col2 <- '#986983'
source('./pubs/webinar/code/functions.R')
out <- here::here("pubs","webinar","figures","modules","dynamic")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Disturbances & dynamics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# load('./Data/ParamInit/omParam.RData')
# omParam <- as.data.frame(omParam)
# uid = 88
lwdD <- .75

# # initial state
# init <- c(x = omParam$x[uid],
#           y = omParam$y[uid],
#           z = omParam$z[uid])
# initial state
init <- c(x = 497.6478,
          y = 259.8222,
          z = 155.5680)

# times
times <- seq(0, 50, by = 1)

deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    dx <- x * (r - alpha*x - beta*y - gamma*z)
    dy <- y * (-m_y + mu*beta*x - delta*z)
    dz <- z * (-m_z + nu*gamma*x + omega*delta*y)
    return(list(c(x = dx, y = dy, z = dz)))
  })
}

# # Parameters
# p <- c(r = 1.02,
#    alpha = 0.0009,
#     beta = omParam$beta[uid],
#    delta = omParam$delta[uid],
#    gamma = omParam$gamma[uid],
#       mu = .5,
#       nu = .5,
#    omega = .5,
#      m_y = omParam$m_y[uid],
#      m_z = omParam$m_z[uid])
# Parameters
p <- c(r = 1.0200000000,
   alpha = 0.0009000000,
    beta = 0.0017214390,
   delta = 0.0026447249,
   gamma = 0.0003540844,
      mu = 0.5000000000,
      nu = 0.5000000000,
   omega = 0.5000000000,
     m_y = 0.0169005008,
     m_z = 0.4316837251)

plotDynamic <- function() {
  plot0(x = c(-3.5,60), y = c(-60,625))
  arrows(-2, 0, 59, 0, length = .05, code = 2, xpd = TRUE)
  arrows(-2, 0, -2, 600, length = .05, code = 2, xpd = TRUE)
  # lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
  # lines(x = c(5,5), y = c(-25,0), col = '#000000')
  text(x = 56, y = -15, adj = c(1,1), 'Time', cex = cexFig)
  text(x = -3, y = 275, adj = c(.5,0), TeX('Abundance ($\\textit{a}$)'), cex = cexFig, srt = 90)
  # text(x = 6, y = -42, adj = c(0,1), 'Stressors', cex = .55)
  for(i in 1:3) lines(x = c(0,5), y = rep(init[i], 2), col = '#000000', lwd = 2)
  # arrows(6, -25, 10, -25, length = .025, code = 2, xpd = TRUE)
}

# Disturbances 1
p1 <- p
p1['r'] <- p1['r']*.8
# Solve system of equations
res1 <- ode(init, times, deriv, p1)

# Disturbances 2
p2 <- p
p2['m_y'] <- p2['m_y']*1.15
# Solve system of equations
res2 <- ode(init, times, deriv, p2)

# Disturbances 3
p3 <- p
p3['gamma'] <- p3['gamma']*.75
p3['delta'] <- p3['delta']*.8
# Solve system of equations
res3 <- ode(init, times, deriv, p3)

# Disturbances 4
p4 <- p
p4['r'] <- p4['r']*.8
p4['m_y'] <- p4['m_y']*1.15
p4['gamma'] <- p4['gamma']*.75
p4['delta'] <- p4['delta']*.8
# Solve system of equations
res4 <- ode(init, times, deriv, p4)

# Delta abundances - sensibilité trophique
xI <- init['x']
xE <- res1[50,'x']
xM <- mean(c(xI,xE))


# Sensibilité trophique
sens1 <- apply(res1, 2, function(x) (x[50]-x[1]) / x[1])
sens2 <- apply(res2, 2, function(x) (x[50]-x[1]) / x[1])
sens3 <- apply(res3, 2, function(x) (x[50]-x[1]) / x[1])
sens4 <- apply(res4, 2, function(x) (x[50]-x[1]) / x[1])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot "density"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
plotDensity <- function(sp, range = c(-1,1), dat1 = NULL, dat2 = NULL) {
  par(mar = c(4.25, .5, 0, 0), family = 'serif', bg = bg)
  plot0(x = range, y = c(0,1))
  lines(x = c(0,0), y = c(0,.7), lty = 2, col = '#a2a2a2')
  axis(1, cex.axis = .75)
  if (sp == 'bew') pchImage(x = range[1]+.1, .8, obj = get(sp), cex.x = 1*1.5, cex.y = .9*1.75, col = col1)
  if (sp == 'cod') pchImage(x = range[1]+.1, .8, obj = get(sp), cex.x = .9*1.5, cex.y = .8*1.75, col = col1)
  if (sp == 'cap') pchImage(x = range[1]+.1, .8, obj = get(sp), cex.x = .7*1.5, cex.y = .8*1.75, col = col1)

  if (!is.null(dat1)) {
    points(x = dat1, y = rep(.4, length(dat1)), cex = 2, pch = 20, col = col2)
  }

  if (!is.null(dat2)) {
    points(x = dat2, y = rep(.4, length(dat2)), cex = 2, pch = 20, col = paste0(col2, '77'))
  }

  if (sp == 'bew') mtext('Trophic sensitivity', side = 1, line = 2, cex = .75)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Layout
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
mat <- matrix(nrow = 3, ncol = 4, data = 0)
mat[1:3,1:3] <- 1
mat[1,4] <- 2
mat[2,4] <- 3
mat[3,4] <- 4



# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# # Measure trophic sensitivity, amplification and variance
# Trophic sensitivity
# resTS1 <- (res1[50, 2:4] - res1[1, 2:4])/res1[1,2:4]
# resTS2 <- (res2[50, 2:4] - res2[1, 2:4])/res2[1,2:4]
# resTS3 <- (res3[50, 2:4] - res3[1, 2:4])/res3[1,2:4]
# resTS4 <- (res4[50, 2:4] - res4[1, 2:4])/res4[1,2:4]
#
# # Trophic amplification
# expTS <- resTS4/3
# resTA <- (resTS1-expTS) + (resTS2-expTS) + (resTS3-expTS)
#
# # Trophic amplification
# resTV <- (resTS1-expTS)^2 + (resTS2-expTS)^2 + (resTS3-expTS)^2
# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'1_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = col1)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'2_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)
# Disturbances
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'3_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Disturbances
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Population dynamics
for (i in 2) lines(x = c(5:55), y = res1[,i], lwd = 2)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'4_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Disturbances
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Population dynamics
for (i in 2:4) lines(x = c(5:55), y = res1[,i], lwd = 2)
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 5
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'5_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Sensibilité trophique
lines(x = c(5,55), y = rep(init['x'],2), lty = 4, col = '#000000aa', lwd = lwdD)
lines(x = c(5,55), y = rep(init['y'],2), lty = 4, col = '#000000aa', lwd = lwdD)
lines(x = c(5,55), y = rep(init['z'],2), lty = 4, col = '#000000aa', lwd = lwdD)
arrows(55, xM+(xI-xM)*.35, 55, xI-8.5, length = .025, code = 2, xpd = TRUE, lwd = lwdD)
arrows(55, xM-(xI-xM)*.35, 55, xE+8.5, length = .025, code = 2, xpd = TRUE, lwd = lwdD)
text(x = 55, y = xM, adj = c(.5,.5), labels = TeX('Trophic sensitivity'), cex = cexFig)

# Disturbances
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Population dynamics
for (i in 2:4) lines(x = c(5:55), y = res1[,i], lwd = 2)
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 6
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'6_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Sensibilité trophique
lines(x = c(5,55), y = rep(init['x'],2), lty = 4, col = '#000000aa', lwd = lwdD)
lines(x = c(5,55), y = rep(init['y'],2), lty = 4, col = '#000000aa', lwd = lwdD)
lines(x = c(5,55), y = rep(init['z'],2), lty = 4, col = '#000000aa', lwd = lwdD)
arrows(55, xM+(xI-xM)*.35, 55, xI-8.5, length = .025, code = 2, xpd = TRUE, lwd = lwdD)
arrows(55, xM-(xI-xM)*.35, 55, xE+8.5, length = .025, code = 2, xpd = TRUE, lwd = lwdD)
text(x = 55, y = xM, adj = c(.5,.5), labels = TeX('Trophic sensitivity'), cex = cexFig)

# Disturbances
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Population dynamics
for (i in 2:4) lines(x = c(5:55), y = res1[,i], lwd = 2)

# Density sensitivity
plotDensity('cap', dat1 = sens1[2])
plotDensity('cod', dat1 = sens1[3])
plotDensity('bew', dat1 = sens1[4])
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 7
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'7_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = col1)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = colM)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = col1)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Disturbances
arrows(8, 320, 5.5, 280, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, 335, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

for (i in 2:4) lines(x = c(5:55), y = res2[,i], lwd = 2)

# Density sensitivity
plotDensity('cap', dat1 = sens2[2], dat2 = sens1[2])
plotDensity('cod', dat1 = sens2[3], dat2 = sens1[3])
plotDensity('bew', dat1 = sens2[4], dat2 = sens1[4])
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 8
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'8_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = colB)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = col1)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = col1)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Disturbances
arrows(8, 215, 5.5, 175, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, 230, obj = Shipping, cex.x = .4, cex.y = .5)#, col = colB[1])

for (i in 2:4) lines(x = c(5:55), y = res3[,i], lwd = 2)

# Density sensitivity
plotDensity('cap', dat1 = sens3[2], dat2 = c(sens1[2], sens2[2]))
plotDensity('cod', dat1 = sens3[3], dat2 = c(sens1[3], sens2[3]))
plotDensity('bew', dat1 = sens3[4], dat2 = c(sens1[4], sens2[4]))

dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Plot 9
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
png(here::here(out,'9_dynamic.png'), res = figres, width = figwd+figwd*(1/3), height = fight, units = "mm")
layout(mat)
par(mar = c(.5, .5, 0, 0), family = 'serif', bg = bg)
plotDynamic()
pchImage(x = 1.5, 110, obj = bew, cex.x = .8, cex.y = .5, col = colB)
pchImage(x = 1.5, 220, obj = cod, cex.x = .7, cex.y = .4, col = colM)
pchImage(x = 1.5, 460, obj = cap, cex.x = .5, cex.y = .4, col = colM)
lines(x = c(5,5), y = c(-25,600), lty = 2, col = '#00000088')
lines(x = c(5,5), y = c(-25,0), col = '#000000')
text(x = 6, y = -42, adj = c(0,1), 'Drivers', cex = cexFig)

# Disturbances
# Capelin
arrows(8, 550, 5.5, 520, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, xI+60, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Cod
arrows(8, 225, 5.5, 245, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, 235, obj = DD, cex.x = .4, cex.y = .5)#, col = colB[1])

# Beluga
arrows(8, 75, 5.5, 125, length = .025, code = 2, xpd = TRUE)
pchImage(x = 10, 60, obj = Shipping, cex.x = .4, cex.y = .5)#, col = colB[1])

# # Joint model
lines(x = c(5:55), y = res4[,'x'], lwd = 2)
lines(x = c(5:55), y = res4[,'y'], lwd = 2)
lines(x = c(5:55), y = res4[,'z'], lwd = 2)

# Density sensitivity
plotDensity('cap', dat1 = sens4[2], dat2 = c(sens1[2],sens2[2],sens3[2]))
plotDensity('cod', dat1 = sens4[3], dat2 = c(sens1[3],sens2[3],sens3[3]))
plotDensity('bew', dat1 = sens4[4], dat2 = c(sens1[4],sens2[4],sens3[4]))


dev.off()


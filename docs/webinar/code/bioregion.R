# source('./pubs/webinar/code/bioregion.R')
library(slmeta)
aoi <- sf::st_read("data/data-raw/federal_marine_bioregions-f635934a/federal_marine_bioregions-f635934a.geojson")
stl <- aoi[aoi$NAME_E == "Gulf of Saint Lawrence",]
ss <- aoi[aoi$NAME_E == "Scotian Shelf",]
nfl <- aoi[aoi$NAME_E == "Newfoundland-Labrador Shelves",]

### Colors
grn    <- '#68908b'
focus  <- '#25364A'
off    <- '#C7CBCE'
border <- '#f4f4f4'
names  <- slmetaPal('platform')[8]
pal <- colorRampPalette(slmetaPal('platform'))
bg <- '#00000000'
ext <- c(-830844.6,  -350000, 1500000, 2214428.7)

png('./pubs/webinar/figures/bioregions.png', res = 250, width = 120, height = 150, units = "mm")
par(bg = bg)
plotEGSL(layers = c('nfl','ss','stl','quebec'),
         prj        = slmetaPrj('default'),
         extent     = ext,
         cols       = c(grn, grn, grn, focus),
         borders    = border,
         lwds       = 1.25,
         mar        = c(0,0,0,0),
         box        = FALSE,
         axes       = NULL,
         background = "#00000000",
         graticules = NULL,
         scale      = FALSE,
         northArrow = FALSE,
         prjText    = FALSE,
         prjCol     = NULL)
dev.off()
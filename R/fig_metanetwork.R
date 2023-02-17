#' Export figures for metanetwork
#'
#' @export
fig_metanetwork <- function() {
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(magrittr)
  library(raster)
  library(tidyverse)
  library(graphicsutils)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors for stressors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    col <- hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |> 
           lapply(graphicsutils::darken, percentage = 10) |>
           unlist() 
    glue::glue("{col}80")
  }
  
  # Colors for species 
  colInv <- '#226c61'
  colVer <- '#6a5024'
  
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Effects per km2
  cekm <- read.csv(here::here("output","cea_km2","cea_km2-2010_2015.csv"))
  
  # Drivers list
  drList <- read.csv(here::here("data","cea_modules","drivers_list.csv")) |>
            dplyr::select(-period) |>
            dplyr::distinct() |>
            dplyr::filter(drivers != "InvasiveSpecies")
  drGroup <- dplyr::select(drList, group) |>
             dplyr::distinct() |>
             dplyr::mutate(cols = gg_color_hue(dplyr::n()))
  drList <- dplyr::left_join(drList, drGroup, by = "group") 
  nDrGroup <- nrow(drGroup)
  
  # Species list
  spList <- read.csv(here::here("data","cea_modules","species_list.csv"))
  spGroup <- dplyr::select(spList, gr1, gr2) |>
             dplyr::distinct() |> 
             dplyr::arrange(gr1, gr2) |>
             dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer))
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Organize data
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Reorder data
  cekm <- left_join(cekm, spList, by = c("Taxa" = "shortname")) |>
          arrange(gr1, gr2, desc(cea_network))

  # Groups
  gr1 <- cekm[, 'gr1', drop = FALSE] |>
         dplyr::mutate(id = 1:dplyr::n()) |>
         dplyr::group_by(gr1) |>
         dplyr::summarize(min = min(id), max = max(id)) |>
         as.data.frame(stringsAsFactors = FALSE)

  gr2 <- cekm[, 'gr2', drop = FALSE] %>%
         dplyr::mutate(id = 1:dplyr::n()) %>%
         dplyr::group_by(gr2) %>%
         dplyr::summarize(min = min(id), max = max(id)) %>%
         as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub('Others2','Others',gr2$gr2)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Metaweb
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  load(here::here("data","format_modules","metaweb.RData"))
  metaweb <- metaweb[cekm$Taxa, cekm$Taxa]

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for metaweb
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Extract links from metaweb
  uid <- which(metaweb == 1, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c('from','to')))
  for(i in 1:nrow(uid)) {
    links[i, 'from'] <- colnames(metaweb)[uid[i,1]]
    links[i, 'to'] <- colnames(metaweb)[uid[i,2]]
  }
  linksTx <- as.data.frame(links)

  # Add color for links
  # linksTx$cols <- '#99836211'
  linksTx$cols <- '#f5f5f5'

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes list with proper groups included
  nodesTx <- data.frame(
    group = cekm$gr1, 
    network = cekm$gr2, 
    name = cekm$Taxa,
    cex = cekm$cea_network*1.25
  )

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Add colors to node now
  metanetwork <- list()
  metanetwork$nodes <- nodesTx
  metanetwork$links <- linksTx

  # Colors
  metanetwork$networkGroup <- bound(metanetwork, order = spGroup$gr2)
  metanetwork <- colGroups(metanetwork, colPal = spGroup$col)
  nodesTx <- metanetwork[[1]]
  colGr <- metanetwork[[3]][,c('Var1','cols')]


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for drivers
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Identify columns with total effects per stressor
  uid <- stringr::str_detect(colnames(cekm), 'Total_Effect')
  dat <- cekm[, uid]
  colnames(dat) <- gsub('_Total_Effect','', colnames(dat))
  dat <- round(dat, 2)

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Links with taxa
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # For now, only select links > 0.05
  uid <- which(dat > 0.05, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c('from','to')))
  for(i in 1:nrow(uid)) {
    links[i, 'from'] <- colnames(dat)[uid[i,2]]
    links[i, 'to'] <- cekm$Taxa[uid[i,1]]
  }
  linksDr <- as.data.frame(links, stringsAsFactors = FALSE)

  # Add color for links
  linksDr <- left_join(linksDr, drList[,c('drivers','cols')], by = c('from' = 'drivers'))

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Node size
  cexDr <- data.frame(
    drivers = colnames(dat), 
    cex = log(colMeans(dat, na.rm = TRUE)+1)*10+.4
  )

  nodesDr <- dplyr::select(
    drList, 
    network = group,
    name = drivers,
    cols
  ) |>
  dplyr::mutate(group = "Stressors") |>
  dplyr::left_join(cexDr, by = c("name" = "drivers")) |>
  dplyr::select(group, network, name, cex, cols)
  
  # # Add transparent nodes for spacing
  # randomString <- function() paste0(letters[runif(20,1,26)], collapse = '')
  # # Marine traffic
  # x <- data.frame(group = 'Stressors', network = 'Marine traffic', name = randomString(), cex = 0, cols = '#00000000', stringsAsFactors = FALSE)
  # y <- nodesDr
  # nodesDr <- rbind(y[1:16,], x, x, y[17,], x, x, x, x, x, y[18, ], x, x)
  # 
  # # Fisheries
  # x <- data.frame(group = 'Stressors', network = 'Fisheries', name = randomString(), cex = 0, cols = '#00000000', stringsAsFactors = FALSE)
  # y <- nodesDr
  # nodesDr <- rbind(y[1:11,], y[12,], x, x, y[13, ], x, x, y[14, ], x, x, y[15, ], x, x, y[16, ], y[17:nrow(nodesDr), ])
  # 
  # # Coastal
  # x <- data.frame(group = 'Stressors', network = 'Coastal', name = randomString(), cex = 0, cols = '#00000000', stringsAsFactors = FALSE)
  # y <- nodesDr
  # nodesDr <- rbind(y[1:6,], y[7,], x, x, y[8, ], x, x, y[9, ], x, x, y[10, ], x, x, y[11, ], y[12:nrow(nodesDr), ])
  # 
  # # Climate
  # x <- data.frame(group = 'Stressors', network = 'Climate', name = randomString(), cex = 0, cols = '#00000000', stringsAsFactors = FALSE)
  # y <- nodesDr
  # nodesDr <- rbind(y[1,], x, x, y[2,], x, x, y[3, ], x, x, y[4, ], x, x, y[5, ], x, x, y[6, ], y[7:nrow(nodesDr), ])

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine in a single object
  metanetwork <- vector('list', 0)
  metanetwork$nodes <- rbind(nodesDr, nodesTx)
  metanetwork$links <- rbind(linksTx, linksDr)


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Network order
  orderNet <- c(unique(nodesDr$network), unique(nodesTx$network))

  # Network boundaries
  metanetwork$networkGroup <- bound(metanetwork, order = orderNet)

  # Node coordinates
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .6)

  # Manually add colors
  # metanetwork[[3]]$cols <- NA
  metanetwork[[3]] <- dplyr::left_join(
    metanetwork[[3]], 
    drGroup[, c('group','cols')], 
    by = c('Var1' = 'group')
  )
  metanetwork[[3]]$cols[match(colGr$Var1, metanetwork[[3]]$Var1)] <- colGr$cols

  # Others
  rad1 = .925
  rad2 = 1
  shadowEdge = TRUE

  out <- here::here("figures","metanetwork")
  chk_create(out)
  png(here::here(out,"metanetwork.png"), res = 300, width = 300, height = 300 ,units = "mm")
  # Plot
  par(mar = c(2,2,2,2))
  plot0(x = c(-1.1, 1.1))

  metanetwork$networkGroup$Var1[12] <- 'Others'
  boxGroup(metanetwork,
           rad1 = rad1,
           colBox = metanetwork$networkGroup$cols,
           colNames = metanetwork$networkGroup$colNames,
           border = 'transparent',
           # border = '#000000',
           cexNetwork = .75)

  plotLinks(metanetwork, col = metanetwork$links$cols)

  if (shadowEdge) {
    points(metanetwork$nodes$x,
           metanetwork$nodes$y,
           pch = 20,
           cex = (metanetwork$nodes$cex * 5),
           col = '#d7d7d7')
  }

  points(metanetwork$nodes$x,
         metanetwork$nodes$y,
         pch = 20,
         cex = (metanetwork$nodes$cex * 3),
         col = metanetwork$nodes$cols)

  # dev.off()



  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Extra elements to graph
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Add Vertebrates, Invertebrates and Stressors
  metanetwork$nodes$network <- metanetwork$nodes$group
  metanetwork$networkGroup <- bound(metanetwork, order = unique(metanetwork$nodes$network))
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)

  boxGroup2(metanetwork,
           rad1 = 1.03, rad2 = 1.13,
           colBox = '#00000000', colNames = '#000000',
           border = '#000000',
           cexNetwork = 1.1)

  # Letter
  text(x = -1.1, y = 1.065, labels = 'b', cex = 1.5, font = 2)

  dev.off()
}

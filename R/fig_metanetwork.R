#' Export figures for metanetwork
#'
#' @describeIn fig_metanetwork metanetwork per period
#' @export
fig_metanetwork_ <- function() {
  out <- here::here("figures", "metanetwork")

  # 2010-2015
  # Net
  fig_metanetwork(
    cekm = read.csv(here::here("output", "cea_km2", "ncea_km2_2010_2015.csv")),
    out = out,
    nm = "metanetwork-Total_2010_2015.png"
  )

  # Direct
  fig_metanetwork(
    cekm = read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2010_2015.csv")),
    out = out,
    nm = "metanetwork-Direct_2010_2015.png"
  )

  # Indirect
  fig_metanetwork(
    cekm = read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2010_2015.csv")),
    out = out,
    nm = "metanetwork-Indirect_2010_2015.png"
  )

  # 2016-2021
  # Net
  cekm <- read.csv(here::here("output", "cea_km2", "ncea_km2_2016_2021.csv"))
  fig_metanetwork(
    cekm = cekm,
    out = out,
    nm = "metanetwork-Total_2016_2021.png"
  )
  fig_metanetwork(
    cekm = cekm,
    bg = "#00000000",
    out = here::here("figures", "webinar"),
    nm = "metanetwork-Total_2016_2021.png"
  )

  # Direct
  cekm <- read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2016_2021.csv"))
  fig_metanetwork(
    cekm = cekm,
    out = out,
    nm = "metanetwork-Direct_2016_2021.png"
  )
  fig_metanetwork(
    cekm = cekm,
    bg = "#00000000",
    out = here::here("figures", "webinar"),
    nm = "metanetwork-Direct_2016_2021.png"
  )

  # Indirect
  cekm <- read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2016_2021.csv"))
  fig_metanetwork(
    cekm = cekm,
    out = out,
    nm = "metanetwork-Indirect_2016_2021.png"
  )
  fig_metanetwork(
    cekm = cekm,
    bg = "#00000000",
    out = here::here("figures", "webinar"),
    nm = "metanetwork-Indirect_2016_2021.png"
  )
}

#' Export figures for metanetwork
#'
#' @describeIn fig_metanetwork metanetwork figure
#' @export
fig_metanetwork <- function(cekm, bg = "#ffffff", out = here::here("figures", "metanetwork"), nm = "metanetwork.png") {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(magrittr)
  library(raster)
  library(tidyverse)
  library(graphicsutils)
  global_parameters()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Impacts per km2
  cekm <- cekm |>
    dplyr::filter(!species %in% c("phytoplankton", "zooplankton")) |>
    dplyr::rowwise() |>
    dplyr::mutate(ncea = sum(
      dplyr::across(!species),
      na.rm = TRUE
    )) |>
    # dplyr::mutate(ncea = ncea / max(ncea)) |>
    dplyr::mutate(ncea = log(ncea + 1) * .5 + .1) |>
    dplyr::rename(Taxa = species) |>
    dplyr::mutate(Taxa = tools::file_path_sans_ext(Taxa))

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors for stressors
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    col <- hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |>
      lapply(graphicsutils::darken, percentage = 30) |>
      unlist()
    glue::glue("{col}80")
  }

  # Colors for species
  colInv <- "#226c61BB"
  colVer <- "#6a5024BB"

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Function to add transparent nodes for spacing between each node groups
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  randomString <- function() paste0(letters[runif(20, 1, 26)], collapse = "")
  insertRow <- function(dat, group, network) {
    # New row to add
    newrow <- data.frame(
      group = group,
      network = network,
      name = randomString(),
      cex = 0,
      cols = "#00000000"
    )

    # ID in data.frame where to add rows
    uid <- which(dat$network == network) |>
      sort(decreasing = TRUE)

    # Add rows
    for (i in uid) {
      dat <- dplyr::add_row(dat, newrow, .after = i)
      dat <- dplyr::add_row(dat, newrow, .before = i)
    }

    # Return
    dat
  }

  insertRow_ <- function(dat, group, network, nrep) {
    for (i in 1:nrep) {
      dat <- insertRow(dat, group, network)
    }
    dat
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Plotting function
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  arctext2 <- function(var1, l1, l2, cl = TRUE, cx = .9) {
    uid <- metanetwork$networkGroup$Var1 == var1
    middle <- mean(c(
      metanetwork$networkGroup$lower[uid],
      metanetwork$networkGroup$upper[uid]
    ))
    plotrix::arctext(
      x = as.character(l1), radius = rad2 - .02, middle = middle,
      col = "#ffffff", clockwise = cl, font = 2, cex = cx
    )
    plotrix::arctext(
      x = as.character(l2), radius = rad1 + .02, middle = middle,
      col = "#ffffff", clockwise = cl, font = 2, cex = cx
    )
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Drivers list
  drList <- read.csv(here::here("data", "cea_modules", "drivers_list.csv")) |>
    dplyr::select(-period) |>
    dplyr::distinct() |>
    dplyr::filter(drivers != "InvasiveSpecies")
  drGroup <- dplyr::select(drList, group) |>
    dplyr::distinct() |>
    dplyr::mutate(cols = gg_color_hue(dplyr::n()))
  drList <- dplyr::left_join(drList, drGroup, by = "group")
  nDrGroup <- nrow(drGroup)

  # Species list
  spList <- read.csv(here::here("data", "cea_modules", "species_list.csv"))
  spGroup <- dplyr::select(spList, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2) |>
    dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer))

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Organize data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Reorder data
  cekm <- left_join(cekm, spList, by = c("Taxa" = "shortname")) |>
    arrange(gr1, gr2, desc(ncea))

  # Groups
  gr1 <- cekm[, "gr1", drop = FALSE] |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    dplyr::group_by(gr1) |>
    dplyr::summarize(min = min(id), max = max(id)) |>
    as.data.frame(stringsAsFactors = FALSE)

  gr2 <- cekm[, "gr2", drop = FALSE] %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::group_by(gr2) %>%
    dplyr::summarize(min = min(id), max = max(id)) %>%
    as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub("Others2", "Others", gr2$gr2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Metaweb
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  load(here::here("data", "format_modules", "metaweb.RData"))
  metaweb <- metaweb[cekm$Taxa, cekm$Taxa]
  # colnames(metaweb) <- rownames(metaweb) <- cekm$Taxa

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for metaweb
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Extract links from metaweb
  uid <- which(metaweb == 1, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c("from", "to")))
  for (i in 1:nrow(uid)) {
    links[i, "from"] <- colnames(metaweb)[uid[i, 1]]
    links[i, "to"] <- colnames(metaweb)[uid[i, 2]]
  }
  linksTx <- as.data.frame(links)

  # Add color for links
  # linksTx$cols <- '#99836211'
  linksTx$cols <- "#e3e1e1"

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes list with proper groups included
  nodesTx <- data.frame(
    group = cekm$gr1,
    network = cekm$gr2,
    name = cekm$Taxa,
    cex = cekm$ncea * .6
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
  colGr <- metanetwork[[3]][, c("Var1", "cols")]


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Link and nodes for drivers
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Identify columns with total effects per stressor
  uid <- colnames(cekm) %in% drList$drivers
  dat <- cekm[, uid]
  dat <- round(dat, 2)

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Links with taxa
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # For now, only select links > 0.05
  uid <- which(dat > 0.00, arr.ind = T)
  links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c("from", "to")))
  for (i in 1:nrow(uid)) {
    links[i, "from"] <- colnames(dat)[uid[i, 2]]
    links[i, "to"] <- cekm$Taxa[uid[i, 1]]
  }
  linksDr <- as.data.frame(links, stringsAsFactors = FALSE)

  # Add color for links
  linksDr <- left_join(linksDr, drList[, c("drivers", "cols")], by = c("from" = "drivers"))

  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Nodes
  # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Node size
  cexDr <- data.frame(
    drivers = colnames(dat),
    cex = log(colMeans(dat, na.rm = TRUE) + 1) * 1 + .2
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

  # Add transparent nodes for spacing
  nodesDr <- insertRow_(nodesDr, "Stressors", "Marine traffic", 2)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Fisheries", 1)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Coastal", 1)
  nodesDr <- insertRow_(nodesDr, "Stressors", "Climate", 1)

  # Remove last line if empty
  ll <- nrow(nodesDr)
  if (is.na(nodesDr$group[ll])) nodesDr <- nodesDr[-ll, ]

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Combine in a single object
  metanetwork <- vector("list", 0)
  metanetwork$nodes <- rbind(nodesDr, nodesTx)
  metanetwork$links <- rbind(linksTx, linksDr)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
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
    drGroup[, c("group", "cols")],
    by = c("Var1" = "group")
  )
  metanetwork[[3]]$cols[match(colGr$Var1, metanetwork[[3]]$Var1)] <- colGr$cols

  # Others
  rad1 <- .925
  rad2 <- 1
  shadowEdge <- TRUE

  # Output
  rcea::chk_create(out)

  png(
    here::here(out, nm),
    res = 300,
    width = 300,
    height = 300,
    units = "mm"
  )

  # Plot
  par(mar = c(2, 2, 2, 2), bg = bg)
  plot0(x = c(-1.1, 1.1))

  # Adjust some group names
  uid <- metanetwork$networkGroup$Var1 == "Others2"
  metanetwork$networkGroup$Var1[uid] <- "Others"
  uid <- metanetwork$networkGroup$Var1 == "Marine traffic"
  metanetwork$networkGroup$Var1[uid] <- "."
  boxGroup(metanetwork,
    rad1 = rad1,
    colBox = metanetwork$networkGroup$cols,
    colNames = "#ffffff",
    border = "transparent",
    # border = '#000000',
    cexNetwork = .9
  )
  arctext2(".", "Marine", "traffic")

  plotLinks(metanetwork, cols = metanetwork$links$cols, lwd = 0.5)

  if (shadowEdge) {
    points(metanetwork$nodes$x,
      metanetwork$nodes$y,
      pch = 20,
      cex = (metanetwork$nodes$cex * 3),
      col = "#d7d7d7"
    )
  }

  points(metanetwork$nodes$x,
    metanetwork$nodes$y,
    pch = 20,
    cex = (metanetwork$nodes$cex * 2),
    col = metanetwork$nodes$cols
  )

  # Add Vertebrates, Invertebrates and Stressors
  metanetwork$nodes$network <- metanetwork$nodes$group
  metanetwork$networkGroup <- bound(metanetwork, order = unique(metanetwork$nodes$network))
  metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)

  boxGroup2(metanetwork,
    rad1 = 1.03, rad2 = 1.13,
    colBox = "#00000000", colNames = "#000000",
    border = "#000000",
    cexNetwork = 1.25
  )

  dev.off()
}

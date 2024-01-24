# source("pubs/webinar/code/fig_contribution_group.R")

contribution_webinar <- function(direct, indirect, suffix = "", type = NULL) {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(magrittr)
  library(raster)
  library(tidyverse)
  library(graphicsutils)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Colors for stressors
  gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n] |>
      lapply(graphicsutils::darken, percentage = 10) |>
      unlist()
  }
  add_alpha <- function(alpha) {
    as.hexmode(as.integer(alpha))
  }

  # Colors for species
  colInv <- "#226c61"
  colVer <- "#6a5024"

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
  drList <- dplyr::left_join(drList, drGroup, by = "group") |>
    dplyr::group_by(group) |>
    dplyr::mutate(alpha = add_alpha(seq(255, 100, length.out = n()))) |>
    dplyr::ungroup() |>
    dplyr::mutate(cols = glue::glue("{cols}{alpha}"))
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
  orgdat <- function(dat) {
    dat |>
      dplyr::filter(!species %in% c("phytoplankton", "zooplankton")) |>
      dplyr::rowwise() |>
      dplyr::mutate(ncea = sum(
        dplyr::across(!species),
        na.rm = TRUE
      )) |>
      # dplyr::mutate(ncea = ncea / max(ncea)) |>
      # dplyr::mutate(ncea = log(ncea + 1) * .5 + .1) |>
      dplyr::rename(Taxa = species) |>
      dplyr::mutate(Taxa = tools::file_path_sans_ext(Taxa)) |>
      dplyr::left_join(spList, by = c("Taxa" = "shortname")) |>
      dplyr::arrange(gr1, gr2, desc(ncea))
  }
  direct <- orgdat(direct)
  indirect <- orgdat(indirect)

  # Groups
  gr1 <- direct[, "gr1", drop = FALSE] |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    dplyr::group_by(gr1) |>
    dplyr::summarize(min = min(id), max = max(id)) |>
    as.data.frame(stringsAsFactors = FALSE)

  gr2 <- direct[, "gr2", drop = FALSE] %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::group_by(gr2) %>%
    dplyr::summarize(min = min(id), max = max(id)) %>%
    as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub("Others2", "Others", gr2$gr2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Prepare data for graphs
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Param
  xG <- .2
  yG <- .75

  # Direct & indirect stacked bars
  di <- direct[, c(drList$drivers, "gr1", "gr2")] %>%
    # mutate(gr1 = direct$gr1, gr2 = direct$gr2) %>%
    group_by(gr1, gr2) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    select(-gr1, -gr2) %>%
    t() %>%
    as.data.frame() %>%
    cumsum() %>%
    t() %>%
    cbind(temp = 0, .) %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    gather("drivers", "ymax", -id) %>%
    arrange(id) %>%
    mutate(ymin = c(0, ymax[1:(length(ymax) - 1)])) %>%
    filter(drivers != "temp") %>%
    mutate(
      ymax = ymax + yG, ymin = ymin + yG,
      xmax = id + xG, xmin = id - xG
    ) %>%
    left_join(drList[, c("drivers", "cols")], by = "drivers")

  ind <- indirect[, c(drList$drivers, "gr1", "gr2")] %>%
    # mutate(gr1 = cekm$gr1, gr2 = cekm$gr2) %>%
    group_by(gr1, gr2) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    select(-gr1, -gr2) %>%
    t() %>%
    as.data.frame() %>%
    cumsum() %>%
    t() %>%
    -. %>%
    cbind(temp = 0, .) %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    gather("drivers", "ymax", -id) %>%
    arrange(id) %>%
    mutate(ymin = c(0, ymax[1:(length(ymax) - 1)])) %>%
    filter(drivers != "temp") %>%
    mutate(
      ymax = ymax - yG, ymin = ymin - yG,
      xmax = id + xG, xmin = id - xG
    ) %>%
    left_join(drList[, c("drivers", "cols")], by = "drivers")

  # Maximum values
  maxVals <- c(
    max(di$ymax, na.rm = TRUE),
    min(ind$ymin, na.rm = TRUE)
  )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  out <- here::here("pubs", "webinar", "figures", "results")
  chk_create(out)

  bg <- "#00000000"
  png(
    here::here(out, glue::glue("{suffix}-contribution_group.png")),
    res = 300,
    width = 200,
    height = 225,
    units = "mm"
  )
  layout(matrix(1:2, nrow = 2), heights = c(.88, .12))
  par(family = "serif", bg = bg)
  par(mar = c(0, 1, 1, 0))

  yMax <- maxVals[1] * 1.25 # 1.11
  yMin <- maxVals[2] * 1.5 #-.85
  graphicsutils::plot0(x = c(-1, (nrow(spGroup) + 1)), y = c(yMin, yMax))

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Axes
  # dL <- c(yG, max(di$ymax)+yG) # This is the reproducible code
  dL <- c(yG, yMax * .9) # This is so periods have same axes (not reproducible)
  # iL <- c(-yG, min(ind$ymax)-yG) # This is the reproducible code
  iL <- c(-yG, yMin * .74) # This is so periods have same axes (not reproducible)
  # Polygons
  polygon(x = c(.6, .6, 5.4, 5.4), y = c(dL[2], rep(iL[2], 2), dL[2]), col = "#f5f4f4", border = "#00000000")
  # Lines
  lines(x = c(0.45, 0.45), y = dL, lwd = 1.5)
  lines(x = c(0.45, 0.45), y = iL, lwd = 1.5)
  lines(x = c(-.1, -.1), y = c(iL[2], dL[2]), lwd = 1.5)
  lines(x = c(.5, 8 + .5), y = c(0, 0), lty = 2)
  # Text
  text(x = .25, y = mean(dL), labels = "Direct", srt = 90, adj = .5, font = 1)
  text(x = .25, y = mean(iL), labels = "Indirect", srt = 90, adj = .5, font = 1)
  text(x = -.3, y = mean(c(iL[2], dL[2])), labels = "Total", srt = 90, adj = .5, font = 1)
  text(x = -.8, y = mean(c(iL[2], dL[2])), labels = "Mean contribution of stressors to cumulative effects", srt = 90, adj = .5, font = 2)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Contribution
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # axis(1);axis(2)
  # Direct effect
  if ("direct" %in% type) {
    for (i in 1:nrow(di)) {
      x <- c(di$xmin[i], di$xmax[i], di$xmax[i], di$xmin[i])
      y <- c(rep(di$ymin[i], 2), rep(di$ymax[i], 2))
      polygon(x = x, y = y, border = "#00000000", col = di$cols[i])
    }
  }

  # Indirect effect
  if ("indirect" %in% type) {
    for (i in 1:nrow(ind)) {
      x <- c(ind$xmin[i], ind$xmax[i], ind$xmax[i], ind$xmin[i])
      y <- c(rep(ind$ymin[i], 2), rep(ind$ymax[i], 2))
      polygon(x = x, y = y, border = "#00000000", col = di$cols[i])
    }
  }


  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Taxonomic groups
  x <- c(.6, 5.4)
  lines(y = rep(yMax * .91, 2), x = x)
  text(y = yMax * .95, x = mean(x), adj = .5, font = 2, labels = "Invertebrates")
  x <- c(5.6, 8.4)
  lines(y = rep(yMax * .91, 2), x = x)
  text(y = yMax * .95, x = mean(x), adj = .5, font = 2, labels = "Vertebrates")

  # #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Group names
  for (i in 1:nrow(spGroup)) {
    text(x = i, y = yMin * .75, labels = spGroup$gr2[i], adj = c(1, .5), srt = 90, cex = 1)
  }


  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Drivers legend
  yG <- .3
  par(mar = c(.5, 0, 0, 0), bg = bg)
  plot0(x = c(1, 7), y = c(0.2, -6.5))
  # Groups names
  x <- c(1, 2.9, 3.95, 6.05)
  text(x = x, y = rep(0.15, 4), labels = drGroup$group, font = 2, adj = c(0, .5), cex = .85)
  # Groups
  for (j in 1:nrow(drList)) {
    dat <- drList[drList$group == drGroup$group[j], ]
    dat$id <- -(1:nrow(dat))
    for (i in 1:nrow(dat)) {
      xi <- c(x[j] + .05, rep(x[j] + .15, 2), x[j] + .05)
      y <- c(rep((dat$id[i] - yG), 2), rep((dat$id[i] + yG), 2))
      polygon(x = xi, y = y, border = "#585858", col = dat$cols[i])
      text(x = x[j] + .2, y = dat$id[i], labels = dat$fullname[i], cex = .6, adj = c(0, .5))
    }
  }
  dev.off()
}

direct <- read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2016_2021.csv"))
indirect <- read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2016_2021.csv"))
contribution_webinar(direct, indirect, "1")
contribution_webinar(direct, indirect, "2", "direct")
contribution_webinar(direct, indirect, "3", c("direct", "indirect"))

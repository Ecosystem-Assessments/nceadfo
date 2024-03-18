#' Export figures for stressor contribution to direct and indirect effects per period
#'
#' @describeIn fig_contribution_taxa contribution stacked barplot per period
#' @export
fig_contribution_taxa_ <- function() {
  # 2010-2015
  direct <- read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2010_2015.csv"))
  indirect <- read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2010_2015.csv"))
  fig_contribution_taxa(direct, indirect, "2010_2015")

  # 2016-2021
  direct <- read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2016_2021.csv"))
  indirect <- read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2016_2021.csv"))
  fig_contribution_taxa(direct, indirect, "2016_2021")
}

#' Export figures for stressor contribution to direct and indirect effects
#'
#' @describeIn fig_contribution_taxa contribution stacked barplot
#' @export
fig_contribution_nceahab <- function(direct, indirect, period) {
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

  # Colors for habitats
  colHab <- c("#535d8aFF", "#535d8aBB", "#535d8a77", "#535d8a44")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data lists
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Drivers list
  drList <- read.csv(here::here("data", "cea_modules", "drivers_list.csv")) |>
    dplyr::select(-period) |>
    dplyr::distinct() |>
    dplyr::filter(drivers != "InvasiveSpecies")
  drGroup <- dplyr::select(drList, group) |>
    dplyr::distinct() |>
    dplyr::mutate(col = gg_color_hue(dplyr::n()))
  drList <- dplyr::left_join(drList, drGroup, by = "group") |>
    dplyr::group_by(group) |>
    dplyr::mutate(alpha = add_alpha(seq(255, 100, length.out = n()))) |>
    dplyr::ungroup() |>
    dplyr::mutate(col = glue::glue("{col}{alpha}"))
  nDrGroup <- nrow(drGroup)

  # Species list
  spList <- read.csv(here::here("data", "cea_modules", "species_list.csv")) |>
    dplyr::select(-aphiaID, -rsq, -gr3) |>
    dplyr::rename(fullname = scientific_name) |>
    dplyr::arrange(gr1, gr2)
  spGroup <- dplyr::select(spList, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2) |>
    dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer))

  # Habitats list
  habGroups <- list(
    data.frame(
      gr2 = "Benthic",
      h = c("SSHLF", "HSHLF", "MSHLF", "CYN", "SDEEP", "HDEEP", "MDEEP", "DPBIO"),
      col = colHab[1]
    ),
    data.frame(
      gr2 = "Nearshore",
      h = c("BITDL", "MFITDL", "RITDL", "SSHLW", "HSHLW", "MSHLW", "SALTMARSH", "SEAG", "ALGAL", "KELP", "HMUSSEL"),
      col = colHab[2]
    ),
    data.frame(
      gr2 = "Pelagic (shallow)",
      h = c("SPELAGIC"),
      col = colHab[4]
    ),
    data.frame(
      gr2 = "Pelagic (deep)",
      h = c("DPELAGIC"),
      col = colHab[3]
    )
  ) |>
    dplyr::bind_rows()
  habList <- read.csv(here::here("data", "cea_modules", "habitats_list.csv")) |>
    dplyr::left_join(habGroups, by = c("HabitatCODE" = "h")) |>
    dplyr::mutate(
      gr1 = "Habitats"
    ) |>
    dplyr::select(gr1, gr2, shortname = HabitatCODE, col, fullname = Habitat) |>
    dplyr::arrange(gr1, gr2)
  habGroup <- dplyr::select(habList, gr1, gr2, col) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2)

  # Valued components list
  cvList <- list(spList, habList) |>
    dplyr::bind_rows()
  cvGroups <- list(spGroup, habGroup) |>
    dplyr::bind_rows()

  # Drivers list for indirect effects
  drListInd <- dplyr::bind_rows(
    dplyr::select(drList, drivers, col, group, fullname),
    dplyr::select(habGroup, drivers = gr2, col, group = gr1, fullname = gr2)
  )
  drGroup <- dplyr::bind_rows(
    drGroup,
    data.frame(group = "Habitats", col = colHab[1])
  )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  period <- "2016_2021"
  # Load and checks
  direct <- read.csv(here::here("output", "cea_km2", "ncea_direct_km2_2016_2021.csv"))
  indirect <- read.csv(here::here("output", "cea_km2", "ncea_indirect_km2_2016_2021.csv"))
  hab <- read.csv(here::here("output", "cea_km2", "cea_indirect_habitats_km2_2016_2021.csv"))
  direct$name <- tools::file_path_sans_ext(direct$name)
  indirect$name <- tools::file_path_sans_ext(indirect$name)
  direct <- direct[match(direct$name, hab$name), ]
  indirect <- indirect[match(indirect$name, hab$name), ]
  stopifnot(all(direct$name == indirect$name))
  stopifnot(all(direct$name == hab$name))

  # Normalize data for combinations
  nm <- direct$name
  direct <- dplyr::select(direct, -name)
  indirect <- dplyr::select(indirect, -name)
  hab <- dplyr::select(hab, -name)

  maxValSp <- max(cbind(direct, indirect))
  direct <- (direct / maxValSp) * 2
  indirect <- (indirect / maxValSp) * 2
  maxValHb <- max(hab, na.rm = TRUE)
  hab <- hab / maxValHb

  direct$name <- nm
  indirect$name <- nm
  hab$name <- nm

  # Combine indirect effects from habitats into groups
  hab <- tidyr::pivot_longer(hab, !name, names_to = "from", values_to = "cea") |>
    dplyr::left_join(habList[, c("gr2", "shortname")], by = c("from" = "shortname")) |>
    dplyr::group_by(name, gr2) |>
    dplyr::summarise(cea = sum(cea, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(id_cols = "name", names_from = "gr2", values_from = "cea", values_fill = 0)

  # Combine all indirect effects to species
  indirect <- dplyr::left_join(indirect, hab, by = "name")

  # Normalize again all effects to species between 0 and 1 to allow relative comparison with effects to habitats
  direct <- dplyr::mutate(direct, dplyr::across(dplyr::where(is.numeric), ~ .x / 2))
  indirect <- dplyr::mutate(indirect, dplyr::across(dplyr::where(is.numeric), ~ .x / 2))

  # Effects to habitats
  habitats <- read.csv(here::here("output", "cea_km2", "cea_habitats_km2_2016_2021.csv"))
  maxVal <- dplyr::select(habitats, -name) |> max()
  habitats <- dplyr::mutate(habitats, dplyr::across(dplyr::where(is.numeric), ~ .x / maxVal))

  # Add to direct effects
  direct <- dplyr::bind_rows(direct, habitats)

  # Add empty values to indirect effects (no indirect effects to habitats considered)
  indirect <- dplyr::bind_rows(
    indirect,
    dplyr::mutate(habitats, dplyr::across(dplyr::where(is.numeric), ~ .x * 0))
  )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Organize data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  orgdat <- function(dat) {
    dat |>
      dplyr::filter(!name %in% c("phytoplankton", "zooplankton")) |>
      dplyr::rowwise() |>
      dplyr::mutate(ncea = sum(
        dplyr::across(!name),
        na.rm = TRUE
      )) |>
      # dplyr::mutate(ncea = ncea / max(ncea)) |>
      # dplyr::mutate(ncea = log(ncea + 1) * .5 + .1) |>
      dplyr::rename(Taxa = name) |>
      dplyr::mutate(Taxa = tools::file_path_sans_ext(Taxa)) |>
      dplyr::left_join(cvList, by = c("Taxa" = "shortname")) |>
      dplyr::arrange(gr1, gr2, desc(ncea))
  }
  direct <- orgdat(direct)
  indirect <- orgdat(indirect)

  # Groups
  gr1 <- direct[, "gr1", drop = FALSE] |>
    dplyr::ungroup() |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    dplyr::group_by(gr1) |>
    dplyr::summarize(min = min(id), max = max(id)) |>
    as.data.frame(stringsAsFactors = FALSE)

  gr2 <- direct[, "gr2", drop = FALSE] %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::group_by(gr2) %>%
    dplyr::summarize(min = min(id), max = max(id)) %>%
    as.data.frame(stringsAsFactors = FALSE)
  gr2$gr2 <- gsub("Others2", "Others", gr2$gr2)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Prepare data for graphs
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Param
  xG <- .3
  yG <- .05

  # Direct & indirect stacked bars
  di <- direct[, drList$drivers] %>%
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
    left_join(drList[, c("drivers", "col")], by = "drivers")

  ind <- indirect[, c(drList$drivers, habGroup$gr2)] %>%
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
    left_join(drListInd[, c("drivers", "col")], by = "drivers")

  # Maximum values
  maxVals <- c(
    max(di$ymax, na.rm = TRUE),
    min(ind$ymin, na.rm = TRUE)
  )

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Output
  out <- here::here("figures", "contribution")
  chk_create(out)

  png(
    here::here(out, glue::glue("contribution_nceahab-{period}.png")),
    res = 300,
    width = 425,
    height = 225,
    units = "mm"
  )
  layout(matrix(1:2, nrow = 2), heights = c(.88, .12))
  par(family = "serif")
  par(mar = c(0, 1, 1, 0))

  yMax <- 2.5
  yMin <- -2
  yMax <- maxVals[1] * 1.25 # 1.11
  yMin <- maxVals[2] * 1.5 #-.85
  graphicsutils::plot0(x = c(-9, nrow(direct)), y = c(yMin, yMax))


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Graph elements
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Axes
  # dL <- c(yG, max(di$ymax)+yG) # This is the reproducible code
  dL <- c(yG, yMax * .9) # This is so periods have same axes (not reproducible)
  # iL <- c(-yG, min(ind$ymax)-yG) # This is the reproducible code
  iL <- c(-yG, yMin * .74) # This is so periods have same axes (not reproducible)

  dat <- gr2[order(gr2$min), ]
  # for(i in seq(1,nrow(dat), by = 2)) polygon(x = c(rep(dat$min[i],2)-.5, rep(dat$max[i],2)+.5), y = c(dL[2],iL[2],iL[2],dL[2]), col = '#f5f4f4', border = '#00000000')
  for (i in seq(1, nrow(dat), by = 2)) {
    polygon(
      x = c(rep(dat$min[i], 2) - .5, rep(dat$max[i], 2) + .5),
      y = c(yMin * 1.07, rep(yMax * .97, 2), yMin * 1.07),
      col = "#f5f4f4",
      border = "#00000000"
    )
  }
  # Lines
  lines(x = c(-2, -2), y = dL, lwd = 1.5)
  lines(x = c(-2, -2), y = iL, lwd = 1.5)
  lines(x = c(-9, -9), y = c(iL[2], dL[2]), lwd = 1.5)
  lines(x = c(0, nrow(direct)), y = c(0, 0), lty = 2)
  # Text
  text(x = -4, y = mean(dL), labels = "Direct", srt = 90, adj = .5, font = 1)
  text(x = -4, y = mean(iL), labels = "Indirect", srt = 90, adj = .5, font = 1)
  text(x = -11, y = mean(c(iL[2], dL[2])), labels = "Total", srt = 90, adj = .5, font = 1)
  text(x = -15, y = mean(c(iL[2], dL[2])), labels = "Mean contribution of stressors to cumulative effects", srt = 90, adj = .5, font = 2)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Contribution
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # axis(1);axis(2)
  # Direct effect
  for (i in 1:nrow(di)) {
    x <- c(di$xmin[i], di$xmax[i], di$xmax[i], di$xmin[i])
    y <- c(rep(di$ymin[i], 2), rep(di$ymax[i], 2))
    polygon(x = x, y = y, border = "#00000000", col = di$col[i])
  }
  # Indirect effect
  for (i in 1:nrow(ind)) {
    x <- c(ind$xmin[i], ind$xmax[i], ind$xmax[i], ind$xmin[i])
    y <- c(rep(ind$ymin[i], 2), rep(ind$ymax[i], 2))
    polygon(x = x, y = y, border = "#00000000", col = ind$col[i])
  }


  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Taxonomic groups
  # 1st group
  for (i in 1:nrow(gr1)) {
    x <- as.numeric(gr1[i, c("min", "max")])
    lines(y = rep(yMax * .97, 2), x = x)
    text(y = yMax, x = mean(x), adj = .5, font = 2, labels = gr1$gr1[i])
  }

  # 2nd group
  for (i in 1:nrow(gr2)) {
    x <- as.numeric(gr2[i, c("min", "max")])
    lines(y = rep(yMax * .92, 2), x = x)
    text(y = yMax * .94, x = mean(x), adj = .5, font = 1, labels = gr2$gr2[i], cex = .8)
  }

  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Taxa names
  for (i in 1:nrow(direct)) {
    text(x = i - xG / 4, y = yMin * .68, labels = direct$fullname[i], cex = .45, srt = 90, adj = c(1, .5))
  }

  #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
  # Drivers legend
  yG <- .3
  par(mar = c(.5, 19, 0, 13))
  plot0(x = c(1, 8), y = c(0.2, -6.5))
  # Groups names
  x <- c(1, 3, 4, 6, 7)
  text(x = x, y = rep(0.15, length(x)), labels = drGroup$group, font = 2, adj = c(0, .5), cex = .85)
  # Groups
  for (j in seq_len(nrow(drGroup))) {
    dat <- drListInd[drListInd$group == drGroup$group[j], ]
    dat$id <- -(1:nrow(dat))
    for (i in 1:nrow(dat)) {
      xi <- c(x[j] + .05, rep(x[j] + .15, 2), x[j] + .05)
      y <- c(rep((dat$id[i] - yG), 2), rep((dat$id[i] + yG), 2))
      polygon(x = xi, y = y, border = "#585858", col = dat$col[i])
      text(x = x[j] + .2, y = dat$id[i], labels = dat$fullname[i], cex = .6, adj = c(0, .5))
    }
  }
  dev.off()
}

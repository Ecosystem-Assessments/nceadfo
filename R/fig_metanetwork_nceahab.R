#' Export figures for metanetwork
#'
#' @describeIn fig_metanetwork metanetwork per period
#' @export
fig_metanetwork_nceahab_ <- function() {
  out <- here::here("figures", "metanetwork")
  fig_metanetwork_nceahab(
    cekm = read.csv(here::here("output", "cea_km2", "ncea_km2_2010_2015.csv")),
    out = out,
    nm = "metanetwork-Total_2010_2015.png"
  )
}

#' Export figures for metanetwork
#'
#' @describeIn fig_metanetwork metanetwork figure
#' @export
fig_metanetwork_nceahab <- function(bg = "#ffffff", out = here::here("figures", "metanetwork"), nm = "metanetwork.png") {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Libraries
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  library(magrittr)
  library(raster)
  library(tidyverse)
  library(graphicsutils)
  global_parameters()

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

  # Colors for habitats
  colHab <- "#535d8aBB"

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Function to add transparent nodes for spacing between each node groups
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  randomString <- function() paste0(letters[runif(20, 1, 26)], collapse = "")
  insertRow <- function(dat, network, subnetwork) {
    # New row to add
    newrow <- data.frame(
      network = network,
      subnetwork = subnetwork,
      category = randomString(),
      col = "#00000000",
      size = 0
    )

    # ID in data.frame where to add rows
    uid <- which(dat$subnetwork == subnetwork) |>
      sort(decreasing = TRUE)

    # Add rows
    for (i in uid) {
      dat <- dplyr::add_row(dat, newrow, .after = i)
      dat <- dplyr::add_row(dat, newrow, .before = i)
    }

    # Return
    dat
  }

  insertRow_ <- function(dat, network, subnetwork, nrep) {
    for (i in 1:nrep) {
      dat <- insertRow(dat, network, subnetwork)
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
  # Cumulative effects
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  cea_long <- function(files) {
    files |>
      read.csv() |>
      tidyr::pivot_longer(!name, names_to = "from", values_to = "cea") |>
      dplyr::filter(cea > 0) |>
      dplyr::mutate(
        cea = cea / max(cea),
        name = tools::file_path_sans_ext(name),
        from = tools::file_path_sans_ext(from)
      )
  }
  cea_long2 <- function(files) {
    dat <- read.csv(files)
    dplyr::mutate(dat, name = colnames(dat)) |>
      tidyr::pivot_longer(!name, names_to = "from", values_to = "cea") |>
      dplyr::filter(cea > 0) |>
      dplyr::mutate(
        cea = cea / max(cea),
        name = tools::file_path_sans_ext(name),
        from = tools::file_path_sans_ext(from)
      )
  }

  # Data
  input <- here::here("output", "cea_km2")
  input2 <- here::here("output", "cea_species_contribution")
  cea <- list(
    hab = cea_long(here::here(input, "cea_habitats_km2_2016_2021.csv")), # Habitats
    direct = cea_long(here::here(input, "ncea_direct_km2_2016_2021.csv")), # Direct species from stressors
    indirect = cea_long(here::here(input, "ncea_indirect_km2_2016_2021.csv")), # Indirect species from stressors
    indirect_sp = cea_long2(here::here(input2, "species_contribution_network.csv")), # Indirect species
    indirect_hab = cea_long(here::here(input, "cea_indirect_habitats_km2_2016_2021.csv")) # Indirect habitats
  )
  nceahab <- read.csv(here::here(input, "nceahab_species_km2_2016_2021.csv")) # NCEAHAB

  # Full effects for species, habitats and drivers
  full_effects <- list(
    habitats = cea$hab |>
      dplyr::group_by(name) |>
      dplyr::summarise(cea = sum(cea)) |>
      dplyr::mutate(cea = cea / max(cea, na.rm = TRUE)),
    species = nceahab |>
      dplyr::rename(cea = nceahab) |>
      dplyr::mutate(cea = cea / max(cea, na.rm = TRUE)),
    drivers = list(cea$hab, cea$direct, cea$indirect) |>
      dplyr::bind_rows() |>
      dplyr::group_by(from) |>
      dplyr::summarise(cea = sum(cea)) |>
      dplyr::rename(name = from) |>
      dplyr::mutate(cea = cea / max(cea, na.rm = TRUE))
  ) |>
    dplyr::bind_rows()

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Nodes
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
    dplyr::mutate(network = "Stressors") |>
    dplyr::select(network, subnetwork = group, category = drivers, col = cols) |>
    dplyr::arrange(network, subnetwork)

  # Species list
  spList <- read.csv(here::here("data", "cea_modules", "species_list.csv"))
  spGroup <- dplyr::select(spList, gr1, gr2) |>
    dplyr::distinct() |>
    dplyr::arrange(gr1, gr2) |>
    dplyr::mutate(col = ifelse(gr1 == "Invertebrates", colInv, colVer))
  spList <- dplyr::left_join(spList, spGroup, by = c("gr1", "gr2")) |>
    dplyr::select(network = gr1, subnetwork = gr2, category = shortname, col) |>
    dplyr::arrange(network, subnetwork)


  # Habitat list
  habGroups <- list(
    data.frame(
      subnetwork = "Nearshore",
      h = c("BITDL", "MFITDL", "RITDL", "SSHLW", "HSHLW", "MSHLW", "SALTMARSH", "SEAG", "ALGAL", "KELP", "HMUSSEL")
    ),
    data.frame(
      subnetwork = "Benthic",
      h = c("SSHLF", "HSHLF", "MSHLF", "CYN", "SDEEP", "HDEEP", "MDEEP", "DPBIO")
    ),
    data.frame(
      subnetwork = "Pelagic",
      h = c("SPELAGIC", "DPELAGIC")
    )
  ) |>
    dplyr::bind_rows()
  habList <- read.csv(here::here("data", "cea_modules", "habitats_list.csv")) |>
    dplyr::left_join(habGroups, by = c("HabitatCODE" = "h")) |>
    dplyr::mutate(
      col = colHab,
      network = "Habitats"
    ) |>
    dplyr::select(network, subnetwork, category = HabitatCODE, col) |>
    dplyr::arrange(network, subnetwork)

  # Full nodes list
  nodes <- list(drList, spList, habList) |>
    dplyr::bind_rows() |>
    # dplyr::arrange(network, subnetwork, category) |>
    dplyr::left_join(full_effects, by = c("category" = "name")) |>
    dplyr::rename(size = cea) |>
    dplyr::mutate(size = log(size + 1))

  # Add rows
  nodes <- nodes |>
    insertRow_("Stressors", "Marine traffic", 1) |>
    insertRow_("Stressors", "Fisheries", 1) |>
    insertRow_("Stressors", "Coastal", 1) |>
    insertRow_("Stressors", "Climate", 1) |>
    insertRow_("Habitats", "Pelagic", 1)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Links
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  links <- cea[c("indirect_sp", "indirect_hab", "hab", "direct")]
  links$hab <- dplyr::left_join(links$hab, drList[, c("category", "col")], by = c("from" = "category"))
  links$direct <- dplyr::left_join(links$direct, drList[, c("category", "col")], by = c("from" = "category"))
  links$indirect_sp$col <- "#e3e1e133"
  links$indirect_hab$col <- "#535d8a33"
  links <- dplyr::bind_rows(links) |>
    dplyr::select(from, to = name, col)

  # Change marine traffic name
  nodes$subnetwork <- stringr::str_replace(nodes$subnetwork, "Marine traffic", "Shipping")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  # Figure
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
  metanetwork::metanetwork(
    nodes,
    links,
    filename = here::here(out, nm),
    legend = FALSE,
    img_size = 400
  )
}

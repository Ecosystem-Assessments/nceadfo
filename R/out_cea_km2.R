#' Function to export cumulative drivers and species (richness)
#'
#' @export
out_cea_km2 <- function() {
  library(tidyverse)
  library(magrittr)
  library(raster)

  # Load files
  modules <- here::here("data","format_modules")
  dat <- dir(modules, full.names = TRUE)
  for(i in dat) load(i)
  names(biotic) <- txNames
  nTx <- length(txNames)
  
  # Output 
  output <- here::here("output","cea_km2")
  chk_create(output)
  
  # There are multiple periods for this analysis
  # Divide drivers data into the periods, will make things easier
  dr_all <- dr 
  per <- names(dr_all)
  # shortnames <- glue::glue("{tools::file_path_sans_ext(sp$file)}.rds")
  # shortnamestif <- glue::glue("{tools::file_path_sans_ext(sp$file)}")
  
  for(k in 1:length(dr_all)) {
    dr <- dr_all[[k]]
    output_cea <- here::here("output","cea_network", per[k])
    output_sp <- here::here("output","cea_species", per[k])
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Evaluate metrics per km2
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #    
    # Setup data.frame
    cekm <- data.frame(Taxa = txNames, stringsAsFactors = FALSE)

    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Use biotic data for area 
    # Assuming 1km2 grid cells
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    dat <- numeric(nTx)
    for(i in 1:nlayers(biotic)) dat[i] <- sum(values(biotic[[i]]), na.rm = TRUE)
    cekm$area <- dat

    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Import cumulative metrics
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # ----------------------
    # Species-scale cea
    out <- cea_metric(output_sp, '')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$cea_species <- dat / cekm$area

    # ----------------------
    # Network-scale cea - normalized by number of interactions
    out <- cea_metric(output_cea, 'cea')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$cea_network <- dat / cekm$area

    # ----------------------
    # Network-scale cea - direct effects
    out <- cea_metric(output_cea, 'cea_direct')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$cea_network_direct <- dat / cekm$area

    # ----------------------
    # Network-scale cea - indirect effects
    out <- cea_metric(output_cea, 'cea_indirect')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$cea_network_indirect <- dat / cekm$area

    # # ----------------------
    # # Trophic position
    # out <- cea_metric('trophic_position')
    # dat <- numeric(nTx)
    # for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    # cekm$trophic_position <- dat / cekm$area
    # 
    # # ----------------------
    # # Trophic sensitivity
    # out <- cea_metric('trophic_sensitivity')
    # dat <- numeric(nTx)
    # for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    # cekm$trophic_sensitivity <- dat / cekm$area

    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Stressors effects
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # ----------------------
    # Total
    path <- here::here(output_cea, "cea_drivers")
    dr <- dir(path)
    for(j in 1:length(dr)) {
      out <- cea_metric(path, dr[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(dr[j], '_Total_Effect')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Direct
    path <- here::here(output_cea, "cea_drivers_direct")
    dr <- dir(path)
    for(j in 1:length(dr)) {
      out <- cea_metric(path, dr[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(dr[j], '_Direct_Effect')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Indirect
    path <- here::here(output_cea, "cea_drivers_indirect")
    dr <- dir(path)
    for(j in 1:length(dr)) {
      out <- cea_metric(path, dr[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(dr[j], '_Indirect_Effect')
      cekm[, cName] <- dat / cekm$area
    }

    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Export
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    write.csv(cekm, here::here(output, glue::glue("cea_km2-{per[k]}.csv")), row.names = FALSE)
  } #k
}

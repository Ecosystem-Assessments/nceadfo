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
    # Normalized cumulative effects - normalized by number of interactions
    out <- cea_metric(output_cea, 'cea')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$cea_network <- dat / cekm$area

    # ----------------------
    # Disconnected impacts
    out <- cea_metric(output_sp, 'Impact_Disconnect')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$impactDisconnect <- dat / cekm$area

    # ----------------------
    # Intensity total
    out <- cea_metric('Intensity_Total')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$intensity_total <- dat / cekm$area

    # ----------------------
    # Intensity direct
    out <- cea_metric('Intensity_Direct')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$intensity_direct <- dat / cekm$area

    # ----------------------
    # Intensity indirect
    out <- cea_metric('Intensity_Indirect')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$intensity_indirect <- dat / cekm$area

    # ----------------------
    # Intensity indirect
    out <- cea_metric('Trophic_Position')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$trophic_position <- dat / cekm$area

    # ----------------------
    # Trophic sensitivity
    out <- cea_metric('Trophic_Sensitivity')
    dat <- numeric(nTx)
    for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
    cekm$trophic_sensitivity <- dat / cekm$area


    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Import stressors intensity
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # ----------------------
    # Total intensity
    fold <- dir('./Data/Results/Stressor_Total/')
    metrics <- paste0('Stressor_Total/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Total')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Direct intensity
    fold <- dir('./Data/Results/Stressor_Direct/')
    metrics <- paste0('Stressor_Direct/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Direct')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Indirect intensity
    fold <- dir('./Data/Results/Stressor_Indirect/')
    metrics <- paste0('Stressor_Indirect/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Indirect')
      cekm[, cName] <- dat / cekm$area
    }


    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Import stressors effects
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # ----------------------
    # Total intensity
    fold <- dir('./Data/Results/Stressor_Total_Effect/')
    metrics <- paste0('Stressor_Total_Effect/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Total_Effect')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Direct intensity
    fold <- dir('./Data/Results/Stressor_Direct_Effect/')
    metrics <- paste0('Stressor_Direct_Effect/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Direct_Effect')
      cekm[, cName] <- dat / cekm$area
    }

    # ----------------------
    # Indirect intensity
    fold <- dir('./Data/Results/Stressor_Indirect_Effect/')
    metrics <- paste0('Stressor_Indirect_Effect/',fold)
    for(j in 1:length(metrics)) {
      out <- cea_metric(metrics[j])
      dat <- numeric(nTx)
      for(i in 1:nlayers(out)) dat[i] <- sum(values(out[[i]]), na.rm = TRUE)
      cName <- paste0(fold[j], '_Indirect_Effect')
      cekm[, cName] <- dat / cekm$area
    }


    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    # Export
    # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    saveRDS(cekm, './Data/Results/Cumulative_Effects/CumulativeEffects_Area.rds')
    write.csv(cekm, here::here(out_per[k]))    
  } #k
}

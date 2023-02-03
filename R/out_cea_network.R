#' Function to export cumulative drivers and species (richness)
#'
#' @export

out_cea_network <- function() {
  library(tidyverse)
  library(magrittr)
  library(raster)

  # Load files
  modules <- here::here("data","format_modules")
  dat <- dir(modules, full.names = TRUE)
  for(i in dat) load(i)

  # Output 
  out <- here::here("output","cea_network")
  chk_create(out)
  
  # There are multiple periods for this analysis
  # Divide drivers data into the periods, will make things easier
  dr_all <- dr 
  per <- names(dr_all)
  out <- here::here(out, per)
  shortnames <- glue::glue("{tools::file_path_sans_ext(sp$file)}.rds")
  for(k in 1:length(dr_all)) {
    dr <- dr_all[[k]]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Impact for connected species
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # If run on a single laptop
    # For species of interest i that are involved in triads (loop over species)
    connectRisk <- list()
    system.time({
    for(i in 1:ncol(bt)) {
      connectRisk[[i]] <- list()
      # # For each raster cell j (loop over cells)
      for(j in 1:nrow(bt)) {
        connectRisk[[i]][[j]] <- CumulativeRisk(focusID = i,
                                              biotic = bt[j,],
                                              drivers = dr[j,],
                                              vulnerability = species_sensitivity,
                                              sensitivity = sensitivity)
      print(j)
      }
    }
    })

    # Save
    saveRDS(connectRisk, file = here::here(out[k],shortnames[i]))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Function to create impact rasters
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    makeRaster <- function(temp, sp) {
      # Create raster
      dat <- r
      values(dat)[idBiotic] <- temp
      names(dat) <- sp

      # Multiply species distribution with cumulative risk
      dat <- dat * biotic[[sp]]

      # Read all
      dat <- readAll(dat)

      # Return
      return(dat)
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Impact for connected species - from Compute Canada outputs
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # This is computationally demanding, so the analyses were run on a server.
    #
    # Acknowledgements:
    # This research was enabled in part by support provided by
    # WestGrid (www.westgrid.ca) and Compute Canada (www.computecanada.ca).
    #
    # There is a lot of things to measure, so to manage memory I calculate each
    # metric (almost) seperately for each taxa
    files <- dir('./Data/res/', full.names = TRUE)
    spNames <- colnames(bt)

    # Process for each taxa
    system.time({
    for(i in 1:length(files)) {
      cat('      ', i, 'of', length(files), ': ', spNames[i], '\n')
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Cumulative risk evaluation
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      risk <- readRDS(files[i])

      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Cumulative impact
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Parameters
      nC <- ncol(risk[[1]][[1]])
      cNames <- colnames((risk[[1]][[1]]))

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[1]]
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nC, byrow = TRUE, dimnames = list(c(), cNames)) %>%
              as.data.frame()

      # --------------------
      # Cumulative impact
      out <- makeRaster(temp$risk, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Impact/impact_sp.%03d.rds", i))

      # --------------------
      # Normalized cumulative impact
      out <- makeRaster(temp$risk/temp$count, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Impact_Normalized/impact_normalized_sp.%03d.rds", i))

      # --------------------
      # Total intensity
      out <- makeRaster(temp$intensity_total/temp$count, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Intensity_Total/intensity_total_sp.%03d.rds", i))

      # --------------------
      # Direct intensity
      out <- makeRaster(temp$intensity_direct/temp$count, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Intensity_Direct/intensity_direct_sp.%03d.rds", i))

      # --------------------
      # Indirect intensity
      out <- makeRaster(temp$intensity_indirect/temp$count, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Intensity_Indirect/intensity_indirect_sp.%03d.rds", i))

      # --------------------
      # Trophic position
      out <- makeRaster(temp$focus, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Trophic_Position/trophic_position_sp.%03d.rds", i))

      # --------------------
      # Trophic sensitivity
      out <- makeRaster(temp$sensitivity/temp$count, spNames[i])
      saveRDS(out, sprintf("./Data/Results/Trophic_Sensitivity/trophic_sensitivity_sp.%03d.rds", i))

      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - total
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[2]])
      drNames <- names(risk[[1]][[2]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[2]] / risk[[j]][[1]]$count
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Total/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Total/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Total/%s/%s_total_sp.%03d.rds", k, k, i))
      }


      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - direct
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[3]])
      drNames <- names(risk[[1]][[3]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[3]] / risk[[j]][[1]]$count
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Direct/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Direct/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Direct/%s/%s_direct_sp.%03d.rds", k, k, i))
      }


      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - indirect
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[4]])
      drNames <- names(risk[[1]][[4]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[4]] / risk[[j]][[1]]$count
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Indirect/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Indirect/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Indirect/%s/%s_indirect_sp.%03d.rds", k, k, i))
      }
    }
    })


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Evaluate cumulative effects
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    source('./Code/Functions/getMetric.R')

    # Cumulative effects
    out <- getMetric('Impact')
    out <- stack(out)
    out <- sum(out, na.rm = TRUE)
    values(out)[!idBiotic] <- NA
    saveRDS(out, './Data/Results/Cumulative_Effects/CumulativeEffects.rds')

    # Cumulative effects normalizd
    out <- getMetric('Impact_Normalized')
    out <- stack(out)
    out <- sum(out, na.rm = TRUE)
    values(out)[!idBiotic] <- NA
    saveRDS(out, './Data/Results/Cumulative_Effects/CumulativeEffects_Normalized.rds')




    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Impact for connected species - from Compute Canada outputs - had to run a 2nd round of partial results
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # This is computationally demanding, so the analyses were run on a server.
    #
    # Acknowledgements:
    # This research was enabled in part by support provided by
    # WestGrid (www.westgrid.ca) and Compute Canada (www.computecanada.ca).
    #
    # There is a lot of things to measure, so to manage memory I calculate each
    # metric (almost) seperately for each taxa
    files <- dir('./Data/res2/', full.names = TRUE)
    files2 <- dir('./Data/res/', full.names = TRUE)
    spNames <- colnames(bt)

    # Process for each taxa
    system.time({
    for(i in 1:length(files)) {
      cat('      ', i, 'of', length(files), ': ', spNames[i], '\n')
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Cumulative risk evaluation
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      risk <- readRDS(files[i])

      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Count
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      count <- readRDS(files2[i])

      # Take only count data needed for metric
      ct <- numeric()
      for(j in 1:length(count)) {
        ct[j] <- count[[j]][[1]]$count
      }

      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - total
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[1]])
      drNames <- names(risk[[1]][[1]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[1]] / ct[j]
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Total_Effect/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Total_Effect/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Total_Effect/%s/%s_total_effect_sp.%03d.rds", k, k, i))
      }


      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - direct
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[2]])
      drNames <- names(risk[[1]][[2]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[2]] / ct[j]
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Direct_Effect/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Direct_Effect/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Direct_Effect/%s/%s_direct_effect_sp.%03d.rds", k, k, i))
      }


      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Impact / stressor - indirect
      # <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
      # Param
      nDr <- length(risk[[1]][[3]])
      drNames <- names(risk[[1]][[3]])

      # Take only data needed for metric
      temp <- list()
      for(j in 1:length(risk)) {
        temp[[j]] <- risk[[j]][[3]] / ct[j]
      }

      # As matrix
      temp <- matrix(unlist(temp), ncol = nDr, byrow = TRUE, dimnames = list(c(), drNames)) %>%
              as.data.frame()

      # Create folders to store results if they do not already exist
      fold <- dir('./Data/Results/Stressor_Indirect_Effect/')
      for(k in drNames) if (!k %in% fold) dir.create(paste0('./Data/Results/Stressor_Indirect_Effect/',k))

      # --------------------
      # Impact per stressor
      for(k in drNames) {
        out <- makeRaster(temp[, k], spNames[i])
        saveRDS(out, sprintf("./Data/Results/Stressor_Indirect_Effect/%s/%s_indirect_effect_sp.%03d.rds", k, k, i))
      }
    }
    })
  } #k
}

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
  names(biotic) <- txNames
  nTx <- length(txNames)
  
  # Output 
  output <- here::here("output","cea_network")
  chk_create(output)
  
  # There are multiple periods for this analysis
  # Divide drivers data into the periods, will make things easier
  dr_all <- dr 
  per <- names(dr_all)
  output <- here::here(output, per)
  lapply(output, chk_create)
  shortnames <- glue::glue("{tools::file_path_sans_ext(sp$file)}.rds")
  shortnamestif <- glue::glue("{tools::file_path_sans_ext(sp$file)}")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Functions
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  makeRaster <- function(temp, sp) {
    # Create raster
    dat <- r
    values(dat)[idBiotic] <- temp
    names(dat) <- sp
  
    # Multiply species distribution with cumulative risk
    dat <- dat * biotic[[sp]]
  
    # Read all
    # dat <- readAll(dat)
  
    # Return
    return(dat)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Impact for connected species - from Compute Canada outputs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  for(k in 1:length(dr_all)) {
    dr <- dr_all[[k]]
    output_cell <- here::here(output[k], "cell_cea")
    chk_create(output_cell)
    
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # # Impact for connected species
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # This is computationally demanding, so the analyses were run on a server.
    #
    # Acknowledgements:
    # This research was enabled in part by support provided by
    # WestGrid (www.westgrid.ca) and Compute Canada (www.computecanada.ca).
    # 
    # See: 
    # - launch.R 
    # - launch.sh
    #
    # # If run on a single laptop
    # # For species of interest i that are involved in triads (loop over species)
    # connectRisk <- list()
    # for(i in 1:ncol(bt)) {
    #   connectRisk[[i]] <- list()
    #   # # For each raster cell j (loop over cells)
    #   for(j in 1:nrow(bt)) {
    #     connectRisk[[i]][[j]] <- network_risk(
    #       focusID = i,
    #       biotic = bt[j,],
    #       drivers = dr[j,],
    #       vulnerability = species_sensitivity,
    #       sensitivity = sensitivity
    #     )
    #   }
    #   saveRDS(connectRisk, file = here::here(output[k],shortnames[i]))
    # }
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # There is a lot of things to measure, so to manage memory I calculate each
    # metric (almost) seperately for each taxa
    files <- dir(here::here(output_cell), full.names = TRUE)
    spNames <- colnames(bt)
      
    # Process for each taxa
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
      temp <- dplyr::bind_rows(temp)
    
      # # --------------------
      # # Cumulative effects - not normalized by number of interactions
      # outfold <- here::here(output[k], "impact")
      # chk_create(outfold)
      # out <- makeRaster(temp$risk_total, spNames[i])
      # export_raster(out, outfold, shortnamestif[i])
      
    
      # --------------------
      # Normalized cumulative effects - normalized by number of interactions
      outfold <- here::here(output[k], "cea")
      chk_create(outfold)
      out <- makeRaster(temp$risk_total/temp$count, spNames[i])
      export_raster(out, outfold, shortnamestif[i])

    
      # --------------------
      # Direct effects
      outfold <- here::here(output[k], "cea_direct")
      chk_create(outfold)
      out <- makeRaster(temp$risk_direct/temp$count, spNames[i])
      export_raster(out, outfold, shortnamestif[i])
    
      # --------------------
      # Indirect effects
      outfold <- here::here(output[k], "cea_indirect")
      chk_create(outfold)
      out <- makeRaster(temp$risk_indirect/temp$count, spNames[i])
      export_raster(out, outfold, shortnamestif[i])
    
      # # --------------------
      # # Trophic position
      # outfold <- here::here(output[k], "trophic_position")
      # chk_create(outfold)
      # out <- makeRaster(temp$focus, spNames[i])
      # export_raster(out, outfold, shortnamestif[i])

    
      # # --------------------
      # # Trophic sensitivity
      # outfold <- here::here(output[k], "trophic_sensitivity")
      # chk_create(outfold)
      # out <- makeRaster(temp$sensitivity/temp$count, spNames[i])
      # export_raster(out, outfold, shortnamestif[i])

    
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
      temp <- dplyr::bind_rows(temp) |>
              as.data.frame()
    
      # --------------------
      outfold <- here::here(output[k], "cea_drivers", drNames)
      lapply(outfold, chk_create)
      for(l in 1:length(drNames)) {
        out <- makeRaster(temp[, drNames[l]], spNames[i])
        export_raster(out, outfold[l], shortnamestif[i])
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
      temp <- dplyr::bind_rows(temp) |>
              as.data.frame()
    
      # --------------------
      outfold <- here::here(output[k], "cea_drivers_direct", drNames)
      lapply(outfold, chk_create)
      for(l in 1:length(drNames)) {
        out <- makeRaster(temp[, drNames[l]], spNames[i])
        export_raster(out, outfold[l], shortnamestif[i])
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
      temp <- dplyr::bind_rows(temp) |>
              as.data.frame()
    
      # --------------------
      outfold <- here::here(output[k], "cea_drivers_indirect", drNames)
      lapply(outfold, chk_create)
      for(l in 1:length(drNames)) {
        out <- makeRaster(temp[, drNames[l]], spNames[i])
        export_raster(out, outfold[l], shortnamestif[i])
      }
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # Evaluate cumulative effects
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #    
    outfold <- here::here("output", "cea")
    chk_create(outfold)

    # # Cumulative effects
    # out <- cea_metric(output[k], "impact")
    # out <- sum(out, na.rm = TRUE)
    # values(out)[!idBiotic] <- NA
    # export_raster(out, outfold, glue::glue("cea_network-{per[k]}"))
    
    # Cumulative effects normalizd
    out <- cea_metric(output[k], "cea")
    out <- sum(out, na.rm = TRUE)
    values(out)[!idBiotic] <- NA
    export_raster(out, outfold, glue::glue("cea_network-{per[k]}"))
  } #k
}

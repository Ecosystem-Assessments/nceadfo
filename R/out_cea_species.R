#' Function to export cumulative drivers and species (richness)
#'
#' @export

out_cea_species <- function() {
  library(tidyverse)
  library(magrittr)
  library(raster)

  # Load files
  modules <- here::here("data","format_modules")
  dat <- dir(modules, full.names = TRUE)
  for(i in dat) load(i)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Impacts cumulés individus centrés
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Trophic sensitivity of disconnected species x
  uid <- sensitivity$speciesID == 1 & sensitivity$motifID == 1 & sensitivity$pathID == 1
  sensitivity_dix <- sensitivity$Sensitivity[uid]

  # Evaluate cumulative risk
  disconnectRisk <- list()
  for(i in 1:length(dr)) {
    disconnectRisk[[i]] <- list()
    system.time({
      for(j in 1:nrow(dr[[i]])) {
        cat("period: ", i, ' of ', length(dr), '\r')
        disconnectRisk[[i]][[j]] <- species_risk(
          drivers = dr[[i]][j, ],
          vulnerability = species_sensitivity,
          sensitivity = sensitivity_dix
        )
      }
    })
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Keep only relevant information for this analysis
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # risk <- riskDr <- list()
  # for(i in 1:length(disconnectRisk)) {
  #   risk[[i]] <- disconnectRisk[[i]][[1]]
  #   riskDr[[i]] <- disconnectRisk[[i]][[2]]
  # }
  risk <- disconnectRisk
  rm(disconnectRisk)

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Disconnected risk as matrix
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # As matrix
  dRisk <- list()
  for(j in 1:length(risk)) {
    dRisk[[j]] <- matrix(0, nrow = length(risk[[j]]), ncol = nrow(risk[[j]][[1]][[1]][]),
                    dimnames = list(c(), rownames(risk[[j]][[1]][[1]])))
    for(i in 1:length(risk[[j]])) dRisk[[j]][i, ] <- risk[[j]][[i]][[1]][,1]    
  }


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Risk rasters
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  risk <- list()
  for(j in 1:length(dRisk)) {
    risk[[j]] <- list()
    for(i in 1:ncol(dRisk[[j]])) {
      dat <- r
      values(dat)[idBiotic] <- dRisk[[j]][,i]
      risk[[j]][[i]] <- dat
    }
    names(risk[[j]]) <- colnames(dRisk[[j]])

    # Remove phytoplankton and zooplankton
    risk[[j]] <- risk[[j]][!names(risk[[j]]) %in% c('phytoplankton','zooplankton')]    
  }


  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Cumulative impact
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Filter biotic distribution (probably useless)
  # biotic <- biotic[[names(risk[[1]])]]

  # Multiply species distribution with cumulative risk
  disconnectImpact <- list()
  for(j in 1:length(risk)) {
    disconnectImpact[[j]] <- list()
    for(i in 1:length(risk[[j]])) {
      disconnectImpact[[j]][[i]] <- risk[[j]][[i]] * biotic[[i]]
    }
    names(disconnectImpact[[j]][[i]]) <- names(risk[[j]])[i]
  }

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Export per species per period
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  out <- here::here("output","cea_species")
  out_per <- here::here(out, names(dr))
  lapply(out_per, chk_create)
  
  for(j in 1:length(disconnectImpact)) {
    for(i in 1:length(disconnectImpact[[j]])) {
      out <- disconnectImpact[[j]][[i]]
      nm <- names(risk[[j]])[i]
      names(out) <- nm
      # readAll(out)
      # saveRDS(out, here::here(out_per[j], glue::glue("{nm}.rds")))
      export_raster(out, out_per[j],nm)
    }
  }
  

  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  # Export cumulative effects disconnected
  #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~#
  disconnectImpact <- lapply(disconnectImpact, stack)
  disconnectCumImpact <- list()
  for(i in 1:length(disconnectImpact)) {
    disconnectCumImpact[[i]] <- sum(disconnectImpact[[i]], na.rm = TRUE)
    values(disconnectCumImpact[[i]])[!idBiotic] <- NA
    names(disconnectCumImpact) <- glue::glue("cea_species-{names(dr)[i]}")
  }

  # Export
  out <- here::here("output","cea")
  chk_create(out)
  for(i in 1:length(disconnectCumImpact)) {
    export_raster(disconnectCumImpact[[i]], out, glue::glue("cea_species-{names(dr)[i]}"))
  }
  
  # Evaluate difference between periods
  cea <- dir(out, pattern = "cea_species", full.names = TRUE) |>
         lapply(stars::read_stars)
  diff <- cea[[2]]-cea[[1]]
  
  # Export 
  out <- here::here("output","cea_difference")
  chk_create(out)
  stars::write_stars(diff, here::here(out, "cea_species_difference.tif"))
}

library(tidyverse)
library(magrittr)
source('R/fnc_network_risk.R')

# Specify and create output folder
output <- "~/scratch/output/cea_network"
# dir.create(output, showWarnings = FALSE, recursive = TRUE)

# Load files for analysis
dat <- dir("data/format_modules", full.names = TRUE)
for (i in dat) load(i)

# Identify periods and create folders for outputs per period
dr_all <- dr 
per <- names(dr_all)
out_per <- paste0(output,"/",per,"/cell_cea")
# lapply(out_per, dir.create, recursive = TRUE, showWarnings = FALSE)  
shortnames <- paste0(tools::file_path_sans_ext(sp$file),".rds")

# Run analysis
# for(k in 1:length(dr_all)) {
  k = 2
  dr <- dr_all[[k]]
  
  # !!!!!
  iid <- which(!shortnames %in% dir(out_per[k]))
  # !!!!!

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Impact for connected species
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # use argument
  i <- as.numeric(commandArgs(trailingOnly = TRUE))
  # Gadus morhua
  out <- list()
  # For each raster cell j (loop over cells)
  for (j in seq_len(nrow(bt))) {
      # cat(j, "/", nrow(bt), "          \r")
      out[[j]] <- network_risk(focusID = iid[i],
                      biotic = bt[j,],
                      drivers = dr[j,],
                      vulnerability = species_sensitivity,
                      sensitivity = sensitivity)
  }
  saveRDS(out, file = paste0(out_per[k],"/",shortnames[iid[i]]))
# }
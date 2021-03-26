# Create area of interest spatial file from canadian bioregions

fmt_aoi <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/0001-bioregions/'

  # dataID: 0001
  ## Bioregions
  ## st_layers(paste0(folder, 'DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb'))
  bioregions <- st_read(paste0(folder, 'DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb'),
                        layer = 'DFO_Marine_Bioregions')

  # Select Scotian Shelf bioregion
  uid <- "11. Scotian Shelf / Plate-forme Scotian"
  aoi <- bioregions[bioregions$Legend == uid, ]

  # Export
  save(aoi, file = './data/aoi.RData')
}

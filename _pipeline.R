library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_parameters()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  make_grid(cellsize = 0.01)

  # Integrate data 
  pipedat::pipeflow("./data/config/pipedat.yml")
  
  # Prepare assessment data modules 
  make_drivers()
  make_abiotic()
  make_biotic()
  make_metaweb()
  # make_species_sensitivity()
  make_trophic_sensitivity()

  # Data for eDrivers 
  make_eDrivers()
  
  # Outputs
  out_footprint()
  out_exposure()
  
  # Figures 
  fig_atlas()
  
  # Report 
  render_report()
}
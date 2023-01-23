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
  make_species_sensitivity()
  make_trophic_sensitivity()

  # ------------------------------------------------------------------------------------------------
  # WARNING 
  # For the sake of efficiency at this point, I'm taking the scripts from my thesis.
  # These scripts use `raster` rather than `stars`
  # This should be corrected later on in the process 
  # The following script essentially creates a list of RData that are then used for the assessment
  # https://github.com/Ecosystem-Assessments/nceadfo/issues/3
  format_modules()
  # ------------------------------------------------------------------------------------------------

  # Data for eDrivers 
  make_eDrivers()
  
  # Outputs
  out_footprint()
  out_exposure()
  out_cea_species()
  
  # Figures 
  fig_atlas()
  
  # Report 
  render_report()
}
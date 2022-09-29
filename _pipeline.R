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
  pipedat::pipeflow("./data/data-config/pipedat.yml")
}
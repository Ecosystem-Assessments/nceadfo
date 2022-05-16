library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_parameters()
  
  # Get raw data 
  get_data()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  make_grid(cellsize = 10000)

  # Integrate data 
  get_integrated()
  
}
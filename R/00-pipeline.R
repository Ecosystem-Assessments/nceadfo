pipeline <- function(pipeline_data = FALSE,
                     pipeline_format = FALSE) {

    if (pipeline_data) {
      dat0001_bioregions()
    }

    if (pipeline_format) {
      # Area of interest
      fmt_aoi()

    }
}

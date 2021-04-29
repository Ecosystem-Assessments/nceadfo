pipeline <- function(pipeline_data = FALSE,
                     pipeline_format = FALSE) {

    if (pipeline_data) {
      dat0001_bioregions()
      dat0002_sdm_ais()
    }

    if (pipeline_format) {
      # Area of interest
      fmt_aoi()

    }
}

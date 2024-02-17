#' Pipeline to execute full cumulative effects assessment.
#'
#' @export

pipeline <- function() {
  # Update global parameters
  global_parameters()

  # Get area of interest & basemaps
  get_aoi()
  get_basemap()

  # Integrate data
  pipedat::pipeflow("./data/config/pipedat.yml")

  # Get metadata & bibliographies
  gather_meta()
  gather_bib()

  # Prepare assessment data modules
  make_drivers()
  make_abiotic()
  make_biotic()
  make_metaweb()
  make_species_sensitivity()
  make_trophic_sensitivity()
  format_data()

  # NCEAMM
  format_data("nceamm_pam")
  format_data("nceamm_wsdb")
  format_data("nceamm_pam_wsdb")


  # Assessment
  cea()
  # source("R/batch/.ncea_species_2010_2015.R") # Run on compute canada
  # source("R/batch/.ncea_species_2016_2021.R") # Run on compute canada

  # Outputs
  out_footprint()
  out_exposure()
  out_cea_full()
  out_cea_km2()
  out_difference()


  # Figures
  fig_aoi()
  figures()
  fig_metanetwork_()
  fig_contribution_group_()
  fig_contribution_taxa_()
  fig_atlas()
  fig_webinar()

  # Report and publications
  render_frontpate()
  render_report()
  render_webinar()

  # Data for eDrivers
  make_eDrivers()

  # Create data for application
  make_bucket()
  make_stac()
}

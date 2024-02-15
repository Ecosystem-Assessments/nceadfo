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
  make_habitats()
  make_habitats_sensitivity()
  make_overlap()
  format_data()

  # Assessment
  cea() # Species-scale assessment
  hcea() # Habitat-scale assessment
  # source("R/.ncea_species_2010_2015.R") # Run on compute canada # Network-scale assessment
  # source("R/.ncea_species_2016_2021.R") # Run on compute canada # Network-scale assessment

  # Outputs
  out_footprint()
  out_exposure()
  out_indirect_species_habitats_()
  out_nceahab_species()
  out_nceahab()
  out_cea_full()
  out_cea_km2()
  out_difference()

  # Figures
  fig_aoi()
  figures()
  fig_metanetwork_()
  fig_metanetwork_nceahab()
  fig_contribution_group_()
  fig_contribution_taxa_()
  fig_contribution_nceahab()
  fig_atlas()
  fig_webinar()
  fig_report_nceahab()

  # Report and publications
  render_frontpate()
  render_report()
  render_webinar()

  # Data for eDrivers
  make_eDrivers()
}

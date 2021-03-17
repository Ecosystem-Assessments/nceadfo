dat0001_bioregions <- function() {
  output <- './analysis/data/0001-bioregions/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Federal Marine Bioregions
  # ------------------------------------
  # dataID:0001
  # ~~~~~~~~~~~~
  #
  # The spatial planning framework for Canada's national network of Marine
  # Protected Areas (MPAs) is comprised of 13 ecologically defined bioregions
  # that cover Canada's oceans and the Great Lakes. Note that the geographic
  # boundaries for the bioregions are fuzzy and may change based on ecosystemic
  # conditions.
  #
  # https://open.canada.ca/data/en/dataset/23eb8b56-dac8-4efc-be7c-b8fa11ba62e9
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!file.exists(paste0(output, 'DFO_Marine_Bioregions.zip'))) {
    # Download from Open Canada portal
    uid <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
    rgovcan::govcan_dl_resources(uid, path = output)

    # Unzip file
    unzip(zipfile = paste0(output, 'DFO_Marine_Bioregions.zip'), exdir = output)
  }
}

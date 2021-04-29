dat0002_sdm_ais <- function() {
  output <- './analysis/data/0002-sdm_ais/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Species distribution models for aquatic invasive species
  # ------------------------------------
  # dataID:0002
  # ~~~~~~~~~~~~
  #
  # Lyons DA, Lowen JB, Therriault TW, Brickman D, Guo L, Moore AM, Peña MA,
  # Wang Z, DiBacco C. (In Press) Identifying Marine Invasion Hotspots Using
  # Stacked Species Distribution Models. Biological Invasions
  #
  # https://open.canada.ca/data/en/dataset/1bbd5131-8b34-4245-b999-3b4c4259d74f
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  if (!file.exists(paste0(output, 'Atl_Pac_tif.zip'))) {
    # Download from Open Canada portal
    uid <- "1bbd5131-8b34-4245-b999-3b4c4259d74f"
    rgovcan::govcan_dl_resources(uid, path = output)

    # Unzip file
    unzip(zipfile = paste0(output, 'Atl_Pac_tif.zip'), exdir = output)
    unzip(zipfile = paste0(output, 'Atl_Pac.gdb.zip'), exdir = output)
  }
}

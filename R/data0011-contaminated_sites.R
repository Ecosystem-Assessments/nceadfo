#' Data 0011 : Federal Contaminated Sites Inventory
#'
#' The Federal Contaminated Sites Inventory includes information on all known federal contaminated sites under the custodianship of departments, agencies and consolidated Crown corporations as well as those that are being or have been investigated to determine whether they have contamination arising from past use that could pose a risk to human health or the environment. The inventory also includes non-federal contaminated sites for which the Government of Canada has accepted some or all financial responsibility. It does not include sites where contamination has been caused by, and which are under the control of, enterprise Crown corporations, private individuals, firms or other levels of government.
#'
#' @keywords contaminated sites
#' @keywords stressors
#'
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/home-accueil-eng.aspx
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/numbers-numeros-eng.aspx?qid=369592 (New-Brunswick)
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/numbers-numeros-eng.aspx?qid=369577 (Nova-Scotia)
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/numbers-numeros-eng.aspx?qid=671230 (Newfoundland)
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/numbers-numeros-eng.aspx?qid=671235 (Prince Edward Island)
#' @source https://www.tbs-sct.gc.ca/fcsi-rscf/numbers-numeros-eng.aspx?qid=671240 (Quebec)
#' @source https://open.canada.ca/data/en/dataset/1d42f7b9-1549-40aa-8ac6-0e0302ff2902 (full dataset, but in XML format)
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0011 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0011-contaminated_sites/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'NB_Tombstone_Data.csv'))) {
    # URL
    dat <- c('https://www.tbs-sct.gc.ca/fcsi-rscf/cscsvreport-eng.aspx?cttype=tombstone&qid=369592',
             'https://www.tbs-sct.gc.ca/fcsi-rscf/cscsvreport-eng.aspx?cttype=tombstone&qid=369577',
             'https://www.tbs-sct.gc.ca/fcsi-rscf/cscsvreport-eng.aspx?cttype=tombstone&qid=671230',
             'https://www.tbs-sct.gc.ca/fcsi-rscf/cscsvreport-eng.aspx?cttype=tombstone&qid=671235',
             'https://www.tbs-sct.gc.ca/fcsi-rscf/cscsvreport-eng.aspx?cttype=tombstone&qid=671240')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'NB_Tombstone_Data.csv'))
    download.file(dat[2], destfile = paste0(folder, 'NS_Tombstone_Data.csv'))
    download.file(dat[3], destfile = paste0(folder, 'NL_Tombstone_Data.csv'))
    download.file(dat[4], destfile = paste0(folder, 'PEI_Tombstone_Data.csv'))
    download.file(dat[5], destfile = paste0(folder, 'QC_Tombstone_Data.csv'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  dat <- list()
  suppressMessages(suppressWarnings({
    dat[[1]] <- readr::read_csv(paste0(folder, 'NB_Tombstone_Data.csv'))
    dat[[2]] <- readr::read_csv(paste0(folder, 'NS_Tombstone_Data.csv'))
    dat[[3]] <- readr::read_csv(paste0(folder, 'NL_Tombstone_Data.csv'))
    dat[[4]] <- readr::read_csv(paste0(folder, 'PEI_Tombstone_Data.csv'))
    dat[[5]] <- readr::read_csv(paste0(folder, 'QC_Tombstone_Data.csv'))
  }))

  # -----
  # WARNING: Likely not reproducible if data structure changes
  dat[[3]][,'Property Number'] <- as.character(dat[[3]][,'Property Number'])

  # -----
  data0011 <- bind_rows(dat) %>%
              filter(!is.na(Latitude) & !is.na(Longitude)) %>%
              as.data.frame() %>%
              st_as_sf(coords = c("Longitude","Latitude"), crs = 4269) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0011,
           dsn = "./data/data-format/data0011-contaminated_sites.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}

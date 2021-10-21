#' Data 0014: WoRMS North-West Atlantic species checklist
#'
#' CaRMS and WoRMS species list for the North-West Atlantic area of interest
#'
#' @keywords
#'
#' @source http://www.marinespecies.org/carms/aphia.php?p=checklist&action=search&gu_id=8048&tRank=220&inc_sub=1&status=pv
#'
#' @export
#'
#'

get_data0014 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0014-carms_checklist/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # NOTE: data transferred manually
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0014 <- read.csv(paste0(folder, "CaRMS_checklist_NW-Atlantic_2021-10-02.csv")) %>%
              select(-DrID, -Locality, -Latitude, -Longitude, -Source) %>%
              unique()

  # Check for duplicates and keep only proper Aphia IDs
  # NOTE: Process done on 2021-10-21
  # sum(duplicated(data0014$ScientificName))
  # nm <- data0014$ScientificName[duplicated(data0014$ScientificName)]
  # for(i in nm) print(data0014[data0014$ScientificName == i, ])

  # IDs to remove:
  rmID <- c(176879,380449,367181,407826,163203,367739,367719,
            384413,384417,379205,400321,159452,403214,151168)

  # Remove unaccepted taxa
  data0014 <- data0014[!data0014$AphiaID %in% rmID, ]
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  write.csv(data0014,
            "./data/data-format/data0014-carms_checklist.csv",
            row.names = FALSE)
  # _________________________________________________________________________ #
}

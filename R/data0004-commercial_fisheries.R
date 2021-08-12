#' Data 0004 : Gear type index for commercial fisheries (id: 0003)
#'
#' Gear type index for Zonal Interchange File Format (ZIFF) data between 2000 and 2020
#'
#' @keywords commercial fisheries
#' @keywords stressors
#'
#' @source Fisheries and Oceans Canada (2021). Index of fishing gears in departement of Fisheries and Oceans Canada’s Fisheries and Oceans Canada Zonal Interchange File Format (ZIFF) data. A compilation of landing data from logbook data between 2000 and 2020. [Data accessed 2021-07-15]
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0004 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0004-commercial_fisheries/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0004 <- read.csv(paste0(folder, 'Codes_engin.csv'))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  write.csv(x = data0004,
            file = "./data/data-format/data0004-commercial_fisheries.csv",
            row.names = FALSE)
  # _________________________________________________________________________ #
}

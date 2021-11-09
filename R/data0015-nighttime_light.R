#' Data 0015 : Nighttime Ligths
#'
#'
#'
#' @keywords light
#'
#' @source https://eogdata.mines.edu/products/vnl/#download
#' @source https://eogdata.mines.edu/nighttime_light/annual/v20/2012/
#' @source https://www.mdpi.com/2072-4292/13/5/922
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0015 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0015-nighttime_light/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # Download
  # NOTE: Files were loaded manually has access to them requires an account set up

  # URL
  dat <- c("VNL_v2_npp_201204-201303_global_vcmcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2013_global_vcmcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2014_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2015_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2016_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2017_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2018_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2019_global_vcmslcfg_c202102150000.average_masked.tif.gz",
           "VNL_v2_npp_2020_global_vcmslcfg_c202102150000.average_masked.tif.gz")
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # data0015 <-
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  # st_write(obj = data0015,
  #          dsn = "./data/data-format/data0015-nighttime_light.geojson",
  #          delete_dsn = TRUE,
  #          quiet = TRUE)
  # _________________________________________________________________________ #

}

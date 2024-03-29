#' @eval get_name("8509eeb1")
#'
#' @eval get_description("8509eeb1")
#'
#' @eval dp_params()
#' @param ... further arguments used in functions, if applicable.
#'
#' @family pipeline functions
#' @rdname data_pipelines
#' @seealso \code{\link{pipedat}}
#'
#' @keywords pipeline_id: 8509eeb1
#'
#' @examples
#' \dontrun{
#' dp_8509eeb1()
#' }
dp_8509eeb1 <- function(bbox = NULL, bbox_crs = NULL, timespan = NULL, ...) {
  # Output folders and other objects used
  uid <- "8509eeb1"
  nm <- glue::glue("{get_shortname(uid)}-{uid}")
  exist <- check_files(uid, ondisk = TRUE)
  path <- make_output(uid)

  if (!exist$clean) {
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # DOWNLOAD DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # If the data is downloaded from online sources.
    # In this particular case, access to the data requires a user account.
    # The data can be downloaded at the following url:
    # https://eogdata.mines.edu/nighttime_light/annual/v20/
    #
    # More specifically, the following urls lead to the specific datasets that were
    # used for this data pipeline:
    # urls <- c(
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2012/VNL_v2_npp_201204-201303_global_vcmcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2013/VNL_v2_npp_2013_global_vcmcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2014/VNL_v2_npp_2014_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2015/VNL_v2_npp_2015_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2016/VNL_v2_npp_2016_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2017/VNL_v2_npp_2017_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2018/VNL_v2_npp_2018_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2019/VNL_v2_npp_2019_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2020/VNL_v2_npp_2020_global_vcmslcfg_c202102150000.average_masked.tif.gz",
    #   "https://eogdata.mines.edu/nighttime_light/annual/v20/2021/VNL_v2_npp_2021_global_vcmslcfg_c202203152300.average_masked.tif.gz"
    # )

    # Decompress files
    gzfiles <- dir(glue::glue("{path}/raw/"), pattern = ".gz", full.names = TRUE)
    lapply(gzfiles, function(x) R.utils::gunzip(x, remove = FALSE))
    newfiles <- dir(glue::glue("{path}/raw/"), full.names = TRUE, pattern = ".tif$")
    # newfiles <- newfiles[!newfiles %in% gzfiles]
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # IMPORT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat <- stars::read_stars(newfiles, proxy = TRUE)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # FORMAT DATA
    # NOTE: optional
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Manual settings, to change if the pipeline and data are updated
    names(dat) <- 2012:2021
    if (is.null(timespan)) {
      years <- names(dat)
    } else {
      years <- timespan
    }
    # _________________________________________________________________________________________ #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE METADATA
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    meta <- get_metadata(
      pipeline_type = "data",
      pipeline_id = uid,
      pipeline_bbox = bbox,
      pipeline_bbox_crs = bbox_crs,
      pipeline_timespan = timespan,
      access = "2022-04-26",
      data_bbox = sf::st_bbox(dat),
      data_timespan = 2012:2021
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # CREATE BIBTEX
    # WARNING: mandatory
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    bib <- get_bib(uid)
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # APPLY SUBSETS AND CRS SPECIFIED BY USER
    # NOTE: optional, only if applicable
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    dat <- dp_parameters(
      dat,
      bbox = bbox,
      bbox_crs = bbox_crs
    )
    # _________________________________________________________________________________________ #

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # EXPORT
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Formatted data
    fm <- glue::glue("{path}/{nm}-{years}.tif")
    for (i in 1:length(years)) {
      uid <- which(names(dat) == years[i])
      stars::write_stars(dat[uid], fm[i])
    }
    # Delete decompressed file, as they are very big
    unlink(newfiles)

    # Metadata
    mt <- glue::glue("{path}/{nm}.yaml")
    yaml::write_yaml(meta, mt, column.major = FALSE)

    # Bibtex
    bi <- glue::glue("{path}/{nm}.bib")
    RefManageR::WriteBib(bib, file = bi, verbose = FALSE)
    # _________________________________________________________________________________________ #
  } # if exist clean, don't run again
}

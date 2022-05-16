#' Script to load and format all data using the pipedat package
#'
#' @export
get_data <- function() {
  old <- getOption("warn")
  on.exit(options(warn = old), add = TRUE)
  options(warn = -1)

  uid <- param$pipedat$data_pipelines
  crs <- param$crs
  bbox <- unlist(param$bbox$assessment) |> as.numeric()
  names(bbox) <- c("xmin","ymin","xmax","ymax")
  pipedat::pipedat(uid, crs = crs, bbox = bbox)
}
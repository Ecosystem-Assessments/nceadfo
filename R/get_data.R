#' Script to load and format all data using the pipedat package
#'
#' @export
get_data <- function() {
  uid <- param$pipedat$data_pipelines
  crs <- param$crs
  bbox <- param$bbox$assessment
  pipedat::pipedat(uid, crs = crs)
}
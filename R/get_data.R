#' Script to load and format all data using the pipedat package
#'
#' @export
get_data <- function() {
  global_parameters()
  uid <- param$pipedat$data_pipelines
  pipedat::pipedat(uid)
}
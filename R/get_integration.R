#' Script to load and format all data using the pipedat package
#'
#' @export
get_integrated <- function() {
  old <- getOption("warn")
  on.exit(options(warn = old), add = TRUE)
  options(warn = -1)

  uid <- param$pipedat$integration_pipelines
  pipedat::pipein(uid)
}
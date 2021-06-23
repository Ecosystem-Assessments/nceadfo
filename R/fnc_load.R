#' Load formatted data available in `./data/data-format/`
#'
#' The function imports data available in `./data/data-format/` regardless of its format.
#'
#' @param data_id `character` id of data to import in R session
#'
#' @keywords metadata
#' @keywords contact
#'
#' @export
#'
#' @details This function is used to avoid putting all formatted dataset in lazy load, yet still make it easy for them to be loaded. The user simply provides the id of the dataset and the function imports it in the R session
#'
#' @examples
#' ncea_load('data0001')

ncea_load <- function(data_id) {


}

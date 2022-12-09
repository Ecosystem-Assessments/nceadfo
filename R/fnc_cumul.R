#' Function to cumulate stars data, i.e. sum individual layers
#'
#' @export

cumul <- function(dat) {
  do.call("c", dat) |>
  stars::st_redimension() |>
  stars::st_apply(c(1,2), sum, na.rm = TRUE)      
}

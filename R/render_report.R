#' Render report
#'
#' @export
render_report <- function() {
  # file.copy("./figures/", "./report/", recursive = TRUE)
  suppressWarnings({
    setwd('./report/')
    bookdown::render_book(
      input = "index.Rmd",
      output_format = "bookdown::gitbook",
      config_file = "_bookdown.yml"
    )
    setwd('../')
  })
}

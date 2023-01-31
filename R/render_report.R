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
    # file.copy("./figures/", "./report/", recursive = TRUE)
  })
  from <- here::here("report","docs")
  to <- here::here("docs")
  if (file.exists(to)) fs::file_delete(to)
  fs::file_move(from, to)
}

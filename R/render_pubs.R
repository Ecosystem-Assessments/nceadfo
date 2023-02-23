#' Render report
#'
#' @export
render_report <- function() {
  # file.copy("./figures/", "./report/", recursive = TRUE)
  suppressWarnings({
    setwd('./pubs/report/')
    bookdown::render_book(
      input = "index.Rmd",
      output_format = "bookdown::gitbook",
      config_file = "_bookdown.yml"
    )
    setwd('../../')
    # file.copy("./figures/", "./report/", recursive = TRUE)
  })
  from <- here::here("pubs","report","docs")
  to <- here::here("docs","report")
  if (file.exists(to)) fs::file_delete(to)
  fs::file_move(from, to)
}

#' Render publications frontpage 
#'
#' @export
render_frontpage <- function() {
    setwd('./pubs/frontpage/')
    rmarkdown::render("index.Rmd")
    setwd('../../')
    out <- here::here("docs")
    chk_create(out)
    files <- list.files(here::here("pubs","frontpage"), full.names = TRUE)
    file.copy(from = files, to = out, recursive = TRUE)
}


#' Render webinar
#'
#' @export
render_webinar <- function() {
    setwd('./pubs/webinar/')
    rmarkdown::render("index.Rmd")
    setwd('../../')
    out <- here::here("docs","webinar")
    chk_create(out)
    files <- list.files(here::here("pubs","webinar"), full.names = TRUE)
    file.copy(from = files, to = out, recursive = TRUE)
}


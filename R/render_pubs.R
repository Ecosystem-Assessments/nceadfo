#' Render report
#'
#' @export
render_report <- function() {
  # file.copy("./figures/", "./report/", recursive = TRUE)
  suppressWarnings({
    setwd("./pubs/report/")
    bookdown::render_book(
      input = "index.Rmd",
      output_format = "bookdown::gitbook",
      config_file = "_bookdown.yml"
    )
    setwd("../../")
    # file.copy("./figures/", "./report/", recursive = TRUE)
  })
  from <- here::here("pubs", "report", "docs")
  to <- here::here("docs", "report")
  if (file.exists(to)) fs::file_delete(to)
  fs::file_move(from, to)
}

#' Render publications frontpage
#'
#' @export
render_frontpage <- function() {
  setwd("./pubs/frontpage/")
  rmarkdown::render("index.Rmd")
  setwd("../../")
  out <- here::here("docs")
  chk_create(out)
  files <- list.files(here::here("pubs", "frontpage"), full.names = TRUE)
  file.copy(from = files, to = out, recursive = TRUE)
}


#' Render webinar
#'
#' @export
render_webinar <- function() {
  setwd("./pubs/webinar/")
  rmarkdown::render("index.Rmd")
  setwd("../../")
  out <- here::here("docs", "webinar")
  chk_create(out)
  files <- list.files(here::here("pubs", "webinar"), full.names = TRUE)
  file.copy(from = files, to = out, recursive = TRUE)
}

#' Render nceahab report
#'
#' @export
render_nceahab <- function() {
  out <- here::here("pubs", "nceahab", "figures")
  chk_create(out)
  figs <- list(
    here::here("figures", "nceahab", "ncea_ceahab.png"),
    here::here("figures", "nceahab", "nceahab_species.png"),
    here::here("figures", "nceahab", "nceahab.png"),
    here::here("figures", "nceahab", "diagram.png"),
    here::here("figures", "metanetwork", "metanetwork.png"),
    here::here("figures", "contribution", "contribution_nceahab-2016_2021.png")
  )
  lapply(figs, function(x) file.copy(from = x, to = out))
  setwd(here::here("pubs", "nceahab"))
  rmarkdown::render("nceahab_report.md")
  setwd("../../")
}

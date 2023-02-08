#' Script to create a single bib file from the downloaded raw data using pipedat
#'
#' @export
get_bib <- function() {
  # List of bib files 
  files <- dir(
    here::here("data","data-raw"),
    recursive = TRUE,
    full.names = TRUE,
    pattern = ".bib$"
  )
  
  # Import them all 
  dat <- lapply(
    files, 
    RefManageR::ReadBib
  ) 
  dat <- do.call("c", dat) |>
    RefManageR::WriteBib(
    file = here::here("report","pipedat.bib"), 
    verbose = FALSE
  )
}


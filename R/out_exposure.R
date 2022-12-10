#' Function to export cumulative stressors and species (richness)
#'
#' @export

out_footporint <- function() {
  dat <- here::here("output","footprint") |>
         dir(full.names = TRUE) |>
         lapply(stars::read_stars)
  nm <- lapply(dat, names) |> unlist()
  names(dat) <- tools::file_path_sans_ext(nm)
  uid <- stringr::str_detect(nm, "stressors")
  stressors <- dat[uid]
  species <- dat[!uid]
  
  # Cumulative exposure 
  exposure <- lapply(stressors, function(x) x * species[[1]])
  
  # Export
  out <- here::here("output","exposure")
  chk_create(out)
  years <- c("2010_2012","2013_2015","2016_2018","2019_2021")
  nm <- glue::glue("exposure-{years}.tif")
  for(i in 1:length(exposure)) {
    stars::write_stars(
      exposure[[i]],
      dsn = here::here(out, nm[i]),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }
}

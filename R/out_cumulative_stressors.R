out_cum_stressors <- function() {
  out <- here::here("data","stressors","transformed")
  r <- dir(out, full.names = TRUE)
  r <- lapply(
    r, 
    function(x) {
      dir(x, recursive = TRUE, full.names = TRUE) |>
      lapply(stars::read_stars)
    }
  )
  
  # Cumulative stressors 
  r <- lapply(r, cumul)
  
  # Export 
  out <- here::here("output","cumulative_stressors")
  chk_create(out)
  years <- c("2010_2012","2013_2015","2016_2018","2019_2021")
  nm <- glue::glue("cumulative_stressors-{years}.tif")
  for(i in 1:length(r)) {
    stars::write_stars(
      r[[i]],
      dsn = here::here(out, nm[i]),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }
}

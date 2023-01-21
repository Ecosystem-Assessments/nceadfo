#' Species-specific sensitivity data to use for assessment
#'
#' @export
make_species_sensitivity <- function() {
  # Load trophic sensitivity data
  sensitivity <- vroom::vroom(
    here::here(
      "data",
      "data-raw",
      "species_vulnerability-1386850b",
      "species_vulnerability-1386850b.csv"
    )
  )

  # Export
  out <- here::here("data","cea_modules","species_sensitivity")
  chk_create(out)
  write.csv(sensitivity, file = here::here(out, "species_sensitivity.csv"), row.names = FALSE)
}
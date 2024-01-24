#' Prepare habitat data
#'
#' @export
make_habitats <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load & prepare data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  input <- here::here("data", "data-integrated", "cea_habitats_dfo-0a7a214c")
  files <- dir(input, full.names = TRUE, pattern = ".tif")
  dat <- lapply(files, stars::read_stars)
  names(dat) <- tools::file_path_sans_ext(basename(files))
  nm <- data.frame(datname = tools::file_path_sans_ext(names(dat)))
  name <- stringr::str_split(nm$datname, "-") |>
    lapply(
      function(x) {
        num <- as.numeric(x)
        iid <- !is.na(num)
        type <- ifelse(!iid[3], x[3], NA)
        data.frame(name = x[1], uid = x[2], type = type)
      }
    ) |>
    dplyr::bind_rows()
  for (i in seq_len(length(dat))) names(dat[[i]]) <- name$type[i]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export
  modules <- here::here("data", "cea_modules", "habitats")
  chk_create(modules)
  for (i in seq_len(length(dat))) {
    stars::write_stars(
      dat[[i]],
      dsn = here::here(modules, glue::glue("{name$type[i]}.tif")),
      quiet = TRUE,
      overwrite = TRUE
    )
  }

  # Habitat list
  here::here("data", "data-raw", "cea_habitats_dfo-49bda6fd", "cea_habitats_dfo-49bda6fd-habitat_list.csv") |>
    vroom::vroom() |>
    write.csv(file = here::here("data", "cea_modules", "habitats_list.csv"), row.names = FALSE)
}

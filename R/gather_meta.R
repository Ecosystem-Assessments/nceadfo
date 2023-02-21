#' Script to extract metadata and contact from raw data list
#'
#' @export
gather_meta <- function() {
  # List of raw data 
  files <- dir(
    here::here("data","data-raw"),
    recursive = TRUE,
    full.names = TRUE,
    pattern = ".yaml$"
  )

  # Import them all 
  dat <- lapply(
    files, 
    yaml::read_yaml
  ) 
  
  # Build table of raw data and contacts for the project
  meta <- contact <- list()
  for(i in 1:length(dat)) {
    d <- dat[[i]]
    
    # -----
    timespan <- suppressWarnings(range(d$description$timespan, na.rm = TRUE))
    timestart <- timespan[1]
    timestop <- timespan[2]
    if (timespan[1] == Inf) {
      timespan <- "-"
    } else {
      timespan <- ifelse(
        timestart == timestop, 
        timestart, 
        glue::glue("{timestart}-{timestop}")
      )
    }
    timespan <- as.character(timespan)
    
    # -----
    contact[[i]] <- suppressWarnings(dplyr::bind_rows(d$description$contacts))
    
    # -----
    meta[[i]] <- data.frame(
      uid = d$pipeline$pipeline_id,
      uid_url = d$pipeline$url,
      name = d$description$name,
      description = d$description$description,
      access = d$description$access_date,
      contact = paste0(contact[[i]]$contact_id, collapse = ","),
      source = paste0(glue::glue("@{d$description$citekey}"), collapse = "; "),
      timespan = timespan,
      url = d$description$url
    )
  }
  
  meta <- dplyr::bind_rows(meta)
  contact <- dplyr::bind_rows(contact) |>
             dplyr::select(contact_id, first_name, last_name, organization, email) |>
             dplyr::distinct() |>
             dplyr::arrange(contact_id)
  
  # Export
  out <- here::here("data","metadata")
  chk_create(out)
  write.csv(meta, here::here(out, "metadata.csv"), row.names = FALSE)
  write.csv(contact, here::here(out, "contacts.csv"), row.names = FALSE)
}


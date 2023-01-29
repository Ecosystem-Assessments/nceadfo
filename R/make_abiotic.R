#' Prepare abiotic data 
#'
#' @export
make_abiotic <- function() {
  library(stars)
  out <- here::here("data","data-abiotic")
  chk_create(out)
  grd <- stars::read_stars("data/grid/grid.tif")
  
  # Functions 
  mean_stars <- function(dat) {
    library(stars)
    do.call("c", dat) |>
    stars::st_redimension() |>
    stars::st_apply(c(1,2), mean, na.rm = TRUE)
  }
  names_stars <- function(dat) {
    lapply(dat, names) |> 
    unlist() |> 
    unname()
  }
  
  # Abiotic data
  ## Bathymetry
  bathy <- importdat("e775900b")
  
  ## Bio-ORACLE
  biooracle <- here::here("data","data-raw","bio-oracle-4d4292ca") |>
               dir(pattern = ".tif$", full.names = TRUE) |>
               lapply(stars::read_stars)
  
  ## BNAM currents 
  # currents <- importdat("906f1155")
  
  ## BNAM salinity
  salinity <- importdat("0d61380a")
  nm <- names_stars(salinity)
  salinity_bottom <- salinity[stringr::str_detect(nm, "Bottom")] |>
                     mean_stars() |>
                     setNames("Bottom_salinity")
  salinity_surface <- salinity[stringr::str_detect(nm, "0m")] |>
                      mean_stars() |>
                      setNames("Surface_salinity")
  salinity <- list(salinity_bottom, salinity_surface)
                      
  
  ## BNAM temperature
  temperature <- importdat("71944efd")
  nm <- names_stars(temperature)
  temperature_bottom <- temperature[stringr::str_detect(nm, "Bottom")] |>
                        mean_stars() |>
                        setNames("Bottom_temperature")
  temperature_surface <- temperature[stringr::str_detect(nm, "0m")] |>
                         mean_stars() |>
                         setNames("Surface_temperature")
  temperature <- list(temperature_bottom, temperature_surface)
                         
                      
  ## All together
  abiotic <- c(
    bathy,
    biooracle,
    # currents,
    salinity,
    temperature
  )

  source <- c(
    rep("e775900b", length(bathy)), 
    rep("4d4292ca",length(biooracle)),
    rep("0d61380a", length(salinity)),
    rep("71944efd", length(temperature))
  )
  cite <- c(
    rep("@gebco2021", length(bathy)), 
    rep("@bosch2022; @tyberghein2012; @assis2018",length(biooracle)),
    rep("@wang2018; @wang2018b", length(salinity)),
    rep("@wang2018; @wang2018a", length(temperature))
  )
  
  # Warp 
  abiotic <- lapply(abiotic, stars::st_warp, dest = grd)
  
  # Change names
  nm <- names_stars(abiotic)
  nm <- gsub("_gebco_2021-e775900b-n90_s0_w-90_e0.tif","", nm)
  nm <- gsub(".Mean.tif","", nm)
  nm <- gsub(".Mean.BOv2_2.tif","", nm)
  nm <- gsub(".BOv2_2.tif","", nm)
  nm <- gsub("Light.bottom","Light", nm)
  nm <- gsub("bio-oracle-4d4292ca-Present.Benthic.Mean.Depth.","Bottom_", nm)
  nm <- gsub("bio-oracle-4d4292ca-Present.Surface.","Surface_", nm)
  nm <- tolower(nm)
  nm
  
  # Verify bio-oracle ph and calcite bottom
  
  # Export
  for(i in 1:length(abiotic)) {
    stars::write_stars(
      abiotic[[i]],
      here::here(out, glue::glue("{nm[i]}.tif")),
      delete_dsn = TRUE
    )
  }
  
  # Abiotic data names 
  nm <- data.frame(
    filename = unlist(lapply(abiotic, names)),
    shortname = nm, 
    name = nm,
    source = source, 
    cite = cite
  ) |>
  dplyr::mutate(name = gsub("_"," ",name)) |>
  dplyr::mutate(name = gsub("\\."," ",name)) |>
  dplyr::mutate(name = stringr::str_to_sentence(name))
  write.csv(nm, file = here::here(out, "abiotic_list.csv"), row.names = FALSE)
}
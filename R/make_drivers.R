#' Prepare drivers data 
#'
#' @export
make_drivers <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load & prepare data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uid <- c(
    "3992e1a6", "6dba9a9f", "e2b7e6c4", "72312316", 
    "041a30d2", "99bb2d51", "aba5e90a", "84b6ea0b"
  )
  dat <- pipedat::importdat(uid)
  names(dat) <- tools::file_path_sans_ext(names(dat))  
  nm <- data.frame(datname = tools::file_path_sans_ext(names(dat)))
  name <- stringr::str_split(nm$datname, "-") |>
          lapply(
            function(x) {
              num <- as.numeric(x)
              iid <- !is.na(num)
              year <- ifelse(any(iid[-c(1,2)]), dplyr::last(num[iid]), NA)
              type <- ifelse(!iid[3], x[3], NA)
              data.frame(name = x[1],uid = x[2],type = type,year = year)
            }
          ) |>
          dplyr::bind_rows()
  name <- cbind(nm,name)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Divide into periods
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  flt_nm <- function(nm, id, tp, yr) {
    dplyr::filter(nm, uid == id, type %in% tp, year %in% yr) |>
    dplyr::select(datname, name)
  }

  cbn_dat <- function(dat, nm, id, tp, yr, met = "mean") {
    library(stars)
    nm <- flt_nm(nm, id, tp, yr)
    do.call("c", dat[nm$datname]) |>
    stars::st_redimension() |>
    stars::st_apply(c(1,2), met, na.rm = TRUE) |>
    setNames(
      ifelse(
        is.na(tp),
        glue::glue("{nm$name[1]}-{id}"),
        glue::glue("{nm$name[1]}-{id}-{tp}")
      )
    )
  }
  
  # Creating four periods for the assessment
  period_2010_2015 <- list(
    cbn_dat(dat, name, "3992e1a6","negative",2010:2015, "sum"), # SST anomalies -
    cbn_dat(dat, name, "3992e1a6","positive",2010:2015, "sum"), # SST anomalies +
    cbn_dat(dat, name, "6dba9a9f","negative",2010:2015, "mean"), # SBT anomalies - 
    cbn_dat(dat, name, "6dba9a9f","positive",2010:2015, "mean"), # SBT anomalies +
    cbn_dat(dat, name, "e2b7e6c4","DD",2010:2015, "mean"), # fisheries DD
    cbn_dat(dat, name, "e2b7e6c4","DNH",2010:2015, "mean"), # fisheries DNH
    cbn_dat(dat, name, "e2b7e6c4","DNL",2010:2015, "mean"), # fisheries DNL
    cbn_dat(dat, name, "e2b7e6c4","PLB",2010:2015, "mean"), # fisheries PLB
    cbn_dat(dat, name, "e2b7e6c4","PHB",2010:2015, "mean"), # fisheries PHB
    cbn_dat(dat, name, "72312316","interpolated_vessels",2017:2020,"mean"), # shipping
    cbn_dat(dat, name, "84b6ea0b","Present_Richness",NA,"mean"), # invasives
    cbn_dat(dat, name, "041a30d2","SummedRasters_AgriCover",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_ImperviousSurface",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_NutrientLoading",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_PopDensity",NA,"mean"),
    cbn_dat(dat, name, "aba5e90a",NA,2012:2015,"mean"), # coastdev
    cbn_dat(dat, name, "99bb2d51",NA,c(2016,2021),"mean") # dhi
  )
  period_2016_2021 <- list(
    cbn_dat(dat, name, "3992e1a6","negative",2016:2021, "sum"), # SST anomalies -
    cbn_dat(dat, name, "3992e1a6","positive",2016:2021, "sum"), # SST anomalies +
    cbn_dat(dat, name, "6dba9a9f","negative",2016:2019, "mean"), # SBT anomalies -
    cbn_dat(dat, name, "6dba9a9f","positive",2016:2019, "mean"), # SBT anomalies +
    cbn_dat(dat, name, "e2b7e6c4","DD",2016:2020, "mean"), # fisheries DD
    cbn_dat(dat, name, "e2b7e6c4","DNH",2016:2020, "mean"), # fisheries DNH
    cbn_dat(dat, name, "e2b7e6c4","DNL",2016:2020, "mean"), # fisheries DNL
    cbn_dat(dat, name, "e2b7e6c4","PLB",2016:2020, "mean"), # fisheries PLB
    cbn_dat(dat, name, "e2b7e6c4","PHB",2016:2020, "mean"), # fisheries PHB
    cbn_dat(dat, name, "72312316","interpolated_vessels",2017:2020,"mean"), # shipping
    cbn_dat(dat, name, "84b6ea0b","Present_Richness",NA,"mean"), # invasives
    cbn_dat(dat, name, "041a30d2","SummedRasters_AgriCover",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_ImperviousSurface",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_NutrientLoading",NA,"mean"),
    cbn_dat(dat, name, "041a30d2","SummedRasters_PopDensity",NA,"mean"),
    cbn_dat(dat, name, "aba5e90a",NA,2016:2021,"mean"), # coastdev
    cbn_dat(dat, name, "99bb2d51",NA,c(2016,2021),"mean") # dhi
  )
  
  # Export 
  out <- here::here("data","drivers","raw")
  p <- list(
    p1 = here::here(out, "2010_2015"),
    p2 = here::here(out, "2016_2021")    
  )
  lapply(p, chk_create)
    
  # Function to export
  exp_stress <- function(x, p) {
    lapply(
      x, 
      function(y) {
        stars::write_stars(
          y,
          dsn = here::here(p, glue::glue("{names(y)}.tif")),
          delete_dsn = TRUE, 
          silent = TRUE
        )
      }
    )  
  }
  
  exp_stress(period_2010_2015, p$p1)
  exp_stress(period_2016_2021, p$p2)

  # Log transform and normalize stressor data
  out <- here::here("data","drivers","transformed")
  p <- list(
    p1 = here::here(out, "2010_2015"),
    p2 = here::here(out, "2016_2021")
  )
  lapply(p, chk_create)
  
  # Get aoi
  aoi <- sf::st_read("data/aoi/aoi.gpkg")

  # Function to transform data 
  trdat <- function(dat) {
    dat <- dat[aoi] # Mask data 
    dat <- log(dat + 1) # Log transformation
    dat <- dat / (max(dat[[1]], na.rm = TRUE)) # Standardize 
    dat <- dat[aoi] # Mask data (again)
    dat
  }
  
  r <- list(period_2010_2015, period_2016_2021)
  for(i in 1:length(r)) {
      r[[i]] <- lapply(r[[i]], trdat)
  }  

  # Export
  exp_stress(r[[1]], p$p1)
  exp_stress(r[[2]], p$p2)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export formated data in format directly usable for the assessment
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Change names of drivers, and add group of drivers (for exported drivers list) 
  mod <- data.frame(
    from = c(
      "bottom_water_temperature_anomalies_atlantic.6dba9a9f.negative.tif",
      "bottom_water_temperature_anomalies_atlantic.6dba9a9f.positive.tif",
      "direct_human_impact.99bb2d51.tif",
      "fisheries_intensity.e2b7e6c4.DD.tif",
      "fisheries_intensity.e2b7e6c4.DNH.tif",
      "fisheries_intensity.e2b7e6c4.DNL.tif",
      "fisheries_intensity.e2b7e6c4.PHB.tif",
      "fisheries_intensity.e2b7e6c4.PLB.tif",
      "invasive_species_richness.84b6ea0b.Present_Richness.tif",
      "night_lights.aba5e90a.tif",
      "sea_surface_temperature_anomalies_dfo.3992e1a6.negative.tif",
      "sea_surface_temperature_anomalies_dfo.3992e1a6.positive.tif",
      "shipping_intensity.72312316.interpolated_vessels.tif",
      "watershed_activity_index.041a30d2.SummedRasters_AgriCover.tif",
      "watershed_activity_index.041a30d2.SummedRasters_ImperviousSurface.tif",
      "watershed_activity_index.041a30d2.SummedRasters_NutrientLoading.tif",
      "watershed_activity_index.041a30d2.SummedRasters_PopDensity.tif"
    ),
    to = c(
      "NegativeSBT",
      "PositiveSBT",
      "DirectHumanImpact",
      "FisheriesDD",
      "FisheriesDNH",
      "FisheriesDNL",
      "FisheriesPHB",
      "FisheriesPLB",
      "InvasiveSpecies",
      "CoastalDevelopment",
      "NegativeSST",
      "PositiveSST",
      "Shipping",
      "OrganicPollution",
      "InorganicPollution",
      "NutrientInput",
      "PopulationDensity"
    ),
    group = c(
      "Climate",
      "Climate",
      "Coastal",
      "Fisheries",
      "Fisheries",
      "Fisheries",
      "Fisheries",
      "Fisheries",
      "Marine traffic",
      "Coastal",
      "Climate",
      "Climate",
      "Marine traffic",
      "Coastal",
      "Coastal",
      "Coastal",
      "Coastal"
    ),
    fullname = c(
      "Negative sea bottom temperature anomalies",
      "Positive sea bottom temperature anomalies",
      "Direct human impact",
      "Demersal, destructive",
      "Demersal, non-destructive, high-bycatch",
      "Demersal, non-destructive, low-bycatch",
      "Pelagic, high-bycatch",
      "Pelagic, low-bycatch",
      "Invasive species",
      "Coastal development",
      "Negative sea surface temperature anomalies",
      "Positive sea surface temperature anomalies",
      "Shipping",
      "Organic pollution",
      "Inorganic pollution",
      "Nutrient input",
      "Population density"
    ),
    spatres = c(
      "0.2 degree",
      "0.2 degree",
      "< 1 to > 40000 $km^2$",
      "Lat/Lon",
      "Lat/Lon",
      "Lat/Lon",
      "Lat/Lon",
      "Lat/Lon",
      "0.01 degree",
      "15 arcsecond",
      "~2 $km^2$",
      "~2 $km^2$",
      "0.1 degree",
      "Modeled $100 $m^2$",
      "Modeled $100 $m^2$",
      "Modeled $100 $m^2$",
      "Modeled $100 $m^2$"
    ),   
    tempres = c(
      "Annual",
      "Annual",
      "Annual",
      "Event based",
      "Event based",
      "Event based",
      "Event based",
      "Event based",
      "Annual",
      "Annual",
      "Monthly",
      "Monthly",
      "Monthly",
      "-",
      "-",
      "-",
      "-"
    ),
    years = c(
      "2010-2019",
      "2010-2019",
      "2016,2021",
      "2010-2020",
      "2010-2020",
      "2010-2020",
      "2010-2020",
      "2010-2020",
      "-",
      "2012-2021",
      "2010-2021",
      "2010-2021",
      "2017-2020",
      "Betwen 2010 and 2019",
      "Betwen 2010 and 2019",
      "Betwen 2010 and 2019",
      "2015,2019"
    ),
    units = c(
      "negative anomalies",
      "positive anomalies",
      "population count",
      "$kg$",
      "$kg$",
      "$kg$",
      "$kg$",
      "$kg$",
      "$n$ species",
      "$nanoWatts$ $cm^{−2}$ $sr^{−1}$",
      "negative anomalies",
      "positive anomalies",
      "$n$ ship lanes",
      "% cover impervious surface",
      "% cover agriculture land",
      "$kg$ $N$ $yr^{-1}$",
      "person $ha^{-1}$"
    ),
    source = c(
      "@dfo2022a",
      "@dfo2022a",
      "@statisticscanada2016a; @statisticscanada2016b; @statisticscanada2017; @statisticscanada2022; @statisticscanada2022a; @statisticscanada2022b",
      "@dfo2021b",
      "@dfo2021b",
      "@dfo2021b",
      "@dfo2021b",
      "@dfo2021b",
      "@lyons2020",
      "@elvidge2021",
      "@dfo2021b",
      "@dfo2021b",
      "@gfw2022",
      "@guijarro-sabaniel2022",
      "@guijarro-sabaniel2022",
      "@kelly2021a; @guijarro-sabaniel2022",
      "@guijarro-sabaniel2022"
    )
  )

  # drivers 
  fold <- here::here("data","drivers","transformed")
  modules <- here::here("data","cea_modules","drivers")
  per <- dir(fold)
  for(i in 1:length(per)) {
    dat <- dir(here::here(fold, per[i]), full.names = TRUE) |>
           lapply(stars::read_stars)
    for(j in 1:length(dat)) {
      # Change names
      uid <- names(dat[[j]]) %in% mod$from
      nm <- glue::glue("{mod$to[j]}-{per[i]}")
      names(dat[[j]]) <- nm
      
      # Export
      out <- here::here(modules, per[i])
      chk_create(out)
      stars::write_stars(
        dat[[j]], 
        dsn = here::here(out,glue::glue("{nm}.tif")),
        quiet = TRUE, 
        overwrite = TRUE)
    }
  }
  
  # Drivers list 
  dir(modules, recursive = TRUE) |>
  basename() |> 
  tools::file_path_sans_ext() |>
  stringr::str_split("-") |>
  lapply(function(x) data.frame(drivers = x[1], period = x[2])) |>
  dplyr::bind_rows() |>
  dplyr::left_join(mod, by = c("drivers" = "to")) |>
  dplyr::arrange(group, drivers, period) |>
  write.csv(file = here::here("data","cea_modules","drivers_list.csv"), row.names = FALSE)
}
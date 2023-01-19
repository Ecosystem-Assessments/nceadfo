#' Predict species distribution
#'
#' @export
make_biotic <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load and prepare data DFO occurrence for analyses
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ------------------------------
  # Load data 
  ## Occurrences 
  datpath <- here::here("data", "data-integrated", "species_occurrences_nw_atlantic-18869625")
  occ <- vroom::vroom(
    here::here(
      datpath, 
      "species_occurrences_nw_atlantic-18869625-occurrences.csv"  
    )
  )
  
  ## Stations
  stations <- vroom::vroom(
    here::here(
      datpath,
      "species_occurrences_nw_atlantic-18869625-stations.csv"  
    )
  ) |>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326) |>
  dplyr::mutate(
    DEPTH = scale(-DEPTH),
    SURF_TEMP = scale(SURF_TEMP),
    BOTT_TEMP = scale(BOTT_TEMP),
    BOTT_SAL = scale(BOTT_SAL)
  )
  
  ## Species list 
  datpath <- here::here("data", "data-integrated", "species_list_nw_atlantic-893b37e8")
  species_list <- vroom::vroom(here::here(datpath, "species_list_nw_atlantic-893b37e8.csv"))
  
  ## Abiotic data
  abiotic <- here::here("data","data-abiotic") |>
             dir(pattern = ".tif$", full.names = TRUE) |>
             lapply(raster::raster) |>
             lapply(raster::scale) |>
             lapply(stars::st_as_stars)
  
  # ------------------------------------
  # Extract abiotic data at all stations
  abioticpt <- lapply(abiotic, stars::st_extract, at = stations) |>
               lapply(sf::st_drop_geometry) |>
               dplyr::bind_cols()
  stations <- cbind(stations, abioticpt)
  rm(abioticpt)
              
  # ------------------------------------
  # Divide by species
  species <- dplyr::group_by(occ, aphiaID) |>
             dplyr::group_split() |>
             lapply(
               function(x) {
                 dplyr::left_join(stations, x, by = c("MISSION", "SETNO")) |>
                 dplyr::mutate(presence = ifelse(is.na(aphiaID), 0, 1))
               }
             )
  
  # ------------------------------------  
  # Species list in data 
  sp <- lapply(species, function(x) {
    unique(x$aphiaID) |>
    na.omit()
  }) |>
  unlist()
  sp <- data.frame(aphiaID = sp) |>
        dplyr::left_join(species_list[, c("aphiaID","ScientificName")], by = "aphiaID") |>
        dplyr::mutate(
          shortname = tolower(
            glue::glue("{stringr::str_replace(ScientificName, ' ', '_')}-{aphiaID}")
          )
        )
  nSp <- nrow(sp)
  
  # -----------------------------------------
  # Environtal covariates to use for modeling 
  envVar <- c(
    "bathymetry",#"DEPTH", 
    "surface_temperature",#"SURF_TEMP",
    "bottom_temperature",#"BOTT_TEMP",
    "bottom_salinity",#"BOTT_SAL",
    "surface_salinity",
    "bottom_chlorophyll",
    "bottom_iron",
    "bottom_light",
    "bottom_nitrate",
    "bottom_phosphate",
    "bottom_phytoplankton",
    "bottom_primary.productivity",
    "bottom_silicate",
    "surface_calcite",
    "surface_chlorophyll",
    "surface_dissolved.oxygen",
    "surface_iron",
    "surface_nitrate",
    "surface_ph",
    "surface_phosphate",
    "surface_phytoplankton",
    "surface_primary.productivity",
    "surface_silicate"
  )

  # ------------------------------------
  # Environmental data as data.frame 
  nm <- lapply(abiotic, names) |> unlist() |> unname()
  abiotic <- abiotic[nm %in% envVar]
  envDat <- lapply(
    abiotic,
    function(x) {
      as.data.frame(x) |>
      dplyr::select(-x,-y)
    }
  ) |>
  dplyr::bind_cols()
  
  # ------------------------------------
  # Create template for rasters 
  env <- as(abiotic[[1]], "Raster")
  
  # ------------------------------------
  # Smoothing objects & parameters
  grd <- raster::raster("data/grid/grid.tif")
  resolution <- 1000
  bandwidth <- 5000
  
  # ------------------------------------
  # Binary parameters 
  th <- 0.5

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Smoothing function for predictions
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  smooth_predict <- function(dat, resolution, bandwidth, grd) {
    # Prepare data 
    ## Technically, a sf object could be provided to the btb_smooth function, but there 
    ## is a problem with the epsg checks when its length it greater than 4 characters. 
    ## See Issue #5 https://github.com/InseeFr/btb/issues/5
    dat <- data.frame(dat) |> na.omit() |>
           sf::st_as_sf(coords = c("x","y"), crs = 4326) |>
           sf::st_transform(crs = 32198) 
    dat <- cbind(sf::st_coordinates(dat), dat) |>
           sf::st_drop_geometry() |>
           dplyr::rename(x = X, y = Y)
    
    # Smoothing
    suppressMessages({
      kernel <- btb::btb_smooth(
                  pts = dat,
                  sEPSG = "32198",
                  iCellSize = resolution,
                  iBandwidth = bandwidth
                )
    })    
    kernel[,3] <- ifelse(kernel[,3,drop=T] > 1, 1, kernel[,3,drop=T])
    kernel <- sf::st_transform(kernel, crs = 4326)
    
    # Rasterize and return
    # WARNING: I am having an issue with transforming the outputs of fasterize to a stars object
    #          I therefore export raster objects directly, then export them to disk using the 
    #          raster functions instead.
    #          Long term this should be amended, but I already spent too much time on this.
    fasterize::fasterize(
      sf = kernel,
      raster = grd, 
      field = colnames(kernel)[3], 
      fun = "max"
    ) 
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Random forest - regression
  #
  # Iterate over species 
  # 
  # Steps:
  # 1. Regression model 
  # 2. Classification model NOTE: removed, but kept in code as comment
  # 3. Predict models
  # 4. Smooth predictions
  # ------------------------------------------------------------------------------
  # Outputs 
  out <- here::here("data","data-biotic")
  out <- list(
    regression_model = here::here(out,"random_forest_regression_rsq"),
    # classification_model = here::here("output","biotic","random_forest_classification"),
    regression = here::here(out, "random_forest_regression"),
    # classification = here::here(out, "random_forest_classification"),
    regression_smooth = here::here(out, "random_forest_regression_smoothing"),
    # classification_smooth = here::here(out, "random_forest_classification_smoothing")
    regression_binary = here::here(out, "random_forest_regression_binary")
  )
  lapply(out, chk_create)
  
  # Functions 
  export_rdata <- function(dat, out, nm) {
    save(
      dat, 
      file = here::here(out, glue::glue("{nm}.RData"))
    )
  }
  export_stars <- function(dat, out, nm) {
    stars::write_stars(
      dat,
      here::here(out, glue::glue("{nm}.tif")),
      quiet = TRUE,
      delete_dsn = TRUE
    )
  }
  export_raster <- function(dat, out, nm) {
    raster::writeRaster(
      x = dat,
      filename = here::here(out, glue::glue("{nm}.tif")),
      overwrite = TRUE
    )
    unlink(here::here(out, glue::glue("{nm}.tif.aux.xml")))
  }
  
  # Modeling
  for(i in 1:nSp) {
    cat(i, ' of ', nSp, '\r')
    nmSp <- sp$shortname[i]
    
    suppressWarnings({
    # ----------------
    reg <- randomForest::randomForest(
      formula = species[[i]]$presence ~ .,
      data = species[[i]][, envVar, drop = TRUE],
      importance = TRUE,
      keep.forest = TRUE,
      na.action = na.omit,
      ntree = 500,
      nodesize = 5
    )
    export_rdata(dplyr::last(reg$rsq), out$regression_model, nmSp) 
    
    # # ----------------
    # classif <- randomForest::randomForest(
    #   formula = as.factor(species[[i]]$presence) ~ .,
    #   data = species[[i]][, envVar, drop = TRUE],
    #   importance = TRUE,
    #   keep.forest = TRUE,
    #   na.action = na.omit,
    #   ntree = 500,
    #   nodesize = 5
    # )
    # export_rdata(classif, out$classification_model, nmSp)
    })
  
    # ----------------
    pred_reg <- stats::predict(reg, envDat)
    pred_reg <- raster::raster(
      crs = raster::projection(env),
      resolution = raster::res(env),
      ext = raster::extent(env),
      vals = round(pred_reg, 6)
    ) |>
    stars::st_as_stars() |>
    setNames(sp$shortname[i])
    export_stars(pred_reg, out$regression, nmSp)

    # # ----------------
    # pred_classif <- stats::predict(classif, envDat)
    # pred_classif <- raster::raster(
    #   crs = raster::projection(env),
    #   resolution = raster::res(env),
    #   ext = raster::extent(env),
    #   vals = as.numeric(pred_classif) - 1
    # ) |>
    # stars::st_as_stars() |>
    # setNames(sp$shortname[i])
    # export_stars(pred_classif, out$classification, nmSp)
  
    # ----------------
    smooth_reg <- smooth_predict(
      dat = pred_reg, 
      resolution = resolution, 
      bandwidth = bandwidth, 
      grd = grd
    )
    export_raster(smooth_reg, out$regression_smooth, nmSp)

    # # ----------------
    # smooth_classif <- smooth_predict(
    #   dat = pred_classif, 
    #   resolution = resolution, 
    #   bandwidth = bandwidth, 
    #   grd = grd
    # )
    # export_raster(smooth_classif, out$classification_smooth, nmSp)
    
    # ----------------
    binary_reg <- smooth_reg
    raster::values(binary_reg) <- ifelse(raster::values(binary_reg) > th, 1, 0)
    export_raster(binary_reg, out$regression_binary, nmSp)
  }

  # ----------------
  # Mask all data 
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  mask <- function(fold) {
    dat <- dir(fold, full.names = TRUE) |>
           lapply(stars::read_stars) 
    nm <- lapply(dat, names) |> unlist()    
    for(i in 1:length(dat)) {
      dat[[i]] <- dat[[i]][aoi]
      names(dat[[i]]) <- nm[i]
    }
           
    lapply(dat, function(x) {
     stars::write_stars(
       x,
       dsn = here::here(fold, names(x)),
       quiet = TRUE,
       overwrite = TRUE
     )
    })    
  }
  mask(here::here("data","data-biotic","random_forest_regression"))
  mask(here::here("data","data-biotic","random_forest_regression_smoothing"))
  mask(here::here("data","data-biotic","random_forest_regression_binary"))
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load and prepare marine mammals data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  out <- here::here("data","data-biotic","marine_mammals")
  chk_create(out)
  mm <- importdat("3d1bfb8e")
  mmList <- importdat("7c150fc3")[[1]] |>
            dplyr::select("ScientificName","aphiaID")
  aoi <- sf::st_read("data/aoi/aoi.gpkg")
  grd <- stars::read_stars("data/grid/grid.tif")
  mm <- lapply(mm, stars::st_warp, dest = grd) |>
        lapply(function(x) x[aoi]) |> # Mask data 
        lapply(function(x) x / max(x[[1]], na.rm = TRUE)) #|> 
        # lapply(function(x) x / x) #binary, keep previous lines in case I want continuous data

  # Remove empty species
  uid <- lapply(mm, function(x) max(x[[1]], na.rm = TRUE)) |> unlist()
  uid <- which(uid == 1)
  mm <- mm[uid]
  
  # Names
  nm <- lapply(mm, names) |>
        unlist() |>
        unname() |>
        tools::file_path_sans_ext() |>
        stringr::str_replace("marine_mammals_wwf_romm.3d1bfb8e.","")
  uid <- which(nm == "leatherback_turtle") 
  nm <- nm[-uid]
  mm[[uid]] <- NULL
  nm <- data.frame(common = nm, species = "")
  nm$species[nm$common == "blue_whale"] <- "Balaenoptera musculus"
  nm$species[nm$common == "fin_whale"] <- "Balaenoptera physalus"
  nm$species[nm$common == "humpback_whale"] <- "Megaptera novaeangliae"
  nm$species[nm$common == "minke_whale"] <- "Balaenoptera acutorostrata"
  nm$species[nm$common == "north_atlantic_right_whale"] <- "Eubalaena glacialis"
  nm$species[nm$common == "northern_bottlenose_whale"] <- "Hyperoodon ampullatus"
  nm$species[nm$common == "sei_whale"] <- "Balaenoptera borealis"
  nm$species[nm$common == "sperm_whale"] <- "Physeter macrocephalus"
  nm <- dplyr::left_join(nm, mmList, by = c("species" = "ScientificName"))
  nm$filename <- tolower(stringr::str_replace(nm$species, " ", "_"))
  nm$filename <- glue::glue("{nm$filename}-{nm$aphiaID}.tif")
  for(i in 1:length(mm)) names(mm[[i]]) <- nm$filename[i]
  
  # Export 
  out2 <- here::here(out,"continuous")
  chk_create(out2)
  for(i in 1:length(mm)) {
    stars::write_stars(
      mm[[i]],
      dsn = here::here(out2, nm$filename[i]),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }
  
  # Binary 
  out2 <- here::here(out,"binary")
  chk_create(out2)
  dat <- lapply(mm, function(x) x / x) 
  for(i in 1:length(dat)) {
    stars::write_stars(
      dat[[i]],
      dsn = here::here(out2, nm$filename[i]),
      delete_dsn = TRUE,
      quiet = TRUE
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export all species considered in the assessement in cea modules
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sp <- here::here("data","data-biotic","random_forest_regression_binary")
  mm <- here::here("data","data-biotic","marine_mammals","binary")
  out <- here::here("data","cea_modules","species")
  chk_create(out)
  file.copy(dir(sp, full.names = TRUE), out, overwrite = TRUE)
  file.copy(dir(mm, full.names = TRUE), out, overwrite = TRUE)
  
  # Species list 
  dir(out) |>
  tools::file_path_sans_ext() |>
  stringr::str_split("-") |>
  lapply(function(x) data.frame(shortname = x[1], aphiaID = x[2])) |>
  dplyr::bind_rows() |>
  dplyr::mutate(scientific_name = stringr::str_replace(shortname, "_", " ")) |>
  dplyr::mutate(scientific_name = stringr::str_to_sentence(scientific_name)) |>
  write.csv(file = here::here("data","cea_modules","species_list.csv"), row.names = FALSE)
}

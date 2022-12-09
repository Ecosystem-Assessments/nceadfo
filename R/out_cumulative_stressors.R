r <- dir("data/stressors/2010_2012", full.names = TRUE) |> lapply(stars::read_stars)
aoi <- sf::st_read("data/aoi/aoi.gpkg")

for(i in 1:length(r)) {
    print(paste0(i," of ", length(r)))
    
    dat <- r[[i]]
    
    # Mask data 
    dat <- dat[aoi]
    
    # Log transformation
    dat <- log(dat + 1)
    
    # Standardize 
    md <- max(dat[[1]], na.rm = TRUE)
    dat <- dat/md
    
    r[[i]] <- dat
}  

dat <- do.call("c", r) |>
       stars::st_redimension() |>
       stars::st_apply(c(1,2), sum, na.rm = TRUE)

#' Script to make grid for the assessment
#'
#' @export
summarise_data <- function() {
  assess <- yaml::read_yaml("data/data-config/assessment.yml")
  uid <- unlist(assess$assessment$stressors)

  grd <- stars::read_stars("data/grid/grid.tif")
  names(grd) <- "uid"
  stress <- as.data.frame(grd)
  for (i in 1:length(uid)) {
    dat <- importdat(uid[i])
    for (j in 1:length(dat)) {
      colnames(dat[[j]])[2] <- "intensity"
    }
    dat <- dplyr::bind_rows(dat) |>
      dplyr::group_by(uid) |>
      dplyr::summarise(intensity = mean(intensity, na.rm = TRUE)) |>
      dplyr::mutate(intensity = rcea::dataTrans(intensity))
    stress <- dplyr::left_join(stress, dat, by = "uid")
  }

  stress <- dplyr::select(stress, -uid)
  stress$cumfoot <- rowSums(stress[, 3:ncol(stress)], na.rm = TRUE)

  stress <- stars::st_as_stars(stress, coords = c("x", "y"), crs = 4326)
  plot(stress[1])
  plot(stress[2])
  plot(stress[3])
  plot(stress[4])
  plot(stress[5])
  plot(stress[6])
  plot(stress[7])
  plot(stress[8])
  plot(stress[9])
  stress2 <- stress[9]
  # WARNING: This is not what it should be, to modify accordingly.
  #          This should be automatic, not manual like this
  # Fisheries
  dat <- pipedat::importdat(uid["fish"])
  dd <- dnh <- list()
  dd[[1]] <- dat[1:5]
  dd[[2]] <- dat[6:10]
  dd[[3]] <- dat[11:15]
  dd[[4]] <- dat[16:20]
  dnh[[1]] <- dat[21:25]
  dnh[[2]] <- dat[26:30]
  dnh[[3]] <- dat[31:35]
  dnh[[4]] <- dat[36:40]
  dnl1 <- dat[51:55]
  dnl2 <- dat[56:60]
  phb1 <- dat[71:75]
  phb2 <- dat[76:80]
  plb1 <- dat[91:95]
  plb2 <- dat[91:100]

  for (i in 1:length(dnh)) {
    for (j in length(dnh[[i]])) {
      colnames(dnh[[i]][[j]])[2] <- "intensity"
    }
  }
  dnh <- lapply(dnh, function(x) {
    dplyr::bind_rows(x) |>
      dplyr::group_by(uid) |>
      dplyr::summarise(intensity = sum(intensity, na.rm = TRUE))
  })
  # grd <- sf::st_read("data/data-grid/grid_poly.geojson")
  grd <- stars::read_stars("data/grid/grid.tif")
  names(grd) <- "uid"
  grddat <- as.data.frame(grd) |>
    dplyr::left_join(dnh[[1]], by = "uid") |>
    dplyr::left_join(dnh[[2]], by = "uid") |>
    dplyr::left_join(dnh[[3]], by = "uid") |>
    dplyr::left_join(dnh[[4]], by = "uid")
  grd2 <- stars::st_as_stars(grddat, coords = c("x", "y"), crs = 4326)
  plot(grd2[1])
  plot(grd2[2])
  plot(grd2[3])
  plot(grd2[4])
  plot(grd2[5])


  dd1 <- dplyr::bind_rows(dd1) |>
    dplyr::group_by(uid) |>
    dplyr::summarise(intensity = sum(intensity, na.rm = TRUE))

  dd2 <- dplyr::bind_rows(dd2) |>
    dplyr::group_by(uid) |>
    dplyr::summarise(intensity = sum(intensity, na.rm = TRUE))

  # grd <- sf::st_read("data/data-grid/grid_poly.geojson")
  grd <- stars::read_stars("data/grid/grid.tif")
  names(grd) <- "uid"
  grddat <- as.data.frame(grd) |>
    dplyr::left_join(dd1, by = "uid") |>
    dplyr::left_join(dd2, by = "uid")


  grd2 <- stars::st_as_stars(grddat, coords = c("x", "y"), crs = 4326)
}

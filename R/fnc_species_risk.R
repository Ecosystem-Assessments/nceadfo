#' Assessment of species-scale risk for species
#'
#' @export
species_risk <- function(drivers,
                           vulnerability,
                           sensitivity) {

  # Vulnerability of species * Drivers intensity in cell j
  # Function to evaluate the relative intensity of all drivers (as a sum of
  # relative intensities) on each species involved in all triads.
  intensity <- sweep(vulnerability, MARGIN=2, drivers, `*`)

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Cumulative risk
  intensityCumulative <- rowSums(intensity, na.rm = TRUE)

  # Multiply with Trophic sensitivity for disconnected species x
  cumulativeRisk <- intensityCumulative * sensitivity

  # Identify whether species is affected (px)
  px <- as.numeric(as.logical(intensityCumulative))

  # Risk
  risk <- data.frame(risk_total = cumulativeRisk,
                     risk_direct = cumulativeRisk,
                     risk_indirect = 0,
                     intensity_total = intensityCumulative,
                     intensity_direct = intensityCumulative,
                     intensity_indirect = 0,
                     sensitivity = sensitivity,
                     count = 1,
                     px = px,
                     py = 0,
                     pz = 0,
                     focus = 1)

  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  # Return
  return(list(Cumulative_risk = risk))
}

#' Assessment of network-scale risk for species
#'
#' @export
CumulativeRisk <- function(focusID,
                           biotic,
                           drivers,
                           vulnerability,
                           sensitivity) {

  # Add focal species to cell
  biotic[focusID] <- TRUE

  # ID of species in cell j
  idsp <- which(biotic)

  # Select triads involving species found in cell j
  uid <- triads$i %in% idsp & triads$j %in% idsp & triads$j %in% idsp
  dat <- triads[uid, ]

  # Identify species of interest
  dat$focus_i <- (dat$i == focusID) * dat$pi
  dat$focus_j <- (dat$j == focusID) * dat$pj
  dat$focus_k <- (dat$k == focusID) * dat$pk
  dat$focus <- dat$focus_i + dat$focus_j + dat$focus_k

  # Select only focal species
  dat <- dat[dat$focus > 0, ]

  # Individual or cumulative risk
  # If species are not part of any triplet in that cell, then calculate individual risk
  if (nrow(dat) == 0) {
    # Trophic sensitivity of disconnected species
    uid <- sensitivity$speciesID == 1 & sensitivity$motifID == 1 & sensitivity$pathID == 1
    sensitivity_dix <- sensitivity$Sensitivity[uid]

    # Individual risk
    risk <- IndividualRisk(drivers = drivers,
                              vulnerability = vulnerability,
                              sensitivity = sensitivity_dix)

    # Cumulative risk and return
    return(list(Cumulative_risk = risk[[1]][focusID, ],
                Intensity_Total = risk[[2]][focusID, ],
                Intensity_Direct = risk[[3]][focusID, ],
                Intensity_Indirect = risk[[4]][focusID, ],
                Effect_total = risk[[5]][focusID, ],
                Effect_direct = risk[[6]][focusID, ],
                Effect_indirect = risk[[7]][focusID, ],
                Contribution_Intensity = NULL,
                Contribution_Effect = NULL))
  } else {
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Vulnerability of species * Drivers intensity in cell j
    # Function to evaluate the relative intensity of all drivers (as a sum of
    # relative intensities) on each species involved in all triads.
    intensity <- sweep(vulnerability, MARGIN=2, drivers, `*`)

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Cumulative risk
    intensityCumulative <- rowSums(intensity)

    # Add relative intensity of drivers on positions i,j,k
    dat$intensity_i <- intensityCumulative[dat$i]
    dat$intensity_j <- intensityCumulative[dat$j]
    dat$intensity_k <- intensityCumulative[dat$k]

    # Relative intensity of integrative pathways of effect (G)
    dat$intensity_total <- rowSums(dat[, c('intensity_i','intensity_j','intensity_k')])

    # Direct intensity
    dat$intensity_direct <- rep(intensityCumulative[focusID], nrow(dat))

    # Indirect intensity
    dat$intensity_indirect <- dat$intensity_total - dat$intensity_direct

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Total intensity per driver (considering all species)
    i <- intensity[dat$i, , drop = FALSE] # HERE I CAN GET INFO ON SPECIES
    j <- intensity[dat$j, , drop = FALSE]
    k <- intensity[dat$k, , drop = FALSE]
    intensity_total <- i + j + k

    # Direct intensity
    intensity_direct <- intensity[rep(focusID, nrow(dat)), , drop = FALSE]

    # Indirect intensity
    intensity_indirect <- intensity_total - intensity_direct

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Identify pathways of effect
    # Transform intensity as logical to identify pathways of effect
    dat$intensity_i_logic <- as.logical(dat$intensity_i)
    dat$intensity_j_logic <- as.logical(dat$intensity_j)
    dat$intensity_k_logic <- as.logical(dat$intensity_k)

    # Identify pathways of effect
    dat <- dat %>%
           mutate(pi = pi * intensity_i_logic,
                  pj = pj * intensity_j_logic,
                  pk = pk * intensity_k_logic,
                  nSp = intensity_i_logic + intensity_j_logic + intensity_k_logic,
                  path = nSp * (pi+pj+pk))

    # Individual id per pathway, including motif and position of interest
    dat <- left_join(dat, sensitivity,
                     by = c("sum_pos" = "motifID",
                            "focus" = "speciesID",
                            "path" = "pathID"))


    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # cumulative_risk(species may not be present)
    dat$risk_total <- dat$intensity_total * dat$Sensitivity
    dat$risk_direct <- dat$intensity_direct * dat$Sensitivity
    dat$risk_indirect <- dat$intensity_indirect * dat$Sensitivity

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Contribution of stressors to effects (not only intensity)
    effect_total <- intensity_total * dat$Sensitivity
    effect_direct <- intensity_direct * dat$Sensitivity
    effect_indirect <- intensity_indirect * dat$Sensitivity

    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Species contribution to indirect effects
    cont_i <- i %>% as.data.frame() %>% mutate(Taxa = rownames(i))
    cont_j <- j %>% as.data.frame() %>% mutate(Taxa = rownames(j))
    cont_k <- k %>% as.data.frame() %>% mutate(Taxa = rownames(k))

    # Indirect intensity
    contribution_intensity <- rbind(cont_i,cont_j,cont_k) %>%
                              gather('Stressor', 'Intensity', -Taxa) %>%
                              group_by(Taxa, Stressor) %>%
                              summarize_all(sum) %>%
                              spread('Stressor', 'Intensity', fill = 0) %>%
                              filter(Taxa != names(biotic[focusID])) %>%
                              as.data.frame(stringsAsFactors = FALSE)

    # Indirect effect
    cont_i <- i * dat$Sensitivity
    cont_j <- j * dat$Sensitivity
    cont_k <- k * dat$Sensitivity
    cont_i <- as.data.frame(cont_i) %>% mutate(Taxa = rownames(i))
    cont_j <- as.data.frame(cont_j) %>% mutate(Taxa = rownames(j))
    cont_k <- as.data.frame(cont_k) %>% mutate(Taxa = rownames(k))

    contribution_effect <- rbind(cont_i,cont_j,cont_k) %>%
                           gather('Stressor', 'Intensity', -Taxa) %>%
                           group_by(Taxa, Stressor) %>%
                           summarize_all(sum) %>%
                           spread('Stressor', 'Intensity', fill = 0) %>%
                           filter(Taxa != names(biotic[focusID])) %>%
                           as.data.frame(stringsAsFactors = FALSE)


    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Summarize stressor contributions
    intensity_total <- colSums(intensity_total)
    intensity_direct <- colSums(intensity_direct)
    intensity_indirect <- colSums(intensity_indirect)
    effect_total <- colSums(effect_total)
    effect_direct <- colSums(effect_direct)
    effect_indirect <- colSums(effect_indirect)


    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # Cumulative risk
    risk <- data.frame(risk_total = sum(dat$risk_total),
                       risk_direct = sum(dat$risk_direct),
                       risk_indirect = sum(dat$risk_indirect),
                       intensity_total = sum(dat$intensity_total),
                       intensity_direct = sum(dat$intensity_direct),
                       intensity_indirect = sum(dat$intensity_indirect),
                       sensitivity = sum(dat$Sensitivity),
                       count = nrow(dat),
                       px = sum(dat$px),
                       py = sum(dat$py),
                       pz = sum(dat$pz),
                       focus = mean(dat$focus))

    # Cumulative risk and return
    return(list(Cumulative_risk = risk,
                Intensity_Total = intensity_total,
                Intensity_Direct = intensity_direct,
                Intensity_Indirect = intensity_indirect,
                Effect_total = effect_total,
                Effect_direct = effect_direct,
                Effect_indirect = effect_indirect,
                Contribution_Intensity = contribution_intensity,
                Contribution_Effect = contribution_effect))
  }
}

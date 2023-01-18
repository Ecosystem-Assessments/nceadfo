#' Trophic sensitivity data to use for assessment
#'
#' @export
make_trophic_sensitivity <- function() {
  # Load trophic sensitivity data
  sensitivity <- vroom::vroom(
    here::here(
      "data",
      "data-raw",
      "species_trophic_sensitivity-0b902b1e",
      "species_trophic_sensitivity-0b902b1e.csv"
    )
  )

  # We need to modify the pathways of effect so that we only consider pathways
  # in which species are involved rather than effects to mortality, conservation
  # and attack rates. Ultimately, we want to evaluate the average trophic
  # sensitivity of pathways knowing that a certain species is affected.

  # We therefore add 3 logical columns names c('px','py','pz') that will identify
  # whether species x, y or z are affected by the pathway of effect
  library(tidyverse)
  library(magrittr)

  # Add columns
  sensitivity <- sensitivity %>%
                 mutate(r = str_detect(Pathways, '\\br\\b'),
                        r_x = str_detect(Pathways, 'r_x'),
                        r_y = str_detect(Pathways, 'r_y'),
                        r_z = str_detect(Pathways, 'r_z'),
                        m_y = str_detect(Pathways, 'm_y'),
                        m_z = str_detect(Pathways, 'm_z'),
                        beta = str_detect(Pathways, 'beta'),
                        delta = str_detect(Pathways, 'delta'),
                        gamma = str_detect(Pathways, 'gamma'),
                        mu = str_detect(Pathways, 'mu'),
                        nu = str_detect(Pathways, 'nu'),
                        omega = str_detect(Pathways, 'omega'))

  # Pathways involving species x
  uid <- sensitivity[, c('r','r_x','beta','gamma','mu','nu')] %>%
         rowSums() %>%
         as.logical()
  sensitivity$px <- uid

  # Pathways involving species y
  uid <- sensitivity[, c('r_y','m_y','beta','delta','mu','omega')] %>%
         rowSums() %>%
         as.logical()
  sensitivity$py <- uid

  # Pathways involving species z
  uid <- sensitivity[, c('r_z','m_z','delta','gamma','nu','omega')] %>%
         rowSums() %>%
         as.logical()
  sensitivity$pz <- uid

  # Group by pathways of effect at the species scale
  sensitivity <- sensitivity %>%
                 group_by(Motif, Species, px, py, pz) %>%
                 summarize(Sensitivity = mean(abs(Sensitivity))) %>%
                 arrange(Motif, Species, px, py, pz) %>%
                 as.data.frame()

  # Duplicate pathways for species y in apparent and exploitative competition
  # Change species x for species y and inverse px and py
  apy <- sensitivity %>%
         filter(Motif == 'ap' & Species == 'x') %>%
         mutate(Species = 'y') %>%
         rename(px = py, py = px) %>%
         select(Motif, Species, px, py, pz, Sensitivity)

  # Change species z for species y and inverse pz and py
  exy <- sensitivity %>%
         filter(Motif == 'ex' & Species == 'z') %>%
         mutate(Species = 'y') %>%
         rename(pz = py, py = pz) %>%
         select(Motif, Species, px, py, pz, Sensitivity)

  # Add pathways with no impact
  noeffect <- data.frame(Motif = c('tt','tt','tt','om','om','om','ap','ap','ap','ex','ex','ex'),
                         Species = c('x','y','z','x','y','z','x','y','z','x','y','z'),
                         px = FALSE,
                         py = FALSE,
                         pz = FALSE,
                         Sensitivity = 0,
                         stringsAsFactors = F)

  # Add to sensitivity data
  sensitivity <- rbind(sensitivity, apy, exy, noeffect) %>%
                 arrange(Motif, Species, px,py,pz)

  # Scale sensitivity values
  sensitivity <- sensitivity %>%
                 mutate(sensitivity_original = Sensitivity,
                        Sensitivity = Sensitivity / max(Sensitivity))


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Index to use with `motifcensus::motif_census_triplet()`
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # From the function:
  # Motifs:
  #  5: exploitative competition
  # 12: linear chain
  # 28: apparent competition
  # 36: omnivory
  # Species:
  #   1: exploitative competition bottom
  #   2: exploitative competition top
  #   3: linear chains bottom
  #   4: linear chains middle
  #   5: linear chains top
  #   9: apparent competition bottom
  #  10: apparent competition top
  #  11: omnivory bottom
  #  12: omnivory middle
  #  13: omnivory top

  # Create individual id per sensitivity value from motif, species and pathways
  # Must fit with output from `motifcensus::motif_census_triplet()`, although the
  # output might have to be modified

  # Identify pathways
  sensitivity <- sensitivity %>%
                 mutate(n = rowSums(.[,c('px','py','pz')]),
                        x = sensitivity$px * 1,
                        y = sensitivity$py * 2,
                        z = sensitivity$pz * 3) %>%
                 mutate(xyz = rowSums(.[,c('x','y','z')])) %>%
                 mutate(pathID = n * xyz) %>%
                 mutate(speciesID = as.numeric(as.factor(Species))) %>%
                 select(-n,-x,-y,-z,-xyz)

  # Identify motifs
  motifID <- data.frame(motif = c('ap','di','ex','om','pa','tt'),
                        motifID = c(28,1,5,36,2,12),
                        stringsAsFactors = F)

  # Add to sensitivity
  sensitivity <- left_join(sensitivity, motifID, by = c('Motif' = 'motif'))


  # Export
  out <- here::here("data","cea_modules","trophic_sensitivity")
  chk_create(out)
  write.csv(sensitivity, file = here::here(out, "trophic_sensitivity.csv"), row.names = FALSE)
}
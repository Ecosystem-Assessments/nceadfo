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

       # Change x = i, y = j, z = k
       sensitivity <- dplyr::mutate(sensitivity,
              Species = dplyr::case_when(
                     Species == "x" ~ "i",
                     Species == "y" ~ "j",
                     Species == "z" ~ "k"
              )
       )

       # We need to modify the pathways of effect so that we only consider pathways
       # in which species are involved rather than effects to mortality, conservation
       # and attack rates. Ultimately, we want to evaluate the average trophic
       # sensitivity of pathways knowing that a certain species is affected.

       # We therefore add 3 logical columns names c('pi','pj','pk') that will identify
       # whether species x, y or z are affected by the pathway of effect
       library(tidyverse)
       library(magrittr)

       # Add columns
       sensitivity <- sensitivity %>%
              mutate(
                     r = str_detect(Pathways, "\\br\\b"),
                     r_x = str_detect(Pathways, "r_x"),
                     r_y = str_detect(Pathways, "r_y"),
                     r_z = str_detect(Pathways, "r_z"),
                     m_y = str_detect(Pathways, "m_y"),
                     m_z = str_detect(Pathways, "m_z"),
                     beta = str_detect(Pathways, "beta"),
                     delta = str_detect(Pathways, "delta"),
                     gamma = str_detect(Pathways, "gamma"),
                     mu = str_detect(Pathways, "mu"),
                     nu = str_detect(Pathways, "nu"),
                     omega = str_detect(Pathways, "omega")
              )

       # Pathways involving species x
       uid <- sensitivity[, c("r", "r_x", "beta", "gamma", "mu", "nu")] %>%
              rowSums() %>%
              as.logical()
       sensitivity$pi <- uid

       # Pathways involving species y
       uid <- sensitivity[, c("r_y", "m_y", "beta", "delta", "mu", "omega")] %>%
              rowSums() %>%
              as.logical()
       sensitivity$pj <- uid

       # Pathways involving species z
       uid <- sensitivity[, c("r_z", "m_z", "delta", "gamma", "nu", "omega")] %>%
              rowSums() %>%
              as.logical()
       sensitivity$pk <- uid

       # Group by pathways of effect at the species scale
       sensitivity <- sensitivity %>%
              group_by(Motif, Species, pi, pj, pk) %>%
              summarize(Sensitivity = mean(abs(Sensitivity))) %>%
              arrange(Motif, Species, pi, pj, pk) %>%
              as.data.frame()

       # Duplicate pathways for species y in apparent and exploitative competition
       # Change species x for species y and inverse pi and pj
       apj <- sensitivity %>%
              filter(Motif == "ap" & Species == "i") %>%
              mutate(Species = "j") %>%
              rename(pi = pj, pj = pi) %>%
              select(Motif, Species, pi, pj, pk, Sensitivity)

       # Change species z for species y and inverse pk and pj
       exj <- sensitivity %>%
              filter(Motif == "ex" & Species == "k") %>%
              mutate(Species = "j") %>%
              rename(pk = pj, pj = pk) %>%
              select(Motif, Species, pi, pj, pk, Sensitivity)

       # Add pathways with no impact
       noeffect <- data.frame(
              Motif = c("tt", "tt", "tt", "om", "om", "om", "ap", "ap", "ap", "ex", "ex", "ex"),
              Species = rep(c("i", "j", "k"), 4),
              pi = FALSE,
              pj = FALSE,
              pk = FALSE,
              Sensitivity = 0,
              stringsAsFactors = F
       )

       # Add to sensitivity data
       sensitivity <- rbind(sensitivity, apj, exj, noeffect) %>%
              arrange(Motif, Species, pi, pj, pk)

       # Scale sensitivity values
       dix <- sensitivity$Sensitivity[sensitivity$Motif == "di" & sensitivity$pi][1]
       sensitivity <- sensitivity %>%
              mutate(
                     sensitivity_original = Sensitivity,
                     sensitivity_scaled = Sensitivity / max(Sensitivity),
                     sensitivity_1 = Sensitivity / dix
              ) |>
              select(-Sensitivity)


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

       pos <- data.frame(
              Motif = c("tt", "tt", "tt", "om", "om", "om", "ap", "ap", "ap", "ex", "ex", "ex"),
              Species = rep(c("i", "j", "k"), 4),
              SpeciesID = c(3, 4, 5, 11, 12, 13, 9, 9, 10, 1, 2, 2)
       )

       # Identify pathways
       sensitivity <- sensitivity %>%
              mutate(
                     n = rowSums(.[, c("pi", "pj", "pk")]),
                     x = sensitivity$pi * 1,
                     y = sensitivity$pj * 2,
                     z = sensitivity$pk * 3
              ) %>%
              mutate(xyz = rowSums(.[, c("x", "y", "z")])) %>%
              mutate(pathID = n * xyz) %>%
              dplyr::left_join(pos, by = c("Motif", "Species")) |>
              select(-n, -x, -y, -z, -xyz)

       # Identify motifs
       motifID <- data.frame(
              motif = c("ap", "di", "ex", "om", "pa", "tt"),
              motifID = c(28, 1, 5, 36, 2, 12),
              stringsAsFactors = F
       )

       # Add to sensitivity
       sensitivity <- left_join(sensitivity, motifID, by = c("Motif" = "motif")) |>
              na.omit() |>
              select(
                     pathID, SpeciesID, motifID, Species, pi, pj, pk,
                     sensitivity_original, sensitivity_scaled, sensitivity_1
              )

       # Pivot wider
       x <- tidyr::pivot_wider(
              sensitivity,
              id_cols = c("pathID", "motifID", "pi", "pj", "pk"),
              names_from = Species,
              values_from = sensitivity_1
       ) |>
              # dplyr::left_join(mt, by = "Motif") |>
              na.omit() #|>
       # dplyr::select(-Motif)

       y <- tidyr::pivot_wider(
              sensitivity,
              id_cols = c("motifID", "pathID", "pi", "pj", "pk"),
              names_from = Species,
              values_from = SpeciesID
       ) |>
              # dplyr::left_join(mt, by = "Motif") |>
              na.omit()

       # Export
       out <- here::here("data", "cea_modules", "trophic_sensitivity")
       chk_create(out)
       write.csv(sensitivity, file = here::here(out, "trophic_sensitivity.csv"), row.names = FALSE)
}

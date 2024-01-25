#' Prepare habitat sensitivity data
#'
#' @export
make_habitats_sensitivity <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load & prepare data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  input <- here::here("data", "data-raw", "cea_habitats_dfo-49bda6fd")
  dat <- vroom::vroom(here::here(input, "cea_habitats_dfo-49bda6fd-habitat_sensitivity.csv"))


  # Data.frame to setup equivalencies between stressors used for species and habitat assessments
  # Duplicated stressors (temperatures, + and -)
  dup <- c("climate_BottomTempChange", "climate_SurfaceTempChange")
  uid <- dat$STRESSOR_CODE %in% dup
  tmp <- dat[uid, ]
  tmp$STRESSOR_CODE <- glue::glue("{tmp$STRESSOR_CODE}_pos")
  dat$STRESSOR_CODE[uid] <- glue::glue("{dat$STRESSOR_CODE[uid]}_neg")
  dat <- dplyr::bind_rows(dat, tmp)

  # Equivalencies
  equiv <- data.frame(
    STRESSOR_CODE = c(
      "climate_Acidification",
      "climate_BottomTempChange_neg",
      "climate_BottomTempChange_pos",
      "DirectHumanImpact_Trampling",
      "fishing_DemersalHabitatModifying",
      "fishing_DemersalNonHabitatModifying_HighByc",
      "fishing_DemersalNonHabitatModifying_LowByc",
      "fishing_PelagicHighByCatch",
      "fishing_PelagicLowByCatch",
      "InvasiveSpecies",
      "CoastalEngineering_HabitatAlteration",
      "climate_SurfaceTempChange_neg",
      "climate_SurfaceTempChange_pos",
      "shipping",
      "PollutantInput_Organic",
      "PollutantInput_Inorganic",
      "NutrientInput_Oligo",
      "PollutantInput_UrbanRunoff"
    ),
    drivers = c(
      "Acidification",
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
    )
  )

  sens <- dplyr::left_join(
    equiv,
    dat,
    by = "STRESSOR_CODE",
    relationship = "many-to-many"
  ) |>
    dplyr::select(drivers, habitats = HabitatCODE, VSCORE)


  habitat_sensitivity <- sens |>
    dplyr::distinct() |>
    tidyr::pivot_wider(
      id_cols = "habitats",
      names_from = "drivers",
      values_from = "VSCORE"
    )


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export
  modules <- here::here("data", "cea_modules", "habitats_sensitivity")
  chk_create(modules)
  write.csv(habitat_sensitivity, file = here::here(modules, "habitats_sensitivity.csv"), row.names = FALSE)
}

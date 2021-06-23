#' Pipeline d'analyse
#'
#' Pipeline d'analyse du projet d'évaluation des effets cumulatifs de la navigation dans le Saguenay et le Saint-Laurent
#'
#' @keywords pipeline
#'
#' @export
#'
#' @details Cette fonction exécute l'ensemble des fonctions pour effectuer l'analyse des effets cumulatifs
#'

pipeline <- function(pipeline_metadata = FALSE,
                     pipeline_data = FALSE,
                     pipeline_integration = FALSE,
                     pipeline_analysis = FALSE,
                     pipeline_figure = FALSE,
                     pipeline_report = TRUE) {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_metadata) {
    # Metadata
    metadata('metadata')

    # Contacts
    metadata('contact')
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_data) {
    # Individual datasets
    get_data0001()
    get_data0002()

  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_integration) {
    # Composantes valorisées
    cv_habitat()
    cv_berge()
    # cv_mammiferes_marins()

    # Stresseurs
    st_ancrage()
    st_deversement()
    st_navigation()

  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_report) {
    suppressWarnings({
      setwd('./report/')
      bookdown::render_book(input = "index.Rmd",
                            output_format = "bookdown::gitbook",
                            config_file = "_bookdown.yml")
      setwd('../')
    })
  }
}

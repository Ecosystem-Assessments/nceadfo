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
                     pipeline_format = FALSE,
                     pipeline_analysis = FALSE,
                     pipeline_figure = FALSE,
                     pipeline_report = TRUE) {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_metadata) {
    # Metadata
    update_metadata("raw")
    update_metadata("integrated")

    # Contacts
    update_contact()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_data) {
    # Individual datasets
    get_data0001()
    get_data0002()
    get_data0003()
    get_data0004()
    get_data0005()

  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_format) {
    # -------------------
    # Study area
    int_aoi()

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

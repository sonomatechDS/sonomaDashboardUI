#' Pollution Rose UI
#'
#' Used alongside pollutionRoseServer() to add a pollution rose with 1hr data to the app.
#'
#' @param id string ID used to link pollutionRoseUI() to pollutionRoseServer()
#'
#' @return
#' @importFrom shiny NS plotOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools tags
#' @export
#'
#' @examples
pollutionRoseUI <- function(id) {
  ns <- shiny::NS(id)
  
  # htmltools::tags$head(
  #   htmltools::tags$style(HTML("
  #                              shiny-output-error-myClass {
  #                               color: green;
  #                              }"))
  #   )
  
  shiny::plotOutput(ns("PollutionRose"),
                    height = "550px") %>% shinycssloaders::withSpinner()
}

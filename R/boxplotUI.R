#' Boxplot UI Function
#'
#' Use alongside boxplotServer() to add boxplot to app.
#'
#' @param id string ID label that links boxplotUI() to boxplotServer()
#'
#' @return
#' @importFrom shiny NS fluidRow column plotOutput
#' @export
#'
#' @examples
boxplotUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(shiny::column(10, offset=1,
                  shiny::plotOutput(outputId = ns('Boxplot'), height = "600px")))
}

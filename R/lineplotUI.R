#' Lineplot Server Function
#'
#' Use alongside lineplotServer() to add lineplot of concentration data to app.
#'
#' @param id string ID label that links lineplotUI() to lineplotServer()
#'
#' @return
#' @importFrom shiny NS fluidRow column plotOutput
#' @export
#'
#' @examples
lineplotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(column(10, offset=1,
                         shiny::plotOutput(ns('Lineplot'))))

}

#' Boxplot Mod UI Function
#'
#' Use alongside boxplotModServer() to add modified boxplot to app.
#' Modified boxplot allows user to specify extent of box limits and whisker
#' limits by passing columns with minimum and maximum extent values.
#'
#' @param id string ID label that links boxplotModUI() to boxplotModServer()
#'
#' @return
#' @importFrom shiny NS fluidRow column plotOutput
#' @export
#'
#' @examples
boxplotModUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(shiny::column(10, offset=1,
                                shiny::plotOutput(outputId = ns('Boxplot'), height = "600px")))
}

#' Data Table UI funtion
#'
#' Used alongside tableServer() to add a data table (wrapped in a fluidRow)
#' to an app.
#'
#' @param id string ID label that links tablueUI() to tableServer()
#'
#' @return
#' @importFrom htmltools div
#' @importFrom shiny column NS fluidRow
#' @importFrom DT dataTableOutput
#' @export
#'
#' @examples
tableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(column(12,
                  htmltools::div(DT::dataTableOutput(ns('Table')),
                      style = "font-size:100%"),
                  align = 'center'),
           style = 'padding-top:20px')
}

#' Header table for selected map site
#'
#' Displays specified columns from data passed to sitemapServer function
#' for site clicked on map.
#'
#' @param id string ID to match with a call to output$<labelId> in the
#'                       server function of the app.
#'
#' @return object Collection of shiny UI elements
#' @importFrom DT dataTableOutput
#' @importFrom shiny tagList wellPanel fluidRow column NS
#' @importFrom htmltools div
#' @export
#'
#' @examples
mapSelectHeaderUI <- function(id) {
  ns <- shiny::NS(id)
  t <- shiny::tagList(
    shiny::wellPanel(style = 'background:#F8F8F8',
            shiny::fluidRow(column(2, offset = 1, align = 'right',
                            htmltools::div(style='font-size:24px; padding-top:40px; padding-right:5%',
                                           "Selected Site:")),
                     shiny::column(8,
                            htmltools::div(style = 'padding-right:10%',
                                           DT::dataTableOutput(ns('mapHeaderTable')))))))

  return(t)
}

#' Timeseries UI Function
#'
#' Used alongside timeseriesServer() to add a timeseries of 1hr and optionally 24hr
#' data (wrapped in a fluidRow) to an app. Timeseries will include zoom
#' functionality, where a box can be dragged across the plot area and
#' double-clicked. To reset zoom, double-click the plot area again.
#'
#' @param id string ID label that links timeseriesUI() to timeseriesServer()
#'
#' @return
#' @importFrom shiny NS tagList fluidRow column plotOutput brushOpts
#' @importFrom htmltools div
#' @importFrom shinycssloaders withSpinner
#' @export
#'
#' @examples
timeseriesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(10, offset = 1,
                      htmltools::div(
                        style = 'padding-left:70px;',
                        'Drag across plot area and double-click to zoom. Double-click again to reset.'
                      )
                    )),
  shiny::fluidRow(shiny::column(10, offset = 1,
                  shiny::plotOutput(outputId = ns('Timeseries'),
                             dblclick = ns('TSDoubleclick'),
                             brush = shiny::brushOpts(id = ns('TSBrush'),
                                                      resetOnNew = TRUE),
                             height = '450px') %>% shinycssloaders::withSpinner())))
}


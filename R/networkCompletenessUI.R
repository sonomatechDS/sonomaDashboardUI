#' Network Completeness Plot UI
#'
#' Used alongside networkCompletenessUI() to add a network completeness
#' plot to app.
#'
#' @param id string ID that links networkCompletenessUI() to networkCompletenessServer()
#' @param add_brush boolean If TRUE (default), brush and double-click added to
#'                          rendered plot. This allows users to display details
#'                          of selected data below the plot.
#'
#' @return
#' @importFrom shiny NS tagList column plotOutput
#' @export
#'
#' @examples
networkCompletenessUI <- function(id, add_brush = TRUE) {
  ns <- shiny::NS(id)

  if (add_brush) {
    col_plot <- shiny::column(10, offset = 1,
                              htmltools::div(style='padding-top:15px; padding-bottom:15px; font-size:16px; color:black;',
                                             "Drag and double-click to display details below plot. Double-click to reset."),
                              shiny::plotOutput(ns('NetworkCompleteness'),
                                                height = '500px',
                                                brush = shiny::brushOpts(id = ns('NCBrush'),
                                                                         resetOnNew = TRUE,
                                                                         clip=TRUE),
                                                dblclick = ns('NCDoubleclick')),
                              shiny::tableOutput(ns('NCSelect')))
  } else {
    col_plot <- shiny::column(10, offset = 1,
                              htmltools::div(style='padding-top:15px;',
                                shiny::plotOutput(ns('NetworkCompleteness'),
                                                  height = '500px')))
  }

  shiny::tagList(shiny::fluidRow(col_plot))
}

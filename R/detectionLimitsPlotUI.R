#' Detection Limits Plot UI
#'
#' Used alongside detectionLimitsPlotServer() to add a detection limits plot to app.
#' Method labels are plotted according to MDL and % of observations above the
#' MDL, colored by parameter and sized by number of observations.
#'
#' @param id string ID label that links detectionLimitsPlotUI() to detectionLimitsPlotServer()
#'
#' @return
#' @importFrom shiny NS fluidRow column brushOpts plotOutput
#' @export
#'
#' @examples
detectionLimitsPlotUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(shiny::column(10, offset=1,
                                shiny::plotOutput(outputId = ns('DetectionLimitsPlot'),
                                           dblclick = ns("DetectionLimitsDoubleclick"),
                                 brush = shiny::brushOpts(id = ns("DetectionLimitsBrush"),
                                                   resetOnNew = TRUE),
                                 height = '400px')))
}

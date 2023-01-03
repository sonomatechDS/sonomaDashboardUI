#' Boxplot/Barplot with pattern UI function
#' 
#' Creates a barplot or boxplot with pattern fill depending on number of variables
#'
#' @param id id that links server function to ui function
#' @param ... other arguments to plotOutput (e.g. height, width)
#'
#' @return
#' @importFrom shiny NS fluidRow column plotOutput
#' @export
#'
#' @examples
geompatternModUI <- function (id, ...) 
{
  ns <- shiny::NS(id)
  shiny::fluidRow(shiny::column(10, offset = 1, shiny::plotOutput(outputId = ns("geom_pattern"), ...)))
}

#' Scatter plot UI
#'
#' Used alongside scatterServer() to add a scatter plot to app.
#'
#' @param id string ID that links scatterUI() to scatterServer()
#' @param add_regression_option bool (optional) If TRUE (default),
#'                                   adds checkbox to add linear regression line
#'                                   to scatterplot.
#' @param add_one2one_option bool (optional) If TRUE (default),
#'                                   adds checkbox to add one-to-one line
#'                                   to scatterplot.
#'
#' @return
#' @importFrom shiny NS tagList fluidRow column plotOutput checkboxInput
#' @importFrom htmltools h4 div
#' @importFrom shinycssloaders withSpinner
#' @export
#'
#' @examples
scatterUI <- function(id,
                      add_regression_option = TRUE,
                      add_one2one_option = TRUE) {
  ns <- shiny::NS(id)
  colors <- sonoma_color_palette(4)


  one_to_one <- shiny::tagList(htmltools::tags$i("Note: one-to-one line may be outside plot limits."),
                               htmltools::div(style = paste0('color:', colors[3]),
                                              shiny::checkboxInput(ns("OneToOne"),
                                                                   label = 'One-to-one',
                                                                   value=FALSE)))

  lin_reg <- htmltools::div(style = paste0('color:', colors[4]),
                            shiny::checkboxInput(ns("LinearRegression"),
                                                 label = 'Linear Regression',
                                                 value=FALSE))

  if (add_regression_option & add_one2one_option) {
    boxes <- column(3, align = 'left',
                    htmltools::div(style = 'padding-top:150px',
                                   htmltools::h4("Toggle Reference Lines:")),
                    one_to_one,
                    lin_reg
    )
  } else if (add_regression_option & !add_one2one_option) {
    boxes <- column(3, align = 'left',
                    htmltools::div(style = 'padding-top:150px',
                                   htmltools::h4("Toggle Reference Lines:")),
                    lin_reg
    )
  } else if (add_one2one_option & !add_regression_option) {
    boxes <- column(3, align = 'left',
                    htmltools::div(style = 'padding-top:150px',
                                   htmltools::h4("Toggle Reference Lines:")),
                    one_to_one
    )
  } else {
    boxes <- ''
  }


  shiny::tagList(shinyjs::useShinyjs(),
                 htmltools::div(id = ns('diffParams'),
                                shiny::fluidRow(shiny::column(9,
                  shiny::plotOutput(ns("Scatter"), height = '500px') %>% shinycssloaders::withSpinner()),
                  boxes)),
                 htmltools::div(id = ns('sameParams'),
                                h2(style = 'text-align:center',
                                   'Select two different parameter/poc combinations to display scatterplot.')))
}

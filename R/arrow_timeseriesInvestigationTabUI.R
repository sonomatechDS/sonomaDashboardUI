#' Timeseries Investigation Tab UI
#'
#' Used alongside timeseriesInvestigationTabServer() to add a collection of
#' reactive elements to the app, including multiple parameter selections,
#' a timeseries plot, a scatter plot, and a pollution rose. Wrapper for standalone
#' functions scatterServer(), timeseriesServer() and pollutionRoseServer().
#'
#' @param id string ID the connects timeseriesInvestigationTabUI() to timeseriesInvestigationTabServer()
#'
#' @return
#' @importFrom shiny NS fluidRow column tagList selectInput radioButtons conditionalPanel textOutput
#' @importFrom htmltools div
#' @importFrom shinyjs useShinyjs
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
arrow_timeseriesInvestigationTabUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(offset = 1,
        2,
        htmltools::div(
          style = 'padding-top: 25px;',
          shiny::selectInput(
            inputId = ns('TSPrimaryDur'),
            label = "Select sample duration",
            choices = "",
            width = '95%'
          )
         )
        ),
    shiny::column(
      2,
      htmltools::div(
        style = 'padding-top: 25px;',
        shiny::selectInput(
          inputId = ns('TSPrimaryParam'),
          label = "Select parameter",
          choices = "",
          width = '95%'
        )
      )
    ),
    shiny::column(
      1,
      htmltools::div(
        style = 'padding-top: 25px;',
        shiny::selectInput(
          inputId = ns('TSPrimaryPOC'),
          label = "Select a POC",
          choices = "",
          width = '95%'
        )
      )
    ),
    shiny::column(6,
                  shiny::fluidRow(
                    shiny::column(
                         3,
                       htmltools::div(
                         style = 'padding-top: 25px;',
                         shiny::radioButtons(
                           inputId = ns("TSSecondaryDur"),
                           label = "Select a second parameter",
                           selected = "None",
                           choices = 'None'
                         )
                       )
                    ),
             shiny::conditionalPanel(condition = "input.TSSecondaryDur != 'None'",
                              ns = ns,
                              shiny::column(
                                4,
                                htmltools::div(
                                  style = 'padding-top: 25px;',
                                  shiny::selectInput(
                                    inputId = ns('TSSecondaryParam'),
                                    label = 'Second parameter',
                                    choices = '',
                                    width = '95%'
                                  )
                                )
                              ),
                              shiny::column(
                                2,
                                htmltools::div(
                                  style = 'padding-top: 25px;',
                                  shiny::selectInput(
                                    inputId = ns('TSSecondaryPOC'),
                                    label = "Second POC",
                                    choices = '',
                                    width = '95%'
                                  )
                                )
                              )),
             shiny::column(1,
                           htmltools::div(
                             style = 'padding-top:55px;',
                             shiny::actionButton(ns('GoButton'),
                                                 'Update Plots',
                                                 style =
                                                   "color: white; background-color: #0072B2FF; border-color: #0072B2FF")))
           ))),
    timeseriesUI(ns('TS')),
    shiny::fluidRow(
      shiny::column(5, offset = 1,
                    htmltools::div(id = ns('pripollrose'),
             pollutionRoseUI(ns('TSPollRose1'))),
             shiny::conditionalPanel(
               "input.TSPrimaryDur.indexOf('1 HOUR') > -1",
               ns = ns,
               htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                              "Note: Displayed 1-hr wind directions are resultant values.")),
             shiny::conditionalPanel(
               "input.TSPrimaryDur.indexOf('24 HOUR') > -1",
               ns = ns,
               htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                              "Note: Displayed 24-hr wind directions are vector averages of 1-hr resultant winds.")),
             shiny::conditionalPanel(
               "input.TSPrimaryDur != '1 HOUR' && input.TSPrimaryDur != '24 HOUR' && input.TSPrimaryDur != 'None'",
               ns = ns,
               htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                              "Wind data available only for 1-Hr and 24-Hr data."))
             ),
      shiny::column(5,
                      htmltools::div(id = ns('secpollrose'),
                        pollutionRoseUI(ns('TSPollRose2'))),
                   shiny::conditionalPanel(
                     "input.TSSecondaryDur.indexOf('24 HOUR') > -1",
                     ns = ns,
                     htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                     "Note: Displayed 24-hr wind directions are vector averages of 1-hr resultant winds.")),
                   shiny::conditionalPanel(
                     "input.TSSecondaryDur != '1 HOUR' && input.TSSecondaryDur != '24 HOUR' && input.TSSecondaryDur != 'None'",
                     ns = ns,
                     htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                                    "Wind data available only for 1-Hr and 24-Hr data."))
                   ),

    ),
    shiny::fluidRow(
      column(10, offset = 1, align = 'center',
             htmltools::div(style = 'border-sytle: solid; border-color: black; padding:10px; background-color: #E8E8E8; font-size: 18px; margin-top: 10px;',
               shiny::htmlOutput(ns('windAvailabilityNotice'))
               )
             ),
      ),
    shiny::conditionalPanel(condition = "input.TSSecondaryDur == input.TSPrimaryDur",
                     ns = ns,
                     htmltools::div(style='padding-top:30px',
                       shiny::column(6, offset = 3,
                                     scatterUI(ns('TSTabScatter'))))),
    shiny::conditionalPanel(condition = "input.TSSecondaryDur != input.TSPrimaryDur",
                     ns = ns,
                     shiny::fluidRow(shiny::column(
                       10,
                       offset = 1,
                       htmltools::div(
                         "Selected parameters must have same sample duration to display scatter plot.",
                         style = paste0(
                           'padding-bottom: 25px;',
                           ' padding-top: 35px;',
                           ' padding-left:70px;',
                           ' text-align:center;',
                           ' font-size:24px;'
                         )
                       )
                     )))
  )

}

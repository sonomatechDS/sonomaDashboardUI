#' Timeseries Investigation Tab UI
#'
#' Used alongside timeseriesInvestigationTabServer() to add a collection of
#' reactive elements to the app, including multiple parameter selections,
#' a timeseries plot, a scatter plot, and a pollution rose. Wrapper for standalone
#' functions scatterServer(), timeseriesServer() and pollutionRoseServer().
#'
#' @param id string ID the connects timeseriesInvestigationTabUI() to timeseriesInvestigationTabServer()
#' @param duration_options vector The durations that should be avilable for the second
#'                          parameter. A subset of '1Hr', '24Hr' and '8Hr'. Default
#'                          is c('1Hr', '24Hr').
#'
#' @return
#' @importFrom shiny NS fluidRow column tagList selectInput radioButtons conditionalPanel
#' @importFrom htmltools div
#' @importFrom shinyjs useShinyjs
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
timeseriesInvestigationTabUI <- function(id,
                                         duration_options = c('1 HOUR' ,'24 HOUR')
                                         ) {

  tmp_v <- setNames(duration_options, paste0('Show ', stringr::str_extract(duration_options, '\\d+'), '-hr Parameters'))
  sec_param_duration <- split(unname(tmp_v), names(tmp_v))
  sec_param_duration[["None"]] <- 'None'

  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(shiny::column(
      2,
      offset = 1,
      htmltools::div(
        style = 'padding-top: 25px;',
        shiny::selectInput(
          inputId = ns('TSPrimaryParam'),
          label = "Select an hourly parameter",
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
                           inputId = ns("TSSecondaryParamRadio"),
                           label = "Select a second parameter",
                           selected = "None",
                           choices = sec_param_duration
                         )
                       )
                    ),
             shiny::conditionalPanel(condition = "input.TSSecondaryParamRadio != 'None'",
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
             pollutionRoseUI(ns('TSPollRose1')),
             htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:60px',
                            "Note: Displayed 1-hr wind directions are resultant values.")),
      shiny::column(5,
                      # shiny::conditionalPanel(
                      # "input.TSSecondaryParamRadio == '1 HOUR' || input.TSSecondaryParamRadio == '24 HOUR'",
                      # ns = ns,
                      # htmltools::div(
                      #   pollutionRoseUI(ns('TSPollRose2')))),

                      htmltools::div(id = ns('secpollrose'),
                        pollutionRoseUI(ns('TSPollRose2'))),
                   shiny::conditionalPanel(
                     "input.TSSecondaryParamRadio.indexOf('24 HOUR') > -1",
                     ns = ns,
                     htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                     "Note: Displayed 24-hr wind directions are vector averages of 1-hr resultant winds.")),
                   shiny::conditionalPanel(
                     "input.TSSecondaryParamRadio != '1 HOUR' && input.TSSecondaryParamRadio != '24 HOUR' && input.TSSecondaryParamRadio != 'None'",
                     ns = ns,
                     htmltools::div(style = 'font-style: italic; font-size:16px; padding-top:10px; padding-left:80px',
                                    "Pollution rose available only for 1-Hr and 24-Hr data."))
                   ),

    ),
    shiny::conditionalPanel(condition = "input.TSSecondaryParamRadio == '1 HOUR'",
                     ns = ns,
                     htmltools::div(style='padding-top:30px',
                       shiny::column(6, offset = 3,
                                     scatterUI(ns('TSTabScatter'))))),
    shiny::conditionalPanel(condition = "input.TSSecondaryParamRadio != '1 HOUR'",
                     ns = ns,
                     shiny::fluidRow(shiny::column(
                       10,
                       offset = 1,
                       htmltools::div(
                         "Select two 1-hour parameters to display scatter plot.",
                         style = paste0(
                           'padding-bottom: 25px;',
                           ' padding-top: 35px;',
                           ' padding-left:70px;',
                           ' font-size:16px;'
                         )
                       )
                     )))
  )

}

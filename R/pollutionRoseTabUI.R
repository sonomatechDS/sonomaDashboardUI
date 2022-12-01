#' Pollution Rose Tab UI
#'
#' Used alongside pollutionRoseTabUI() to add a collection of reactive
#' elements to an app, including multiselects for seasons and years, a select
#' for metric type (aqi or concentration), and up to 2 pollution roses for
#' two specified parameters.
#'
#' @param id string ID used to link pollutionRoseTabUI() to pollutionRoseTabServer()
#' @param season_filter boolean If TRUE, multi-select input for season is added.
#'                              Seasons include spring, summer, fall, winter.
#' @param year_filter boolean If TRUE, multi-select input for year is added.
#'                              Years range from 2019-2020.
#' @param dow_filter boolean If TRUE, multi-select input for day of week is added.
#' @param hour_filter boolean If TRUE, multi-select input for hour is added.
#' @param metric_filter boolean (optional) If TRUE (default) filter
#'                               is added on tab to display either
#'                               concentrations or AQI index.
#' @param num_plots integer Number of plots to show on the PR tab. Maximum of 4.
#'
#' @return
#' @importFrom shiny NS fluidRow column selectInput actionButton plotOutput tagList tagAppendChild
#' @importFrom shinyWidgets pickerInput
#' @importFrom htmltools div
#' @importFrom shinycssloaders withSpinner
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
pollutionRoseTabUI <- function(id,
                              season_filter = TRUE,
                              year_filter = TRUE,
                              dow_filter = FALSE,
                              hour_filter = FALSE,
                              metric_filter = TRUE,
                              num_plots = 2
                              ) {

    assertthat::assert_that(num_plots <= 4, msg= 'Number of plots allowed.')

    ns <- shiny::NS(id)

    input_width = '250px'

    season_menu <- shiny::fluidRow(shiny::column(3, offset=1,
        shinyWidgets::pickerInput(
          inputId = ns('Seasons'),
          label = 'Select Season(s)',
          choices = c('Winter',
                      'Spring',
                      'Summer',
                      'Fall'),
          selected = c('Winter',
                       'Spring',
                       'Summer',
                       'Fall'),
          choicesOpt = list(subtext = c('(DJF)',
                                        '(MAM)',
                                        '(JJA)',
                                        '(SON)')),
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          width = input_width
      )
    ))

    year_menu <- shiny::fluidRow(shiny::column(3, offset=1,
                                     shinyWidgets::pickerInput(
                                       inputId = ns('Years'),
                                       label = 'Select Year(s)',
                                       choices = c(''),
                                       selected = c(''),
                                       options = list(`actions-box` = TRUE),
                                       multiple = TRUE,
                                       width=input_width
                                     )
    ))

    dow_menu <- shiny::fluidRow(shiny::column(3, offset=1,
                                               shinyWidgets::pickerInput(
                                                 inputId = ns('dow'),
                                                 label = 'Select Day(s) of the Week',
                                                 choices = c("Sun","Mon","Tue",
                                                             "Wed","Thu","Fri",
                                                             "Sat"),
                                                 selected = c("Sun","Mon","Tue",
                                                              "Wed","Thu","Fri",
                                                              "Sat"),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE,
                                                 width=input_width
                                               )
    ))

    hour_menu <- shiny::fluidRow(shiny::column(3, offset=1,
                                               shinyWidgets::pickerInput(
                                                 inputId = ns('hour'),
                                                 label = 'Select Hour(s)',
                                                 choices = c('00:00', '01:00', '02:00', '03:00', '04:00',
                                                             '05:00', '06:00', '07:00', '08:00', '09:00',
                                                             '10:00', '11:00', '12:00', '13:00', '14:00',
                                                             '15:00', '16:00', '17:00', '18:00', '19:00',
                                                             '20:00', '21:00', '22:00', '23:00'),
                                                 selected = c('00:00', '01:00', '02:00', '03:00', '04:00',
                                                              '05:00', '06:00', '07:00', '08:00', '09:00',
                                                              '10:00', '11:00', '12:00', '13:00', '14:00',
                                                              '15:00', '16:00', '17:00', '18:00', '19:00',
                                                              '20:00', '21:00', '22:00', '23:00'),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE,
                                                 width=input_width
                                               )
    ))

    conc_aqi_button <- shiny::fluidRow(shiny::column(3, offset = 1,
                                                 shiny::selectInput(ns('ConcentrationAQI'),
                                                             label='Select Metric',
                                                             choices=c('Concentrations'='Concentrations',
                                                                       'Air Quality Index'='AQI'),
                                                             selected = 'Concentrations',
                                                             width = input_width)
                                ))
    action_button <- shiny::column(3,
                     htmltools::div(
                     style = 'padding-top:26px;',
                     shiny::actionButton(ns('GoButton'),
                                         'Update Plots',
                     style =
                       "color: white; background-color: #0072B2FF; border-color: #0072B2FF")))

    if (num_plots == 2) {
      pr_plots <- shiny::fluidRow(
        shiny::column(5, offset = 1,
                      shiny::plotOutput(ns("PollutionRose1"),
                                        height = "700px") %>%
                        shinycssloaders::withSpinner()
        ),
        shiny::column(5,
                      shiny::plotOutput(ns("PollutionRose2"),
                                        height = "700px") %>%
                        shinycssloaders::withSpinner()
        )
      )
    } else if (num_plots == 1) {
      pr_plots <- shiny::fluidRow(
        shiny::column(5, offset = 1,
                      shiny::plotOutput(ns("PollutionRose1"),
                                        height = "700px") %>%
                        shinycssloaders::withSpinner()
        )
      )
    } else if (num_plots == 3) {
      pr_plots <- shiny::tagList(
        shiny::fluidRow(
          shiny::column(5, offset = 1,
                        shiny::plotOutput(ns("PollutionRose1"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          ),
          shiny::column(5,
                        shiny::plotOutput(ns("PollutionRose2"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          )
      ),
        shiny::fluidRow(
          shiny::column(5, offset = 1,
                        shiny::plotOutput(ns("PollutionRose3"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          )
        )
      )
    } else if (num_plots == 4) {
      pr_plots <- shiny::tagList(
        shiny::fluidRow(
          shiny::column(5, offset = 1,
                        shiny::plotOutput(ns("PollutionRose1"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          ),
          shiny::column(5,
                        shiny::plotOutput(ns("PollutionRose2"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          )
        ),
        shiny::fluidRow(
          shiny::column(5, offset = 1,
                        shiny::plotOutput(ns("PollutionRose3"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          ),
          shiny::column(5,
                        shiny::plotOutput(ns("PollutionRose4"),
                                          height = "700px") %>%
                          shinycssloaders::withSpinner()
          )
        )
      )
    }


    tl <- shiny::tagList()
    if (year_filter) {
      tl <- shiny::tagAppendChild(tl, year_menu)
    }
    if (season_filter) {
      tl <- shiny::tagAppendChild(tl, season_menu)
    }
    if (dow_filter) {
      tl <- shiny::tagAppendChild(tl, dow_menu)
    }
    if (hour_filter) {
      tl <- shiny::tagAppendChild(tl, hour_menu)
    }
    if (metric_filter) {
      tl <- shiny::tagAppendChild(tl, conc_aqi_button)
    }

    n <- sum(year_filter, season_filter, metric_filter, dow_filter, hour_filter)
    if (n > 0) {
      tl$children[[n]] <- shiny::tagAppendChild(tl$children[[n]], action_button)
    }

    htmltools::div(style='padding-top:15px',
    shiny::tagAppendChildren(tl,
    pr_plots,
    list(
      htmltools::div(shiny::fluidRow(
        column(10, offset = 1, align = 'center',
               htmltools::div(style = 'border-sytle: solid; border-color: black; padding:10px; background-color: #E8E8E8; font-size: 18px; margin-top: 10px;',
                              shiny::htmlOutput(ns('windAvailabilityNotice'))
               )
        ),
      )),
      htmltools::div(style = 'padding-top: 20px; padding-bottom:30px;',
    sitemapUI(ns('PRSitemap'))))
    ))
    # )
  }

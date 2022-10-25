#' Pollution rose tab UI
#'
#' Used alongside pollutionRoseTabUI() to add a collection of reactive
#' elements to an app, including multiselects for seasons and years, a select
#' for metric type (aqi or concentration), and up to 2 pollution roses for
#' two specified parameters.
#'
#' @param id string ID used to link pollutionRoseTabUI() to pollutionRoseTabServer()
#' @param input_data reactive data.frame Dataframe with concentration data for
#'                   all input parameters as well as a column with wind
#'                   direction, and a column with wind speed (speed can be
#'                   all NAs if plotting pollution rose)
#' @param map_data reactive data.frame Dataframe with lat and lng columns
#'                                     with the location of program sites,
#'                                     to be plotted on a leaflet map.
#' @param wd_col string Column name for column with wind direction values
#' @param ws_col string Column name for column with wind speed values
#' @param conc_col_1 string Column name for column with concentration values
#'                          for title_1
#' @param conc_col_2 string Column name for column with concentration values
#'                          for title_2
#' @param conc_col_3 string Column name for column with concentration values
#'                          for title_3
#' @param conc_col_4 string Column name for column with concentration values
#'                          for title_4
#' @param site_rct reactive string AQS sitecode for selected site
#' @param site_col string Column name for column with AQS sitecode values
#' @param unit_str_1 string Unit of title_1. Can be an expression.
#' @param unit_str_2 (optional) string Unit of optional title_2. Can be an expression.
#' @param unit_str_3 (optional) string Unit of optional title_3. Can be an expression.
#' @param unit_str_4 (optional) string Unit of optional title_4. Can be an expression.
#' @param title_1 reactive string Name of first parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param title_2 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param title_3 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param title_4 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param year_filter boolean If true, multi-select year input included and
#'                            plots may be filtered by year.
#' @param year_col string (optional) Column name of column with year values if year_filter specified.
#' @param season_filter boolean If true, multi-select season input included and
#'                            plots may be filtered by season.
#' @param season_col string (optional) Column name of column with season values if season_filter specified.
#' @param dow_filter boolean If true, multi-select day of week input included and
#'                            plots may be filtered by day of week.
#' @param dow_col string (optional) Column name of column with day of week values if season_filter specified.
#' @param hour_filter boolean If true, multi-select hour input included and
#'                            plots may be filtered by hour
#' @param hour_col string (optional) Column name of column with hour values if season_filter specified.
#' @param breaks_conc_1 vector (optional) Manual scale breaks in legend for first
#'                                 parameter. If not specified, sequence of
#'                                 6 equal breaks will be created with endpoints
#'                                 at the max and min concentration value.
#' @param breaks_aqi_1 vector Manual scale breaks for AQI designations for first
#'                            parameter.
#' @param breaks_conc_2 (optional) Manual scale breaks in legend for optional second
#'                                 parameter. If not specified, sequence of
#'                                 6 equal breaks will be created with endpoints
#'                                 at the max and min concentration value.
#' @param breaks_aqi_2 (optional) vector Manual scale breaks for AQI designations for
#'                                optional second parameter.
#' @param breaks_conc_3 vector (optional) Manual scale breaks in legend for first
#'                                 parameter. If not specified, sequence of
#'                                 6 equal breaks will be created with endpoints
#'                                 at the max and min concentration value.
#' @param breaks_aqi_3 vector Manual scale breaks for AQI designations for first
#'                            parameter.
#' @param breaks_conc_4 (optional) Manual scale breaks in legend for optional second
#'                                 parameter. If not specified, sequence of
#'                                 6 equal breaks will be created with endpoints
#'                                 at the max and min concentration value.
#' @param breaks_aqi_4 (optional) vector Manual scale breaks for AQI designations for
#'                                optional second parameter.
#' @param aqipalette vector Standard color palette for aqi categories with elements:
#'                   - aqipalette$greenGood,
#'                   - aqipalette$yellowModerate,
#'                   - aqipalette$orangeUSG,
#'                   - aqipalette$redUnhealthy,
#'                   - aqipalette$purpleVeryUnhealthy,
#'                   - aqipalette$maroonHazardous
#'
#' @return
#' @importFrom shiny moduleServer reactive renderPlot reactiveValues observeEvent req validate need
#' @importFrom dplyr filter sym pull
#' @importFrom magrittr `%>%`
#' @importFrom assertthat assert_that
#' @importFrom viridis magma
#' @importFrom openair pollutionRose
#' @importFrom grid unit
#' @importFrom shinyWidgets updatePickerInput
#' @export
#'
#' @examples
pollutionRoseTabServer <- function(id,
                                input_data,
                                map_data,
                                wd_col,
                                ws_col,
                                conc_col_1,
                                conc_col_2,
                                conc_col_3,
                                conc_col_4,
                                site_rct,
                                site_col,
                                unit_str_1 = NULL,
                                unit_str_2 = NULL,
                                unit_str_3 = NULL,
                                unit_str_4 = NULL,
                                title_1 = shiny::reactive({'PM[2.5]'}),
                                title_2 = shiny::reactive({'O[3]'}),
                                title_3 = shiny::reactive({'Fill title_3!'}),
                                title_4 = shiny::reactive({'Fill title_4!'}),
                                year_filter = TRUE,
                                year_col = NULL,
                                season_filter = TRUE,
                                season_col = NULL,
                                dow_filter = FALSE,
                                dow_col = NULL,
                                hour_filter = FALSE,
                                hour_col = NULL,
                                breaks_conc_1 = NULL,
                                breaks_aqi_1,
                                breaks_conc_2 = NULL,
                                breaks_aqi_2,
                                breaks_conc_3 = NULL,
                                breaks_aqi_3 = NULL,
                                breaks_conc_4 = NULL,
                                breaks_aqi_4 = NULL,
                                aqipalette) {
  shiny::moduleServer(id, {
    function(input, output, session) {
      filters <- shiny::reactiveValues(years = "",
                     seasons = c('Winter',
                                 'Spring',
                                 'Summer',
                                 'Fall'),
                     aqi_conc = 'Concentrations',
                     hours = c('00:00', '01:00', '02:00', '03:00', '04:00',
                               '05:00', '06:00', '07:00', '08:00', '09:00',
                               '10:00', '11:00', '12:00', '13:00', '14:00',
                               '15:00', '16:00', '17:00', '18:00', '19:00',
                               '20:00', '21:00', '22:00', '23:00'),
                     dow = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

      observe({
          shiny::req(site_rct(), input_data(), year_col)
          year_choices <- input_data() %>%
            dplyr::filter(!!sym(site_col) == site_rct()) %>%
            dplyr::pull(!!sym(year_col)) %>%
            unique()

          shinyWidgets::updatePickerInput(session,
                                          inputId = "Years",
                                          choices = year_choices,
                                          selected = year_choices
                                          )
          filters$years <- year_choices
      })

      shiny::observeEvent(input$GoButton,{
        if (!is.null(input$Years)) {
          filters$years <- input$Years
        }
        if (!is.null(input$Seasons)) {
          filters$seasons <- input$Seasons
        }
        if (!is.null(input$ConcentrationAQI)) {
          filters$aqi_conc <- input$ConcentrationAQI
        }
        if (!is.null(input$dow)) {
          filters$dow <- input$dow
        }
        if (!is.null(input$hour)) {
          filters$hours <- input$hour
        }
      })

      data <- shiny::reactive({
        shiny::req(site_rct())

        plot_data <- input_data() %>%
          dplyr::filter(!!sym(site_col) == site_rct())

        if (season_filter) {
          assertthat::assert_that(!is.null(season_col),
                                  msg = paste('`season_filter` specified without',
                                              '`season_col`.'))
          plot_data <- plot_data %>%
            dplyr::filter(!!dplyr::sym(season_col) %in% filters$seasons)

        }
        if (year_filter) {
          assertthat::assert_that(!is.null(year_col),
                                  msg = paste('`year_filter` specified without',
                                              '`year_col`.'))
          plot_data <- plot_data %>%
            dplyr::filter(!!sym(year_col) %in% filters$years)

        }
        if (dow_filter) {

          assertthat::assert_that(!is.null(dow_col),
                                  msg = paste('`dow_filter` specified without',
                                              '`dow_col`.'))
          plot_data <- plot_data %>%
            dplyr::filter(!!sym(dow_col) %in% filters$dow)

        }
        if (hour_filter) {

          assertthat::assert_that(!is.null(hour_col),
                                  msg = paste('`hour_filter` specified without',
                                              '`hour_col`.'))
          plot_data <- plot_data %>%
            dplyr::filter(!!sym(hour_col) %in% filters$hours)

        }

        return(plot_data)
      })

      output$PollutionRose1 <- shiny::renderPlot({
        shiny::req(is.data.frame(data()))
        shiny::validate(
          shiny::need(nrow(data() %>%
                             filter(!is.na(!!dplyr::sym(wd_col)))) > 0,
                      message = paste('Insuffcient wind data at selected site to',
                                      'display pollution rose.')))

        shiny::validate(
          shiny::need(nrow(data() %>%
                             filter(!is.na(!!dplyr::sym(conc_col_1)))) > 0,
                      message = paste('Insuffcient parameter data at selected site to',
                                      'display pollution rose.')))

        if (is.null(breaks_conc_1)) {
          minRose <- min(data()[conc_col_1], na.rm = T)
          maxRose <- max(data()[conc_col_1], na.rm = T)
          breaks_conc_1 <- seq(from = minRose, to = maxRose, length.out = 6)
        }

        if(filters$aqi_conc == 'Concentrations'){
          breaks_1 <- breaks_conc_1
          cols <- viridis::magma(length(breaks_1))
        } else if (filters$aqi_conc == 'AQI'){
          assertthat::assert_that(!is.null(breaks_aqi_1),
                                  msg = 'If AQI metric chose, `breaks_aqi_n` must be specified.')
          breaks_1 <- breaks_aqi_1
          cols <- c(aqipalette$greenGood,
                    aqipalette$yellowModerate,
                    aqipalette$orangeUSG,
                    aqipalette$redUnhealthy,
                    aqipalette$purpleVeryUnhealthy,
                    aqipalette$maroonHazardous)
        }


        # if (is.null(title_1)) {
        #   title_1 <- param_rct_1()
        #   if (!is.null(filters$years)) {
        #     title_1 <- paste(title_1)
        #                      # "~~~~ '",
        #                      # paste(filters$seasons, collapse = ', '),
        #                      # "\n",
        #                      # paste(filters$years, collapse = ', '),
        #                      # "'")
        #   }
        # }

        openair::pollutionRose(mydata = data(),
                               wd = wd_col,
                               ws = ws_col,
                               paddle = FALSE,
                               pollutant = conc_col_1,
                               breaks = breaks_1,
                               cols = cols,
                               offset = 5,
                               par.settings=list(fontsize=list(text=16),
                                                 par.main.text = list(just='left',
                                                                      x = grid::unit(5, "mm"))),
                               key.footer = parse(text = paste(unit_str_1)),
                               main = parse(text = title_1()))
      })

      output$PollutionRose2 <- shiny::renderPlot({
        if(is.null(conc_col_2)) {
          NULL
        } else {
          shiny::req(is.data.frame(data()),
                     filters$aqi_conc,
                     filters$years,
                     filters$seasons)

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(wd_col)))) > 0,
                        message = paste('Insuffcient wind data at selected site to',
                                        'display pollution rose.')))

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(conc_col_2)))) > 0,
                        message = paste('Insuffcient parameter data at selected site to',
                                        'display pollution rose.')))

          if (is.null(breaks_conc_2)) {
            minRose <- min(data()[conc_col_2], na.rm = T)
            maxRose <- max(data()[conc_col_2], na.rm = T)
            breaks_conc_2 <- seq(from = minRose, to = maxRose, length.out = 6)
          }

          if(filters$aqi_conc == 'Concentrations'){
            breaks_2 <- breaks_conc_2
            cols <- viridis::magma(length(breaks_2))
          } else if (filters$aqi_conc == 'AQI'){
            assertthat::assert_that(!is.null(breaks_aqi_2),
                                    msg = 'If AQI metric chose, `breaks_aqi_n` must be specified.')
            breaks_2 <- breaks_aqi_2
            cols <- c(aqipalette$greenGood,
                      aqipalette$yellowModerate,
                      aqipalette$orangeUSG,
                      aqipalette$redUnhealthy,
                      aqipalette$purpleVeryUnhealthy,
                      aqipalette$maroonHazardous)
          }


          # if (is.null(title_2)) {
          #   title_2 <- param_rct_2()
          #   if (!is.null(filters$years)) {
          #     title_2 <- paste(title_2)
          #
          #   }
          # }

          openair::pollutionRose(mydata = data(),
                                 wd = wd_col,
                                 ws = ws_col,
                                 paddle = FALSE,
                                 pollutant = conc_col_2,
                                 breaks = breaks_2,
                                 cols = cols,
                                 offset = 5,
                                 par.settings=list(fontsize=list(text=16),
                                                   par.main.text = list(just='left',
                                                                        x = grid::unit(5, "mm"))),
                                 key.footer = parse(text = paste(unit_str_2)),
                                 main = parse(text = title_2()))

        }

      })

      output$PollutionRose3 <- shiny::renderPlot({
        if(is.null(conc_col_3)) {
          NULL
        } else {
          shiny::req(is.data.frame(data()),
                     filters$aqi_conc,
                     filters$years,
                     filters$seasons)

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(wd_col)))) > 0,
                        message = paste('Insuffcient wind data at selected site to',
                                        'display pollution rose.')))

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(conc_col_3)))) > 0,
                        message = paste('Insuffcient parameter data at selected site to',
                                        'display pollution rose.')))

          if (is.null(breaks_conc_3)) {
            minRose <- min(data()[conc_col_3], na.rm = T)
            maxRose <- max(data()[conc_col_3], na.rm = T)
            breaks_conc_3 <- seq(from = minRose, to = maxRose, length.out = 6)
          }

          if(filters$aqi_conc == 'Concentrations'){
            breaks_3 <- breaks_conc_3
            cols <- viridis::magma(length(breaks_3))
          } else if (filters$aqi_conc == 'AQI'){
            assertthat::assert_that(!is.null(breaks_aqi_3),
                                    msg = 'If AQI metric chose, `breaks_aqi_n` must be specified.')
            breaks_3 <- breaks_aqi_3
            cols <- c(aqipalette$greenGood,
                      aqipalette$yellowModerate,
                      aqipalette$orangeUSG,
                      aqipalette$redUnhealthy,
                      aqipalette$purpleVeryUnhealthy,
                      aqipalette$maroonHazardous)
          }


          # if (is.null(title_3)) {
          #   title_3 <- param_rct_3()
          #   if (!is.null(filters$years)) {
          #     title_3 <- paste(title_3)
          #
          #   }
          # }

          openair::pollutionRose(mydata = data(),
                                 wd = wd_col,
                                 ws = ws_col,
                                 paddle = FALSE,
                                 pollutant = conc_col_3,
                                 breaks = breaks_3,
                                 cols = cols,
                                 offset = 5,
                                 par.settings=list(fontsize=list(text=16),
                                                   par.main.text = list(just='left',
                                                                        x = grid::unit(5, "mm"))),
                                 key.footer = parse(text = paste(unit_str_3)),
                                 main = parse(text = title_3()))

        }

      })

      output$PollutionRose4 <- shiny::renderPlot({
        if(is.null(conc_col_4)) {
          NULL
        } else {
          shiny::req(is.data.frame(data()),
                     filters$aqi_conc,
                     filters$years,
                     filters$seasons)

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(wd_col)))) > 0,
                        message = paste('Insuffcient wind data at selected site to',
                                        'display pollution rose.')))

          shiny::validate(
            shiny::need(nrow(data() %>%
                               filter(!is.na(!!dplyr::sym(conc_col_4)))) > 0,
                        message = paste('Insuffcient parameter data at selected site to',
                                        'display pollution rose.')))

          if (is.null(breaks_conc_4)) {
            minRose <- min(data()[conc_col_4], na.rm = T)
            maxRose <- max(data()[conc_col_4], na.rm = T)
            breaks_conc_4 <- seq(from = minRose, to = maxRose, length.out = 6)
          }

          if(filters$aqi_conc == 'Concentrations'){
            breaks_4 <- breaks_conc_4
            cols <- viridis::magma(length(breaks_4))
          } else if (filters$aqi_conc == 'AQI'){
            assertthat::assert_that(!is.null(breaks_aqi_4),
                                    msg = 'If AQI metric chose, `breaks_aqi_n` must be specified.')
            breaks_4 <- breaks_aqi_4
            cols <- c(aqipalette$greenGood,
                      aqipalette$yellowModerate,
                      aqipalette$orangeUSG,
                      aqipalette$redUnhealthy,
                      aqipalette$purpleVeryUnhealthy,
                      aqipalette$maroonHazardous)
          }


          # if (is.null(title_4)) {
          #   title_4 <- param_rct_4()
          #   if (!is.null(filters$years)) {
          #     title_4 <- paste(title_4)
          #
          #   }
          # }

          openair::pollutionRose(mydata = data(),
                                 wd = wd_col,
                                 ws = ws_col,
                                 paddle = FALSE,
                                 pollutant = conc_col_4,
                                 breaks = breaks_4,
                                 cols = cols,
                                 offset = 5,
                                 par.settings=list(fontsize=list(text=16),
                                                   par.main.text = list(just='left',
                                                                        x = grid::unit(5, "mm"))),
                                 key.footer = parse(text = paste(unit_str_4)),
                                 main = parse(text = title_4()))

        }

      })


      site_loc <- shiny::reactive({
        d <- map_data() %>%
          dplyr::filter(!!dplyr::sym(site_col) == site_rct())
        return(d)
      })

      sitemapServer(id='PRSitemap',
                    data=shiny::reactive({site_loc()}),
                    label_col = site_col)

    }
  })
}

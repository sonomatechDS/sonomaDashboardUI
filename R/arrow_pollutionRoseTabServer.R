#' Pollution rose tab Server
#'
#' Used alongside pollutionRoseTabUI() to add a collection of reactive
#' elements to an app, including multiselects for seasons and years, a select
#' for metric type (aqi or concentration), and up to 4 pollution roses for
#' four specified parameters.
#'
#' @param id string ID used to link pollutionRoseTabUI() to pollutionRoseTabServer()
#' @param ds_uri string uri to Arrow dataset, formatted as .parquet
#' @param datetime_col string Name of column with datetime values in posixct format
#' @param site_year dataframe Dataframe with distinct combinations of AQS sitecode and year
#' @param map_data reactive data.frame Dataframe with lat and lng columns
#'                                     with the location of program sites,
#'                                     to be plotted on a leaflet map.
#' @param wd_param_name string Parameter name for wind direction values
#' @param param1 string Name of first pollutant parameter
#' @param param_col string Name of column with parameter name values
#' @param sample_measurement_col string Name of column with concentration values
#' @param sample_dur_col string Name of column with sample duration values
#' @param site_rct reactive string AQS sitecode for selected site
#' @param site_col string Column name for column with AQS sitecode values
#' @param sample_dur string Sample duration of data -- defaults to '1 HOUR'.
#' @param unit_str_1 string Unit of title_1. Can be an expression.
#' @param title_1 reactive string Name of first parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param year_filter boolean If true, multi-select year input included and
#'                            plots may be filtered by year.
#' @param year_col string (optional) Column name of column with year values if year_filter specified.
#' @param season_filter boolean If true, multi-select season input included and
#'                            plots may be filtered by season.
#' @param dow_filter boolean If true, multi-select day of week input included and
#'                            plots may be filtered by day of week.
#' @param hour_filter boolean If true, multi-select hour input included and
#'                            plots may be filtered by hour
#' @param unit_str_2 (optional) string Unit of optional title_2. Can be an expression.
#' @param unit_str_3 (optional) string Unit of optional title_3. Can be an expression.
#' @param unit_str_4 (optional) string Unit of optional title_4. Can be an expression.
#' @param title_2 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param title_3 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param title_4 reactive string (optional) Name of optional second parameter to include in plot title
#'                             (can be an expression if subscripts needed).
#' @param breaks_conc_1 vector (optional) Manual scale breaks in legend for first
#'                                 parameter. If not specified, sequence of
#'                                 6 equal breaks will be created with endpoints
#'                                 at the max and min concentration value.
#' @param breaks_aqi_1 vector Manual scale breaks for AQI designations for first
#'                            parameter.
#' @param param2 string Name of optional 2nd pollutant parameter
#' @param param3 string Name of optional 3rd pollutant parameter
#' @param param4 string Name of optional 4th pollutant parameter
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
#' @param timeout_sec numeric (Optional) default is 30, the number of
#'                    seconds passed to request_timeout and connect_timeout when creating an arrow s3_bucket object
#'
#' @return
#' @importFrom shiny moduleServer reactive renderPlot reactiveValues observeEvent req validate need renderText
#' @importFrom dplyr filter sym pull collect rename
#' @importFrom magrittr `%>%`
#' @importFrom assertthat assert_that
#' @importFrom viridis magma
#' @importFrom openair pollutionRose
#' @importFrom grid unit
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom lubridate month year wday hour
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom arrow open_dataset s3_bucket
#' @export
#'
#' @examples
arrow_pollutionRoseTabServer <- function(id,
                                ds_uri,
                                datetime_col,
                                site_year,
                                map_data,
                                wd_param_name,
                                param1,
                                param_col,
                                sample_measurement_col,
                                sample_dur_col,
                                site_rct,
                                site_col,
                                sample_dur = '1 HOUR',
                                breaks_aqi_1,
                                param2=NULL,
                                param3=NULL,
                                param4=NULL,
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
                                dow_filter = FALSE,
                                hour_filter = FALSE,
                                breaks_conc_1 = NULL,
                                breaks_conc_2 = NULL,
                                breaks_aqi_2 = NULL,
                                breaks_conc_3 = NULL,
                                breaks_aqi_3 = NULL,
                                breaks_conc_4 = NULL,
                                breaks_aqi_4 = NULL,
                                aqipalette,
                                timeout_sec = 30) {
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
          shiny::req(site_rct())
          year_choices <- site_year %>%
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

      site_data <- shiny::reactive({
        shiny::req(site_rct())
        shinybusy::show_modal_spinner(text = 'Querying Database', spin = 'fading-circle', color = '#0C53AF')

        buk <- arrow::s3_bucket(ds_uri, connect_timeout = timeout_sec,
                                request_timeout = timeout_sec)
        ds <- arrow::open_dataset(buk, format = 'parquet')

        s <- as.integer(site_rct())

        # Start with available wind data for site
        data <- ds %>%
          filter(!!sym(site_col) == s,
                 !!sym(sample_dur_col) == sample_dur,
                 !!sym(param_col) == wd_param_name) %>%
          rename(!!dplyr::sym(wd_param_name) := !!dplyr::sym(sample_measurement_col)) %>%
          select(all_of(c(site_col, wd_param_name, datetime_col, year_col)))

        # Join each called parameter as a new column
        p_list <- c(param1, param2, param3, param4)

        for (p in p_list[!is.null(p_list)]) {
          i <- ds %>%
            dplyr::filter(!!sym(site_col) == s,
                          !!sym(sample_dur_col) == sample_dur,
                          !!sym(param_col) == p) %>%
            rename(!!dplyr::sym(p) := !!dplyr::sym(sample_measurement_col)) %>%
            select(all_of(c(site_col, p, datetime_col, year_col)))

          data <- data %>%
            left_join(i, by = c(site_col, datetime_col, year_col))
        }
        collected <- data %>% dplyr::collect()


        if (season_filter) {
          collected <- collected %>%
            mutate(month = lubridate::month(!!dplyr::sym(datetime_col))) %>%
            mutate(season = case_when(
              month < 3 ~ 'Winter',
              month < 6 ~ 'Spring',
              month < 9 ~ 'Summer',
              month < 12 ~ 'Fall',
              month == 12 ~ 'Winter',
              TRUE ~ 'Missing'
            ))
        }
        if (dow_filter) {
          collected <- collected %>%
            mutate(dow = lubridate::wday(!!dplyr::sym(datetime_col)))
        }
        if (hour_filter) {
          collected <- collected %>%
            mutate(hr = lubridate::hour(!!dplyr::sym(datetime_col)))
        }

        collected$WS <- 10

        remove_modal_spinner()

        return(collected)
      })

      # filter site data by dropdown menu inputs
      filt_data <- shiny::reactive({
        req(site_data())

        plot_data <- site_data()

        if (season_filter) {
          plot_data <- plot_data %>%
            dplyr::filter(season %in% filters$seasons)
        }
        if (year_filter) {
          assertthat::assert_that(!is.null(year_col),
                                  msg = paste('`year_filter` specified without',
                                              '`year_col`.'))
          plot_data <- plot_data %>%
            dplyr::filter(!!sym(year_col) %in% filters$years)
        }
        if (dow_filter) {
          plot_data <- plot_data %>%
            dplyr::filter(dow %in% filters$dow)
        }
        if (hour_filter) {

          plot_data <- plot_data %>%
            dplyr::filter(hr %in% filters$hours)
        }

        return(plot_data)
      })

      output$windAvailabilityNotice <- renderText({
        req(site_data())

        wdat <- site_data() %>%
          select(!!sym(wd_param_name), !!sym(datetime_col)) %>%
          filter(!is.na(!!sym(wd_param_name)))

        max_d <- format(max(wdat[[datetime_col]], na.rm=T), '%Y-%m-%d')
        min_d <- format(min(wdat[[datetime_col]], na.rm=T), '%Y-%m-%d')
        wa_text <- paste('Wind data is available from', min_d,'through', max_d)

        return(shiny::HTML(wa_text))
      })

      output$PollutionRose1 <- shiny::renderPlot({
        shiny::req(is.data.frame(filt_data()))

        plot_data <- filt_data() %>% drop_na(!!sym(param1))

        shiny::validate(
          shiny::need(nrow(plot_data) > 0,
                      message = paste('Insuffcient parameter data at selected site to',
                                      'display pollution rose.')))

        shiny::validate(
          shiny::need(nrow(plot_data %>%
                             filter(!is.na(!!dplyr::sym(wd_param_name)))) > 0,
                      message = paste('Insuffcient wind data at selected site to',
                                      'display pollution rose.')))

        if (is.null(breaks_conc_1)) {
          minRose <- min(filt_data()[param1], na.rm = T)
          maxRose <- max(filt_data()[param1], na.rm = T)
          breaks_conc_1 <- seq(from = minRose, to = maxRose, length.out = 6)
        }

        if(filters$aqi_conc == 'Concentrations'){
          breaks_1 <- breaks_conc_1
          cols <- viridis::magma(length(breaks_1))
        } else if (filters$aqi_conc == 'AQI'){
          assertthat::assert_that(!is.null(breaks_aqi_1),
                                  msg = 'If AQI metric chosen, `breaks_aqi_n` must be specified.')
          breaks_1 <- breaks_aqi_1
          cols <- c(aqipalette$greenGood,
                    aqipalette$yellowModerate,
                    aqipalette$orangeUSG,
                    aqipalette$redUnhealthy,
                    aqipalette$purpleVeryUnhealthy,
                    aqipalette$maroonHazardous)
        }

        # browser()
        openair::pollutionRose(mydata = filt_data(),
                               wd = wd_param_name,
                               ws = 'WS',
                               paddle = FALSE,
                               pollutant = param1,
                               breaks = breaks_1,
                               cols = cols,
                               offset = 5,
                               par.settings=list(fontsize=list(text=16),
                                                 par.main.text = list(just='left',
                                                                      x = grid::unit(5, "mm"))),
                               key.footer = parse(text = paste(unit_str_1)),
                               main = parse(text = title_1())
                               )
      })

      output$PollutionRose2 <- shiny::renderPlot({
        shiny::req(is.data.frame(filt_data()))

        plot_data <- filt_data() %>% drop_na(!!sym(param2))

        shiny::validate(
          shiny::need(nrow(plot_data) > 0,
                      message = paste('Insuffcient parameter data at selected site to',
                                      'display pollution rose.')))

        shiny::validate(
          shiny::need(nrow(plot_data %>%
                             filter(!is.na(!!dplyr::sym(wd_param_name)))) > 0,
                      message = paste('Insuffcient wind data at selected site to',
                                      'display pollution rose.')))

        if (is.null(breaks_conc_2)) {
          minRose <- min(filt_data()[param2], na.rm = T)
          maxRose <- max(filt_data()[param2], na.rm = T)
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


        openair::pollutionRose(mydata = filt_data(),
                               wd = wd_param_name,
                               ws = 'WS',
                               paddle = FALSE,
                               pollutant = param2,
                               breaks = breaks_2,
                               cols = cols,
                               offset = 5,
                               par.settings=list(fontsize=list(text=16),
                                                 par.main.text = list(just='left',
                                                                      x = grid::unit(5, "mm"))),
                               key.footer = parse(text = paste(unit_str_2)),
                               main = parse(text = title_2()))
      })

      output$PollutionRose3 <- shiny::renderPlot({
        shiny::req(is.data.frame(filt_data()))

        plot_data <- filt_data() %>% drop_na(!!sym(param3))

        shiny::validate(
          shiny::need(nrow(plot_data) > 0,
                      message = paste('Insuffcient parameter data at selected site to',
                                      'display pollution rose.')))

        shiny::validate(
          shiny::need(nrow(plot_data %>%
                             filter(!is.na(!!dplyr::sym(wd_param_name)))) > 0,
                      message = paste('Insuffcient wind data at selected site to',
                                      'display pollution rose.')))

        if (is.null(breaks_conc_3)) {
          minRose <- min(filt_data()[param3], na.rm = T)
          maxRose <- max(filt_data()[param3], na.rm = T)
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


        openair::pollutionRose(mydata = filt_data(),
                               wd = wd_param_name,
                               ws = 'WS',
                               paddle = FALSE,
                               pollutant = param3,
                               breaks = breaks_3,
                               cols = cols,
                               offset = 5,
                               par.settings=list(fontsize=list(text=16),
                                                 par.main.text = list(just='left',
                                                                      x = grid::unit(5, "mm"))),
                               key.footer = parse(text = paste(unit_str_3)),
                               main = parse(text = title_3()))
      })

      output$PollutionRose4 <- shiny::renderPlot({
        shiny::req(is.data.frame(filt_data()))

        plot_data <- filt_data() %>% drop_na(!!sym(param4))

        shiny::validate(
          shiny::need(nrow(plot_data) > 0,
                      message = paste('Insuffcient parameter data at selected site to',
                                      'display pollution rose.')))

        shiny::validate(
          shiny::need(nrow(plot_data %>%
                             filter(!is.na(!!dplyr::sym(wd_param_name)))) > 0,
                      message = paste('Insuffcient wind data at selected site to',
                                      'display pollution rose.')))

        if (is.null(breaks_conc_4)) {
          minRose <- min(filt_data()[param4], na.rm = T)
          maxRose <- max(filt_data()[param4], na.rm = T)
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


        openair::pollutionRose(mydata = filt_data(),
                               wd = wd_param_name,
                               ws = 'WS',
                               paddle = FALSE,
                               pollutant = param4,
                               breaks = breaks_4,
                               cols = cols,
                               offset = 5,
                               par.settings=list(fontsize=list(text=16),
                                                 par.main.text = list(just='left',
                                                                      x = grid::unit(5, "mm"))),
                               key.footer = parse(text = paste(unit_str_4)),
                               main = parse(text = title_4()))
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

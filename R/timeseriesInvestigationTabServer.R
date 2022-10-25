#' Timeseries Investigation Tab Server
#'
#' Used alongside timeseriesInvestigationTabUI() to add a collection of
#' reactive elements to the app, including multiple parameter selections,
#' a timeseries plot, a scatter plot, and a pollution rose. Wrapper for standalone
#' functions scatterServer(), timeseriesServer() and pollutionRoseServer().
#'
#' @param id string ID the connects timeseriesInvestigationTabUI() to timeseriesInvestigationTabServer()
#' @param data_1hr_rct reactive data.frame 1-hr concentration data with columns
#'                                         for datetime, parameter name, units,
#'                                         concentration measurements, sitecode.
#' @param data_2_rct reactive data.frame Concentration data for non-1hr data with columns
#'                                         for datetime, parameter name, units,
#'                                         concentration measurements, sample duration,
#'                                         sitecode.
#' @param data_1hr_wind_rct reactive data.frame 1hr Wind data with columns
#'                                         for datetime, sitecode, wind direction,
#'                                         wind speed.
#' @param data_24hr_wind_rct reactive data.frame 24hr Wind data with columns
#'                                         for datetime, sitecode, wind direction,
#'                                         wind speed.
#' @param selected_site_rct reactive string Selected AQS Sitecode
#' @param paramlist_1hr_rct reactive vector List of 1hr parameters for selected site
#' @param paramlist_2_rct reactive vector List of non- 1-hr parameters for selected site.
#'                             Must have column for sample_duration.
#' @param dt_col string Name of datetime column in concentration data
#' @param param_col string Name of column with parameter name values in concentration AND wind data
#' @param unit_col string Name of column with unit values in concentration data
#' @param measurement_col string Name of concentration column for timeseries
#' @param sampledur_col string Name of sample duration column in data_2_rct
#' @param poc_col string Column name of column with poc number.
#' @param wd_col string Name of column in wind data with direction values
#' @param ws_col string Name of column in wind data with speed values
#' @param site_col string Name of column in wind AND concentration data with values
#'                        of AQS sitecode.
#' @param linesize numeric (optional) Size of plotted line (default = 0.5) on TS
#' @param pointsize numeric (optional) Size of plotted points (default = 3) on TS
#'
#' @return
#' @importFrom shiny moduleServer observeEvent updateSelectInput reactive req updateRadioButtons
#' @importFrom dplyr filter pull sym
#' @importFrom tidyr unite
#' @importFrom magrittr `%>%`
#' @importFrom shinyjs showElement hideElement
#' @export
#'
#' @examples
timeseriesInvestigationTabServer <- function(id,
                                             data_1hr_rct,
                                             data_2_rct,
                                             data_1hr_wind_rct,
                                             data_24hr_wind_rct,
                                             selected_site_rct,
                                             paramlist_1hr_rct,
                                             paramlist_2_rct,
                                             dt_col,
                                             param_col,
                                             unit_col,
                                             measurement_col,
                                             sampledur_col,
                                             poc_col,
                                             wd_col,
                                             ws_col,
                                             site_col,
                                             linesize = .75,
                                             pointsize=2
                                             ) {
  shiny::moduleServer(id, {
    function(input, output, session) {


      selections <- reactiveValues(primary = NULL,
                                   p_poc = NULL,
                                   secondary = NULL,
                                   s_poc = NULL,
                                   sec_dur = 'None',
                                   duration_list = NULL)

      shiny::observeEvent(selected_site_rct(),{
        shiny::req(selected_site_rct())

        p_params <- paramlist_1hr_rct() %>%
          dplyr::filter(!!dplyr::sym(site_col) == selected_site_rct())

        shiny::updateSelectInput(session, "TSPrimaryParam",
                          choices = p_params[[param_col]],
                          selected = p_params[[param_col]][1])

        p_pocs <- data_1hr_rct() %>%
          dplyr::filter(!!dplyr::sym(param_col) == p_params[[param_col]][1],
                 !!dplyr::sym(site_col) == selected_site_rct()) %>%
          dplyr::pull(poc_col) %>%
          sort() %>%
          unique()

        durs <- data_2_rct() %>%
          dplyr::filter(!!dplyr::sym(site_col) == selected_site_rct()) %>%
          dplyr::pull(!!dplyr::sym(sampledur_col)) %>%
          unique()

        durs <- unique(c('1 HOUR', durs))

        tmp_v <- setNames(durs, paste0('Show ', stringr::str_extract(durs, '\\d+'), '-hr Parameters'))
        sec_dur_opts <- split(unname(tmp_v), names(tmp_v))
        sec_dur_opts[["None"]] <- 'None'

        shiny::updateSelectInput(session, "TSPrimaryPOC",
                                 choices = p_pocs,
                                 selected = min(p_pocs))

        shiny::updateRadioButtons(session, 'TSSecondaryParamRadio',
                                  choices= sec_dur_opts,
                                  selected = 'None')

        selections$primary <- p_params[[param_col]][1]
        selections$sec_dur <- 'None'
        selections$p_poc <- min(p_pocs)
      })


      shiny::observe({
        req(input$TSPrimaryParam, selected_site_rct())
        p_pocs <- data_1hr_rct() %>%
          dplyr::filter(!!dplyr::sym(param_col) == input$TSPrimaryParam,
                 !!dplyr::sym(site_col) == selected_site_rct()) %>%
          dplyr::pull(poc_col) %>%
          sort() %>%
          unique()

        shiny::updateSelectInput(session, "TSPrimaryPOC",
                                 choices = p_pocs,
                                 selected = min(p_pocs))
      })


      shiny::observe({
        req(input$TSSecondaryParam, selected_site_rct())

        radio <- input$TSSecondaryParamRadio
        if(radio != '1 HOUR') {

          s_pocs <- data_2_rct() %>%
            dplyr::filter(!!dplyr::sym(sampledur_col) %in% input$TSSecondaryParamRadio,
                   !!dplyr::sym(param_col) == input$TSSecondaryParam,
                   !!dplyr::sym(site_col) == selected_site_rct()) %>%
            dplyr::pull(poc_col) %>%
            sort() %>%
            unique()
          shiny::updateSelectInput(session, "TSSecondaryPOC",
                                   choices = s_pocs,
                                   selected = min(s_pocs))
        } else if (radio == '1 HOUR') {

          s_pocs <- data_1hr_rct() %>%
            dplyr::filter(!!dplyr::sym(param_col) == input$TSSecondaryParam,
                   !!dplyr::sym(site_col) == selected_site_rct()) %>%
            dplyr::pull(poc_col) %>%
            sort() %>%
            unique()
          shiny::updateSelectInput(session, "TSSecondaryPOC",
                                   choices = s_pocs,
                                   selected = min(s_pocs))
        }
      })

      # Update "Select a second parameter" list on TS tab based on selected duration
      shiny::observeEvent(c(input$TSSecondaryParamRadio, selected_site_rct()), {
        shiny::req(selected_site_rct())
        radio <- input$TSSecondaryParamRadio
        if(radio != '1 HOUR' & radio != 'None') {

          plist_filt <- paramlist_2_rct() %>%
            filter(!!dplyr::sym(sampledur_col) == radio,
                   !!dplyr::sym(site_col) == selected_site_rct()) %>%
            arrange(!!dplyr::sym(param_col))

          shiny::updateSelectInput(session, "TSSecondaryParam",
                            choices = plist_filt[[param_col]],
                            selected = plist_filt[[param_col]][1])

          s_pocs <- data_2_rct() %>%
            dplyr::filter(!!dplyr::sym(param_col) == plist_filt[[param_col]][1],
                   !!dplyr::sym(site_col) == selected_site_rct()) %>%
            dplyr::pull(poc_col) %>%
            sort() %>%
            unique()
          shiny::updateSelectInput(session, "TSSecondaryPOC",
                                   choices = s_pocs,
                                   selected = min(s_pocs))
        } else if (radio == '1 HOUR') {
          plist_filt <- paramlist_1hr_rct() %>%
            filter(!!dplyr::sym(site_col) == selected_site_rct()) %>%
            arrange(!!dplyr::sym(param_col))

          shiny::updateSelectInput(session, "TSSecondaryParam",
                            choices = plist_filt[[param_col]],
                            selected = plist_filt[[param_col]][3])
          s_pocs <- data_1hr_rct() %>%
            dplyr::filter(!!dplyr::sym(param_col) == plist_filt[[param_col]][3],
                   !!dplyr::sym(site_col) == selected_site_rct()) %>%
            dplyr::pull(poc_col) %>%
            sort() %>%
            unique()
          shiny::updateSelectInput(session, "TSSecondaryPOC",
                                   choices = s_pocs,
                                   selected = min(s_pocs))
        } else {
          shiny::updateSelectInput(session, "TSSecondaryParam",
                            choices = '')
          shiny::updateSelectInput(session, "TSSecondaryPOC",
                                   choices = '')
        }
      })

      shiny::observeEvent(input$GoButton,{

        selections$primary <- input$TSPrimaryParam
        selections$p_poc <- input$TSPrimaryPOC
        selections$secondary <- input$TSSecondaryParam
        selections$sec_dur <- input$TSSecondaryParamRadio
        selections$s_poc <- input$TSSecondaryPOC
      })

      sec_param_rct <- shiny::reactive({
        if (selections$sec_dur != 'None') {
          return(selections$secondary)}
        else {
          return(NULL)}
      })

      filter_param1_data <- shiny::reactive({
        req(selected_site_rct(), selections$primary, selections$p_poc)

        f1 <- selections$primary
        f2 <- selected_site_rct()
        f3 <- selections$p_poc

          filter_param1_data <- data_1hr_rct() %>%
            dplyr::filter(!!dplyr::sym(param_col) == f1,
                          !!dplyr::sym(site_col) == f2,
                          !!dplyr::sym(poc_col) == f3)

        # }

          filter_param1_data <- filter_param1_data %>%
            tidyr::unite('param_poc',dplyr::sym(!!param_col),
                                           dplyr::sym(!!poc_col),
                         sep = ' - ',
                         remove = F)

        return(filter_param1_data)
      })

      # Filter 24hr data based on Time Series selections
      filter_param2_data <- reactive({

        shiny::req(selected_site_rct(), selections$secondary, selections$s_poc)
        if (selections$sec_dur != '1 HOUR') {
          filter_param2_data <- data_2_rct() %>%
            dplyr::filter(!!dplyr::sym(sampledur_col) %in% selections$sec_dur,
                          !!dplyr::sym(param_col) %in% c(selections$secondary),
                          !!dplyr::sym(site_col) == selected_site_rct(),
                          !!dplyr::sym(poc_col) == selections$s_poc)
        } else {
          filter_param2_data <- data_1hr_rct() %>%
            dplyr::filter(!!dplyr::sym(param_col)%in% c(selections$secondary),
                          !!dplyr::sym(site_col) == selected_site_rct(),
                          !!dplyr::sym(poc_col) == selections$s_poc)
        }

        filter_param2_data <- filter_param2_data %>%
          tidyr::unite('param_poc',dplyr::sym(!!param_col),
                       dplyr::sym(!!poc_col),
                       sep = ' - ',
                        remove = F)

        return(filter_param2_data)
      })

      # Assign wind data depending on secondary duration
      wind_2_data <- shiny::reactive({
        shiny::req(selected_site_rct())
        if (unique(stringr::str_extract(selections$sec_dur, '\\d+')) == '24') {
          wind_2_data <- data_24hr_wind_rct()
        } else if (selections$sec_dur == '1 HOUR') {
          wind_2_data <- data_1hr_wind_rct()
        } else {
          wind_2_data <- NULL
        }
        return(wind_2_data)
      })

      parampoc_display <- shiny::reactive({
        if (selections$sec_dur == 'None') {
          val <- NULL
        } else {
          val <- paste(selections$secondary, '-', selections$s_poc)
        }
        return(val)
      })


      TS_brush <- timeseriesServer(id='TS',
                                   data_param1_rct = shiny::reactive({filter_param1_data()}),
                                   data_param2_rct = shiny::reactive({filter_param2_data()}),
                                   x=dt_col,
                                   y=measurement_col,
                                   param_col = param_col,
                                   unit_col = unit_col,
                                   param_rct = shiny::reactive({paste(selections$primary, '-', selections$p_poc)}),
                                   sec_param_rct = shiny::reactive({parampoc_display()}),
                                   sec_duration_rct = shiny::reactive({selections$sec_dur}),
                                   linesize=linesize,
                                   pointsize=pointsize)

      scatterServer(id = 'TSTabScatter',
                    data_param1_rct = shiny::reactive({filter_param1_data()}),
                    data_param2_rct = shiny::reactive({filter_param2_data()}),
                    param_col='param_poc',
                    measurement_col=measurement_col,
                    key_col = dt_col,
                    unit_col = unit_col,
                    poc_col = poc_col,
                    param_rct = shiny::reactive({paste(selections$primary, '-', selections$p_poc)}),
                    sec_param_rct = shiny::reactive({parampoc_display()}),
                    brush = shiny::reactive({TS_brush$brush}))


      pollutionRoseServer(
        id='TSPollRose1',
        data_rct = shiny::reactive({filter_param1_data()}),
        data_wind_rct = shiny::reactive({data_1hr_wind_rct()}),
        wd_col = wd_col,
        ws_col = ws_col,
        param_col = 'param_poc',
        measurement_col = measurement_col,
        key_col = dt_col,
        unit_col = unit_col,
        site_col = site_col,
        param_rct = shiny::reactive({paste(selections$primary, '-', selections$p_poc)}),
        brush = shiny::reactive({TS_brush$brush}),
        title = NULL
      )


      observe({
        if(selections$sec_dur %in% c('1 HOUR', '24 HOUR')) {
          shinyjs::showElement('secpollrose')
        } else {
          shinyjs::hideElement('secpollrose')
        }
      })

      pollutionRoseServer(
        id='TSPollRose2',
        data_rct = shiny::reactive({filter_param2_data()}),
        data_wind_rct = shiny::reactive({wind_2_data()}),
        wd_col = wd_col,
        ws_col = ws_col,
        param_col = 'param_poc',
        measurement_col = measurement_col,
        key_col = dt_col,
        unit_col = unit_col,
        site_col = site_col,
        param_rct = shiny::reactive({parampoc_display()}),
        brush = shiny::reactive({TS_brush$brush}),
        title = NULL
      )



    }
  })
}


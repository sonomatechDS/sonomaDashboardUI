#' Timeseries Investigation Tab Server -- Arrow
#'
#' Used alongside timeseriesInvestigationTabUI() to add a collection of
#' reactive elements to the app, including multiple parameter selections,
#' a timeseries plot, a scatter plot, and a pollution rose. Wrapper for standalone
#' functions scatterServer(), timeseriesServer() and pollutionRoseServer(). 
#' Data input is an arrow dataset.
#'
#' @param id string ID the connects timeseriesInvestigationTabUI() to timeseriesInvestigationTabServer()
#' @param selected_site_rct reactive string Selected AQS Sitecode
#' @param ds dataset Arrow dataset object, created with arrow::open_dataset()
#' @param site_dur_param_poc dataframe dataframe with unique combinations of 
#'                                     aqs_sitecode, sample_duration, parameter,
#'                                     poc (with these column names)
#' @param dt_col string Name of column with datetime in posixct format
#' @param param_col string Name of column with parameter name
#' @param unit_col string name of column with units
#' @param measurement_col string Name of column with concentration measurements
#' @param sampledur_col string Name of column with sample duration values
#' @param poc_col string Name of column with POC values
#' @param wd_param_name string Parameter name for wind direction values
#' @param site_col string Name of column with aqs sitecodes
#' @param linesize numeric (Optional) size of line on timeseries, default = 0.75.
#' @param pointsize numeric (Optional) size of point on timeseries, default = 3.
#'
#' @return
#' @importFrom shiny moduleServer observeEvent updateSelectInput reactive req updateRadioButtons HTML renderText
#' @importFrom dplyr filter pull sym collect rename
#' @importFrom tidyr unite
#' @importFrom magrittr `%>%`
#' @importFrom shinyjs showElement hideElement
#' @export
#'
#' @examples
arrow_timeseriesInvestigationTabServer <- function(id,
                                             ds,
                                             selected_site_rct,
                                             site_dur_param_poc,
                                             dt_col,
                                             param_col,
                                             unit_col,
                                             measurement_col,
                                             sampledur_col,
                                             poc_col,
                                             wd_param_name,
                                             site_col,
                                             linesize = .75,
                                             pointsize=2
                                             ) {
  shiny::moduleServer(id, {
    function(input, output, session) {

      selections <- reactiveValues(primary = NULL,
                                   p_poc = NULL,
                                   p_dur = NULL,
                                   secondary = NULL,
                                   s_poc = NULL,
                                   sec_dur = 'None',
                                   duration_list = NULL)
      
      shiny::observeEvent(selected_site_rct(),{
        shiny::req(selected_site_rct())
        # Determine available sample durations and set choices for primary duration menu
        # Default to '1 HOUR; -- should be first in sort()
        durs <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(site_col) == selected_site_rct()) %>%
          pull(sampledur_col) %>%
          unique() %>%
          sort()
        
        shiny::updateSelectInput(session, "TSPrimaryDur",
                                choices = durs,
                                selected = durs[1])
        
        # Determine available parameters and set choices for primary parameter menu
        p_params <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(site_col) == selected_site_rct(),
                        !!dplyr::sym(sampledur_col) == durs[1]) %>%
          pull(param_col) %>%
          unique()

        shiny::updateSelectInput(session, "TSPrimaryParam",
                          choices = p_params,
                          selected = p_params[1])
        
        # Determine available pocs and set choices for primary poc menu
        p_pocs <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(param_col) == p_params[1],
                 !!dplyr::sym(site_col) == selected_site_rct(),
                 !!dplyr::sym(sampledur_col) == durs[1]) %>%
          dplyr::pull(poc_col) %>%
          sort() %>%
          unique()
        
        shiny::updateSelectInput(session, "TSPrimaryPOC",
                                 choices = p_pocs,
                                 selected = min(p_pocs))
        
        # Set up radio buttons for choice of 2nd parameter
        tmp_v <- setNames(durs, paste0('Show ', stringr::str_extract(durs, '\\d+'), '-hr Parameters'))
        sec_dur_opts <- split(unname(tmp_v), names(tmp_v))
        sec_dur_opts[["None"]] <- 'None'

        shiny::updateRadioButtons(session, 'TSSecondaryDur',
                                  choices= sec_dur_opts,
                                  selected = 'None')

        selections$p_dur <- durs[1]
        selections$primary <- p_params[1]
        selections$sec_dur <- 'None'
        selections$p_poc <- min(p_pocs)
      })

      
      # Update parameter choices if primary duration selection changes
      shiny::observe({
        req(selected_site_rct(), input$TSPrimaryDur)
        p_params <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(site_col) == selected_site_rct(),
                        !!dplyr::sym(sampledur_col) == input$TSPrimaryDur) %>%
          pull(param_col) %>%
          unique()
        
        shiny::updateSelectInput(session, "TSPrimaryParam",
                                 choices = p_params,
                                 selected = p_params[1])
      })
      
      # Update poc choices if primary duration or parameter selection changes
      shiny::observe({
        req(selected_site_rct(), input$TSPrimaryParam, input$TSPrimaryDur)
        p_pocs <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(param_col) == input$TSPrimaryParam,
                 !!dplyr::sym(site_col) == selected_site_rct(),
                 !!dplyr::sym(sampledur_col) == input$TSPrimaryDur) %>%
          dplyr::pull(poc_col) %>%
          sort() %>%
          unique()

        shiny::updateSelectInput(session, "TSPrimaryPOC",
                                 choices = p_pocs,
                                 selected = min(p_pocs))
      })

      # Update options for secondary parameter/poc if radio button != None
      # Update "Select a second parameter" list on TS tab based on selected duration
      shiny::observeEvent(c(input$TSSecondaryDur, selected_site_rct()), {
        shiny::req(selected_site_rct())
      if (input$TSSecondaryDur != 'None') {
        plist_filt <- site_dur_param_poc %>%
          filter(!!dplyr::sym(sampledur_col) == input$TSSecondaryDur,
                 !!dplyr::sym(site_col) == selected_site_rct()) %>%
          arrange(!!dplyr::sym(param_col)) %>%
          pull(param_col) %>%
          unique()
          
          shiny::updateSelectInput(session, "TSSecondaryParam",
                                   choices = plist_filt,
                                   selected = plist_filt[1])
          
          s_pocs <- site_dur_param_poc %>%
            filter(!!dplyr::sym(sampledur_col) == input$TSSecondaryDur,
                   !!dplyr::sym(site_col) == selected_site_rct(),
                   !!dplyr::sym(param_col) == plist_filt[1]) %>%
            pull(poc_col) %>%
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
      
      shiny::observe({
        req(input$TSSecondaryDur, selected_site_rct())
        
        s_params <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(sampledur_col) %in% input$TSSecondaryDur,
                        !!dplyr::sym(site_col) == selected_site_rct()) %>%
          dplyr::pull(param_col) %>%
          sort() %>%
          unique()
        shiny::updateSelectInput(session, "TSSecondaryParam",
                                 choices = s_params,
                                 selected = s_params[1])
      })
            
      shiny::observe({
        req(input$TSSecondaryParam, selected_site_rct())

        s_pocs <- site_dur_param_poc %>%
          dplyr::filter(!!dplyr::sym(sampledur_col) %in% input$TSSecondaryDur,
                 !!dplyr::sym(param_col) == input$TSSecondaryParam,
                 !!dplyr::sym(site_col) == selected_site_rct()) %>%
          dplyr::pull(poc_col) %>%
          sort() %>%
          unique()
        shiny::updateSelectInput(session, "TSSecondaryPOC",
                                 choices = s_pocs,
                                 selected = min(s_pocs))
      })


      shiny::observeEvent(input$GoButton,{

        selections$primary <- input$TSPrimaryParam
        selections$p_poc <- input$TSPrimaryPOC
        selections$p_dur <- input$TSPrimaryDur
        selections$secondary <- input$TSSecondaryParam
        selections$sec_dur <- input$TSSecondaryDur
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

        f1 <- selections$p_dur
        f2 <- selections$primary
        f3 <- as.integer(selected_site_rct())
        f4 <- as.integer(selections$p_poc)
        
        filter_param1_data <- ds %>%
          dplyr::filter(!!dplyr::sym(sampledur_col) == f1,
                        !!dplyr::sym(param_col) == f2,
                        !!dplyr::sym(site_col) == f3,
                        !!dplyr::sym(poc_col) == f4) %>%
          dplyr::collect()

        filter_param1_data <- filter_param1_data %>%
          tidyr::unite('param_poc',dplyr::sym(!!param_col),
                                         dplyr::sym(!!poc_col),
                       sep = ' - ',
                       remove = F)

        return(filter_param1_data)
      })

      # Filter 24hr data based on Time Series selections
      filter_param2_data <- reactive({

        shiny::req(selected_site_rct(), selections$secondary, selections$sec_dur, selections$s_poc)
        
        f1 <- selections$sec_dur
        f2 <- selections$secondary
        f3 <- as.integer(selected_site_rct())
        f4 <- as.integer(selections$s_poc)

        filter_param2_data <- ds %>%
          dplyr::filter(!!dplyr::sym(sampledur_col) == f1,
                        !!dplyr::sym(param_col) == f2,
                        !!dplyr::sym(site_col) == f3,
                        !!dplyr::sym(poc_col) == f4) %>%
          dplyr::collect()
        
        filter_param2_data <- filter_param2_data %>%
          tidyr::unite('param_poc',dplyr::sym(!!param_col),
                       dplyr::sym(!!poc_col),
                       sep = ' - ',
                       remove = F)
        return(filter_param2_data)
      })
      
      # Assign wind data depending on primary duration
      wind_1_data <- shiny::reactive({
        shiny::req(selected_site_rct())
        if (selections$p_dur %in% c('1 HOUR', '24 HOUR')) {
          f1 <- as.integer(selected_site_rct())
          f2 <- selections$p_dur
          f3 <- wd_param_name

          wind_1_data <- ds %>%
            filter(!!dplyr::sym(site_col) == f1,
                   !!sym(sampledur_col) ==f2,
                   !!sym(param_col) == f3) %>%
            select(!!sym(site_col), !!sym(measurement_col), !!dplyr::sym(dt_col)) %>%
            collect() %>%
            rename(!!sym(wd_param_name) := measurement_col)
        } else {
          wind_1_data <- NULL
        }
        return(wind_1_data)
      })

      # Assign wind data depending on secondary duration
      wind_2_data <- shiny::reactive({
        shiny::req(selected_site_rct(),
                   sec_param_rct())
        if (selections$sec_dur %in% c('1 HOUR', '24 HOUR')) {
          f1 <- as.integer(selected_site_rct())
          f2 <- selections$sec_dur
          f3 <- wd_param_name
          
          wind_2_data <- ds %>%
            filter(!!sym(site_col) == f1,
                   !!sym(sampledur_col) ==f2,
                   !!sym(param_col) == f3) %>%
            select(!!sym(site_col), !!sym(measurement_col), !!dplyr::sym(dt_col)) %>%
            collect() %>%
            rename(!!sym(wd_param_name) := measurement_col)
        } else {
          wind_2_data <- NULL
        }
        return(wind_2_data)
      })
      
      output$windAvailabilityNotice <- renderText({
        req(selected_site_rct(), wind_1_data())
        max_d <- format(max(wind_1_data()[[dt_col]], na.rm=T), '%Y-%m-%d')
        min_d <- format(min(wind_1_data()[[dt_col]], na.rm=T), '%Y-%m-%d')
        wa_text <- paste(selections$p_dur, 'wind data is available from', min_d,'through', max_d)
        
        if (selections$sec_dur %in% c('1 HOUR', '24 HOUR')) {
          req(wind_2_data())
          if (selections$sec_dur != selections$p_dur) {
            max_sd <- format(max(wind_2_data()[[dt_col]], na.rm=T), '%Y-%m-%d')
            min_sd <- format(min(wind_2_data()[[dt_col]], na.rm=T), '%Y-%m-%d')
            wa_text2 <- paste(selections$sec_dur, 'wind data is available from', min_sd,'through', max_sd)
            wa_text <- paste0(wa_text, '<br>', wa_text2)
          }
        }
        return(shiny::HTML(wa_text))
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

      observe({
        req(selected_site_rct(), selections$p_dur)
        if(selections$p_dur %in% c('1 HOUR', '24 HOUR')) {
          shinyjs::showElement('pripollrose')
        } else {
          shinyjs::hideElement('pripollrose')
        }
      })

      arrow_pollutionRoseServer(
        id='TSPollRose1',
        data_rct = shiny::reactive({filter_param1_data()}),
        data_wind_rct = shiny::reactive({wind_1_data()}),
        wd_col = wd_param_name,
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
        req(selected_site_rct(), selections$sec_dur)
        if(selections$sec_dur %in% c('1 HOUR', '24 HOUR')) {
          shinyjs::showElement('secpollrose')
        } else {
          shinyjs::hideElement('secpollrose')
        }
      })

      arrow_pollutionRoseServer(
        id='TSPollRose2',
        data_rct = shiny::reactive({filter_param2_data()}),
        data_wind_rct = shiny::reactive({wind_2_data()}),
        wd_col = wd_param_name,
        param_col = 'param_poc',
        measurement_col = measurement_col,
        key_col = dt_col,
        unit_col = unit_col,
        site_col = site_col,
        param_rct = shiny::reactive({parampoc_display()}),
        brush = shiny::reactive({TS_brush$brush}),
        title = NULL
      )

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



    }
  })
}


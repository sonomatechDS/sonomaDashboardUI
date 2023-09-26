#' Pollution Rose Server
#'
# Used alongside pollutionRoseTab() to add a pollution rose with 1hr data to the app.
# Takes in two different dataframes, one with 1hr concentration data and
# one with wind data. Input concentration data should be in "long-form"
# with a column for parameter, concentration, unit, site, and a "key" (typically
# a timestamp) used to spread the data by parameter. Accepts an existing brush to
# adjust plot boundaries based on selection from another plot.
#'
#' @param id string ID used to link pollutionRoseUI() to pollutionRoseServer()
#' @param data_rct reactive Concentration data. Should have
#'                              columns for site, parameter, unit
#'                              and a "key" used in joining wind to concentration
#'                              data (typically a timestamp).
#' @param data_wind_rct reactive Wind data. Should have column for
#'                               direction and speed, and the same
#'                               key_col and site_col as data_rct. Speed can be
#'                               filled with NA if unavailable.
#' @param wd_col string Name of column in wind data with direction values.
#' @param ws_col string Name of column in wind data with speed values.
#' @param param_col string Name of column in 1-hr concentration data with
#'                         parameter name values.
#' @param param_rct reactive string Parameter to plot on windrose. Used to
#'                                  filter 1-hr concentration data via the
#'                                  param_col.
#' @param measurement_col string Name of column in 1-hr concentration data with
#'                         parameter name values.
#' @param key_col string Name of column in 1-hr concentration and wind data with
#'                         key values (typically datetime column).
#' @param unit_col string Name of column in 1-hr concentration data with
#'                         unit values.
#' @param site_col string Name of column in 1-hr concentration data with
#'                         sitecode values.
#' @param brush (optional) reactive brush Brush object with elements for x and y that
#'                                        are each vectors with x/y min and x/y max values
#'                                        respectively (e.g. list(x=c(0,10), y=c(-10,10)))
#' @param title (optional) string Title for plot. If not specified, title will be current value of param_rct.
#'
#' @return
#' @importFrom shiny moduleServer reactive renderPlot reactiveValues observeEvent req observe validate need
#' @importFrom dplyr filter select sym left_join
#' @importFrom magrittr `%>%`
#' @importFrom assertthat assert_that
#' @importFrom viridis magma
#' @importFrom openair windRose
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
pollutionRoseServer <- function(id,
                                data_rct,
                                data_wind_rct,
                                wd_col,
                                ws_col,
                                param_col,
                                measurement_col,
                                key_col,
                                unit_col,
                                site_col,
                                param_rct,
                                brush = reactive({NULL}),
                                title = NULL) {

  shiny::moduleServer(id, {
    function(input, output, session) {
      data <- shiny::reactive({
        shiny::req(is.data.frame(data_rct()),
                   is.character(param_rct()))
        data <- data_rct() %>%
          dplyr::select(param_col,
                       measurement_col,
                       key_col,
                       site_col,
                        unit_col) %>%
          dplyr::filter(!!dplyr::sym(param_col) == param_rct()) %>%
          dplyr::left_join(data_wind_rct()[c(key_col, site_col, wd_col, ws_col)],
                           by = c(key_col, site_col))
        return(data)
      })

      brush_range <-
        shiny::reactiveValues(x = NULL, y = NULL)

      shiny::observe({
        brush_vals <- brush()

        if (!is.null(brush_vals)) {
          brush_range$x <- as.POSIXct(c(brush_vals$xmin, brush_vals$xmax), origin = '1970-01-01')

          brush_range$y <-
            c(brush_vals$ymin, brush_vals$ymax)

        } else {
          brush_range$x <- NULL
          brush_range$y <- NULL
        }
      })

      output$PollutionRose <- shiny::renderPlot({
        shiny::req(nrow(data()) > 0,
                   is.character(param_rct()))

        if (is.null(brush_range$x)) {
          brush_range$x <- c(min(data()[[key_col]], na.rm=TRUE),
                             max(data()[[key_col]], na.rm=TRUE))
          brush_range$y <-
            c(min(data()[[param_rct()]], na.rm=TRUE),
              max(data()[[param_rct()]], na.rm=TRUE))
        }

        plot_data <- data() %>%
          dplyr::filter(between(!!sym(key_col),
                                brush_range$x[1],
                                brush_range$x[2])) %>%
          tidyr::drop_na(!!sym(wd_col),
                         !!sym(measurement_col),
                         !!sym(measurement_col))
        shiny::validate(
          shiny::need(nrow(plot_data) > 0,
                message = paste('Insuffcient data at selected site to',
                                'display pollution rose.')))

        minRose <- min(plot_data[[measurement_col]], na.rm = T)
        maxRose <- max(plot_data[[measurement_col]], na.rm = T)
        breaks <- seq(from = minRose, to = maxRose, length.out = 6)
        cols <- viridis::magma(length(breaks))

        unit <- plot_data %>%
                   dplyr::select(unit_col) %>%
                   unique() %>%
                   utils::head(1) %>%
                   dplyr::pull()

        footer <- unit_to_expression(unit)

        param <- param_to_expression(param_rct())
        if (is.null(title)) {
          title <- param
        }

        p <- openair::windRose(mydata = plot_data,
                               wd = wd_col,
                               ws = measurement_col,
                               paddle = FALSE,
                               pollutant = measurement_col,
                               breaks = breaks,
                               cols = cols,
                               offset = 5,
                                par.settings=list(fontsize=list(text=16),
                                                  par.main.text = list(just='left',
                                                                       x = grid::unit(5, "mm")),
                                                  par.xlab.text = list(just='center',
                                                                       x = grid::unit(5, "mm"))),
                               key.footer = parse(text = footer),
                               main = parse(text = title),
                               key.position = 'right')
        p
      })
    }
  })
}

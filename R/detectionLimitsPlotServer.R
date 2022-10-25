#' Detection Limits Plot Server
#'
#' Used alongside detectionLimitsPlotUI() to add a detection limits plot to app.
#' Text labels are plotted in a position determined by the x and y arguments,
#' colored by the color argument, and sized by the size argument.
#'
#' @param id string ID label that links detectionLimitsPlotUI() to detectionLimitsPlotServer()
#' @param input_data reactive Reactive dataframe with columns that determine x
#'                            and y position, label, size, and color.
#' @param x string Column name of x-position
#' @param y string Column name of y-position
#' @param color string Column name that determines color of plotted label
#' @param label string Column name with labels to be plotted
#' @param size string Column name that determines size of plotted label (numeric)
#' @param x_lab string (optional) X-axis label, default = 'Percent above detection limit (%)'
#' @param y_lab string (optional) Y-axis label, default = 'Detection limit (median in units of measure)'
#' @param color_lab string (optional) Title for color category in legend
#' @param size_lab string (optional) Title for size category in legend
#' @param title string (optional) Plot title
#' @param filter_col string (optional) Column used to filter data based on
#'                          specified `filter_rct`.
#' @param filter_rct reactive Value in `filter_col` to filter dataframe (equal).

#'
#' @return character vector Method codes displayed in the plot (updated by brush)
#' @importFrom shiny reactive observe observeEvent reactiveValues moduleServer renderPlot
#' @importFrom dplyr filter
#' @importFrom magrittr `%>%`
#' @importFrom ggplot2 ggplot geom_label geom_vline aes coord_cartesian scale_size labs
#' @importFrom janitor make_clean_names
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
detectionLimitsPlotServer <- function(id,
                                      input_data,
                                      x,
                                      y,
                                      label,
                                      color,
                                      size,
                                      x_lab = 'Percent above detection limit (%)',
                                      y_lab = 'Detection limit (median in units of measure)',
                                      color_lab=NULL,
                                      size_lab=NULL,
                                      title='',
                                      filter_col = NULL,
                                      filter_rct=shiny::reactive({NULL})) {
  # Check arguments
  assertthat::assert_that(is.reactive(input_data),
                          msg = paste0('`input_data` argument is not reactive.',
                                       'Wrap in `reactive({})` within function',
                                       'call.'))
  assertthat::assert_that(is.reactive(filter_rct),
                          msg = paste0('`filter_rct` argument is not reactive.',
                                       'Wrap in `reactive({})` within function',
                                       'call.'))
  assertthat::assert_that(class(c(x,y,label,color,size)) == 'character',
                          msg = 'All of x, y, label, color, size should be strings')

  shiny::moduleServer(id, {
    function(input, output, session) {

      data <- shiny::reactive({
        if (!is.null(filter_rct())) {
          assertthat::assert_that(!is.null(filter_col),
                                   msg= paste0('`filter_col` is not specified.'))
          data <- input_data() %>%
            dplyr::filter(!!sym(filter_col) == filter_rct())
        } else {
          data <- input_data()
        }
        return(data)
      })


      brush_range <-
        shiny::reactiveValues(x = NULL, y = NULL)

      shiny::observeEvent(input$DetectionLimitsDoubleclick, {
        brush <- input$DetectionLimitsBrush
        if (!is.null(brush)) {
          brush_range$x <- c(brush$xmin, brush$xmax)
          brush_range$y <-
            c(brush$ymin, brush$ymax)
        } else {
          brush_range$x <- NULL
          brush_range$y <- NULL
        }
      })

      if (is.null(size_lab)) {
        size_lab <- janitor::make_clean_names(size, case = 'title')
      }
      if (is.null(color_lab)) {
        color_lab <- janitor::make_clean_names(color, case = 'title')
      }

      output$DetectionLimitsPlot <- shiny::renderPlot({

        if (is.null(brush_range$x)) {
          shiny::observe({
            brush_range$x <- c(0,
                               100)
            brush_range$y <-
              c(min(data()[[y]], na.rm=TRUE),
                max(data()[[y]], na.rm=TRUE))
          })
        }

        plot <- ggplot2::ggplot(data = data()) +
          ggplot2::geom_label(ggplot2::aes(x = !!sym(x),
                         y = !!sym(y),
                         label = !!sym(label),
                         color = !!sym(color),
                         size = !!sym(size))) +
          ggplot2::scale_size(range = c(4,8)) +
          ggplot2::labs(x = x_lab,
               y = y_lab,
               size=size_lab,
               color=color_lab) +
          ggplot2::geom_vline(xintercept = 50,
                     color = 'orange',
                     linetype = 'dashed',
                     size = 1.5) +
          ggplot2::geom_vline(xintercept = 25,
                     color = 'red',
                     linetype = 'dashed',
                     size = 1.5) +
          ggplot2::coord_cartesian(xlim = brush_range$x,
                          ylim = brush_range$y,
                          expand = TRUE) +
          scale_color_sonoma() +
          theme_sonoma()

        plot})

      selected_methods <- shiny::reactiveValues(methods=NULL)

      shiny::observe({
        if (!is.null(brush_range$x[1])){
          selected_methods$methods <- data() %>%
            dplyr::filter(between(!!sym(x), brush_range$x[1], brush_range$x[2]),
                          between(!!sym(y), brush_range$y[1], brush_range$y[2])) %>%
                    dplyr::pull(label)
        }
      })
      return(selected_methods)


    }
  })
}

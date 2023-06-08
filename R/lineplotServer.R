#' Lineplot Server Function
#'
#' Use alongside lineplotUI() to add lineplot of concentration data to app.
#' Specified color determines how to separate data into distinct lines (
#' e.g. one line per AQS_SITECODE). Specify a reactive parameter and duration to filter
#' `input_data`. Specify `highlight = TRUE` and a reactive `highlight_value_rct`, a value from the
#' `highlight_col` column, to highlight the line for that group. Overlay points
#' over lines using the `point` argument.
#'
#' @param id string ID label that links lineplotUI() to lineplotServer()
#' @param input_data reactive Reactive dataframe
#' @param x string Column with values to plot on x-axis
#' @param y string Name of values to plot on y-axis
#' @param units string Name of column with units per value
#' @param color string Name of column with groups to separate data into
#'                     distinct lines (e.g. one line per sitecode)
#' @param linetype string (optional) Name of column that determines linetype. If null,
#'                            (default) all column will be solid.
#' @param line_width numeric (optional) Width of lines
#' @param point_size numeric (optional) Size of points, if points = TRUE
#' @param point boolean (optional) If TRUE (default), points will be plotted over
#'                         lines
#' @param label string (optional) Name of column with labels to print above every
#'                         data point
#' @param label_size integer (optional) Size of label text, if label is specified. (default = 8)
#' @param label_vjust numeric (optional) Vertical adjustment of labels compared
#'                            to position of observation, if label is specified. (default = -1.05)
#' @param title string (optional) Plot title
#' @param x_lab string (optional) X-axis label. If not specified, left blank
#' @param y_lab string (optional) Y-axis label.
#'                                If not specified, defaults to the first value
#'                                in the filtered data's units column.
#' @param color_lab string (optional) Legend label for color categories
#' @param linetype_lab string (optional) Legend label for linetype categories
#' @param filter_rct reactive Parameter to plot
#' @param filter_col string Column name of parameter names
#' @param filter2_rct reactive (optional) Duration to plot. If spcified, input_data
#'                                         will be filtered to the passed duration.
#' @param filter2_col string (optional) Column name of durations
#' @param highlight boolean (optional) If TRUE, all lines but highlight line will be
#'                                     gray. If false, colors applied as normal
#'                                     aes.
#' @param highlight_col string Column name with value to determine highlighted line(s)
#' @param highlight_value_rct reactive (optional) Value to highlight (equality)
#' @param x_numeric boolean (optional, default = TRUE) If TRUE, x-axis is
#'                          specified as continuous and pretty_breaks() is called.
#' @param corner_ann string (optional) Annotation to put in the top-right corner
#'                           when no highlight value is specified. Default is
#'                           "Select site on map to highlight"
#' @param plot_specs (optional) obj Additional GGplot elements to add to plot
#'                                  e.g. theme(axis.text.x = element_text(angle = 90)).
#'                                  Can be string of added elements.
#'
#' @return
#' @importFrom shiny moduleServer reactive renderPlot is.reactive
#' @importFrom ggplot2 ggplot geom_line aes labs geom_text theme scale_x_continuous geom_point
#' @importFrom gghighlight gghighlight
#' @importFrom scales pretty_breaks
#' @importFrom dplyr filter
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
lineplotServer <- function(id,
                           input_data,
                           point = FALSE,
                           x,
                           y,
                           units,
                           color,
                           line_width = 1,
                           point_size = 2,
                           linetype=NULL,
                           label=NULL,
                           label_size = 8,
                           label_vjust = -1.05,
                           title = '',
                           x_lab = '',
                           y_lab = '',
                           color_lab = '',
                           linetype_lab = '',
                           filter_rct = shiny::reactive({NULL}),
                           filter2_rct = shiny::reactive({NULL}),
                           filter_col = NULL,
                           filter2_col = NULL,
                           highlight = FALSE,
                           highlight_col = 'AQS_SITECODE',
                           highlight_value_rct = shiny::reactive({NULL}),
                           x_numeric = TRUE,
                           corner_ann = 'Select site on map to highlight',
                           plot_specs = NULL) {

  assertthat::assert_that(shiny::is.reactive(input_data),
                          msg = paste0('Input data must be reactive.',
                                       'Wrap in `reactive({})` within function',
                                       'call.'))

  assertthat::assert_that(shiny::is.reactive(filter_rct),
                          msg = paste('`filter_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(filter2_rct),
                          msg = paste('`filter2_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(highlight_value_rct),
                          msg = paste('`highlight_value_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))

  shiny::moduleServer(id, {
    function(input, output, session) {
      data <- shiny::reactive({
        data <- input_data()
      if (!is.null(filter_rct())) {
        data <- data %>%
          dplyr::filter(!!sym(filter_col) == filter_rct())
        }
      if (!is.null(filter2_rct())) {
        assertthat::assert_that(!is.null(filter2_col),
                                msg = '`filter2_rct` specified without `filter2_col`')
        data <- data %>%
          dplyr::filter(!!sym(filter2_col) == filter2_rct())
      }
        return(data)
      })

      if (is.null(color)) {
        color_val <- NULL
      } else {
        color_val <- dplyr::sym(color)
      }

      if (is.null(linetype)) {
        lt_val <- NULL
      } else {
        lt_val <- dplyr::sym(linetype)
      }

      output$Lineplot <- shiny::renderPlot({

        if (y_lab == '') {
          y_lab <- unique(data()[[units]])[1]
        }

        plot <- ggplot2::ggplot(data = data()) +
          ggplot2::geom_line(ggplot2::aes(
            x = !!sym(x),
            y = !!sym(y),
            color = !!color_val,
            linetype = !!lt_val
          ),
          size = line_width) +
          ggplot2::labs(y = y_lab,
               title = title,
               x = x_lab,
               color=color_lab,
               linetype=linetype_lab) +
          scale_color_sonoma() +
          theme_sonoma()

        if (point) {
          plot <- plot +
            ggplot2::geom_point(ggplot2::aes(
              x = !!sym(x),
              y = !!sym(y),
              color = !!color_val),
              size = point_size)
        }

          if(highlight) {

            hv <- ifelse(is.null(highlight_value_rct()),
                                 '',
                                 highlight_value_rct())
            plot <- plot +
              gghighlight::gghighlight(!!sym(highlight_col) == hv,
                                       label_params = list(size = 6),
                                       label_key = !!color_val) +
              ggplot2::geom_text(
                ggplot2::aes(
                  x = Inf,
                  y = Inf,
                  label = ifelse(is.null(highlight_value_rct()),
                                  corner_ann,
                                  ''),
                  hjust = 1.05,
                  vjust = 1.05),
                color = 'black',
                size = 7) +
              ggplot2::theme(legend.position = 'none')
          }

        if (!is.null(label)) {
          plot <- plot +
            ggplot2::geom_text(ggplot2::aes(x = !!sym(x),
                                            y = !!sym(y)+0.4,
                                            label = !!sym(label)),
                               size = label_size,
                               vjust = label_vjust,
                               show.legend = FALSE)
        }

        if (x_numeric) {
          plot <- plot +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
        }

        if (!is.null(plot_specs)) {
          plot <- plot + plot_specs
        }
        plot

      })

    }
  })
}

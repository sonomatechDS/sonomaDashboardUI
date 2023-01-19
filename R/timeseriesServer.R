#' Timeseries Server Function
#'
#' Used alongside timeseriesUI() to add a timeseries of a 1hr parameter
#' and optionally a secondary 1hr OR 24hr parameter (wrapped in a fluidRow)
#' to an app. Timeseries will include zoom functionality, where a box
#' can be dragged across the plot area and double-clicked. To reset zoom,
#' double-click the plot area again.
#'
#' 1Hr data and 24Hr data should have the same x, y, and units column names.
#' A secondary 1Hr parameter will be displayed as a dotted line. A secondary
#' 24Hr parameter will be displayed as points.
#'
#' @param id string ID label that links timeseriesUI() to timeseriesServer()
#' @param data_param1_rct reactive Reactive hourly data
#' @param data_param2_rct reactive (optional) Reactive data (either 1hr or 24hr)
#'                                 for the optionally specified sec_param_rct.
#'                                 Should have the same columns as
#'                                 data_param1_rct.
#' @param x string Column name of datetime value (POSIXct format)
#' @param y string Column name of concentration values
#' @param param_col string Column name of parameter values
#' @param unit_col string Column name of unit values
#' @param param_rct reactive String value of primary parameter to display
#' @param sec_param_rct reactive (optional) String value of secondary parameter
#'                               to display. If not specified, only 1 parameter
#'                               (param_rct) is displayed.
#' @param sec_duration_rct reactive (optional) Duration of secondary parameter.
#'                                   Either '1 HOUR' or other. If `sec_param_rct`
#'                                   is specified, this must also be specified.
#' @param linesize numeric (optional) Size of plotted line (default = 0.5)
#' @param pointsize numeric (optional) Size of plotted points (default = 3)
#'
#' @return reactive List with elements `x` and `y`, each a vector with the min
#'                  and max x and y values (respectively) of the brush.
#' @importFrom shiny reactive reactiveValues req observeEvent observe renderPlot moduleServer is.reactive
#' @importFrom ggplot2 ggplot aes labs geom_line geom_point scale_x_datetime scale_y_continuous scale_color_manual sec_axis guides guide_legend scale_linetype_manual
#' @importFrom tidyquant coord_x_datetime
#' @importFrom dplyr filter sym
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
timeseriesServer <- function(id,
                             data_param1_rct,
                             data_param2_rct = shiny::reactive({NULL}),
                             x,
                             y,
                             param_col,
                             unit_col,
                             param_rct,
                             sec_param_rct = shiny::reactive({
                               NULL
                             }),
                             sec_duration_rct = shiny::reactive({
                               NULL
                             }),
                             linesize = .5,
                             pointsize = 3) {
  assertthat::assert_that(shiny::is.reactive(data_param1_rct) & is.reactive(data_param2_rct),
                          msg = paste('All input data must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(param_rct),
                          msg = paste('`param_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(sec_param_rct),
                          msg = paste('`sec_param_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(sec_duration_rct),
                          msg = paste('`sec_duration_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  shiny::moduleServer(id,
                      function(input, output, session) {

                        # Set Range of x/y axes, incorporating brushed range
                        brush_range <- shiny::reactiveValues(x = NULL,
                                                             y = NULL,
                                                             brush = NULL)

                        param_range <- shiny::reactiveValues(min_x = NULL,
                                                             max_x = NULL,
                                                             min_y = NULL,
                                                             max_y = NULL)
                        # Record max x and y values for available parameters
                        shiny::observe({
                          req(data_param1_rct())
                          param_range$max_x <- max(data_param1_rct()[[x]],
                                  ifelse(!is.null(sec_param_rct()), data_param2_rct()[[x]], NA),
                                  na.rm=TRUE)

                          param_range$min_x <- min(data_param1_rct()[[x]],
                                  ifelse(!is.null(sec_param_rct()), data_param2_rct()[[x]], NA),
                                  na.rm=TRUE)

                          # Only include primary parameter in y-range consideration
                          # to account for axis multiplier for 2nd axis.
                          param_range$max_y <- max(data_param1_rct()[[y]],
                                  # ifelse(!is.null(sec_param_rct()), data_param2_rct()[[y]], NA),
                                  na.rm=TRUE)

                            param_range$min_y <- min(data_param1_rct()[[y]],
                                  # ifelse(!is.null(sec_param_rct()), data_param2_rct()[[y]], NA),
                                  na.rm=TRUE)
                        })

                        # Update brush range on double click if area is brushed
                        shiny::observeEvent(input$TSDoubleclick, {
                          brush <- input$TSBrush
                          if (!is.null(brush)) {
                            brush_range$x <- c(brush$xmin, brush$xmax)
                            brush_range$y <- c(brush$ymin, brush$ymax)
                            brush_range$brush <- brush
                          } else {
                            req(param_range$max_x)
                            brush_range$x <- c(param_range$min_x, param_range$max_x)
                            brush_range$y <- c(param_range$min_y, param_range$max_y)
                            brush_range$brush <- NULL
                          }
                        })

                        # Listen for parameter change
                        param_change <- shiny::reactive({
                          list(param_rct(),sec_param_rct())
                        })

                        # Reset brush range if parameters change
                        shiny::observeEvent(param_change(), {
                          req(param_range$max_x)
                          brush_range$x <- c(param_range$min_x, param_range$max_x)
                          brush_range$y <- c(param_range$min_y, param_range$max_y)
                          brush_range$brush <- NULL
                        })


                        # Unit label for primary parameter
                        unit1 <- shiny::reactive({
                          shiny::req(is.data.frame(data_param1_rct()))
                          return(unique(data_param1_rct()[[unit_col]]))
                        })
                        # Set axis label for primary parameter
                        axis_lab_1 <- reactive({
                          paste0(param_to_expression(param_rct()),
                                             '~`in`~',
                                             unit_to_expression(unit1()))
                          })

                          axis_lab_2 <- shiny::reactive({
                            shiny::req(data_param2_rct())
                            paste0(param_to_expression(sec_param_rct()),
                                               '~`in`~',
                                               unit_to_expression(unique(data_param2_rct()[[unit_col]])))
                          })


                        output$Timeseries <- shiny::renderPlot({

                          shiny::req(is.data.frame(data_param1_rct()),
                                     class(param_rct())=='character',
                                     param_rct() != ' - ')

                          if (!is.null(sec_param_rct())){
                            req(axis_lab_2(), data_param2_rct())
                            # Establish multiplier for second axis
                            range1 <- range(data_param1_rct()[[y]], na.rm=T)
                            range2 <- range(data_param2_rct()[[y]], na.rm=T)
                            s <- diff(range1)/diff(range2)
                            axis_mult <-
                              max(data_param1_rct()[[y]], na.rm = TRUE) / max(data_param2_rct()[[y]],
                                                                              na.rm = TRUE)
                            # Non 1-hr plots are plotted as points
                            if (sec_duration_rct() != '1 HOUR') {

                              TSplot <- ggplot2::ggplot() +
                                ggplot2::geom_line(data = data_param1_rct(),
                                                   ggplot2::aes(
                                                     x = !!dplyr::sym(x),
                                                     y = !!dplyr::sym(y),
                                                     color = param_rct()
                                                   ),
                                                   size = linesize) +
                                ggplot2::geom_point(
                                  data = data_param2_rct(),
                                  ggplot2::aes(
                                    x = !!dplyr::sym(x),
                                    y = ((!!dplyr::sym(y) - range2[1])*s)+range1[1],
                                    color = sec_param_rct()
                                  ),
                                  size=pointsize,
                                  shape = 21,
                                  stroke = 1.5
                                ) +
                                ggplot2::labs(
                                  y = parse(text = axis_lab_1()),
                                  x = '',
                                  color = ''
                                ) +
                                ggplot2::scale_x_datetime() +
                                tidyquant::coord_x_datetime(xlim = brush_range$x,
                                                            ylim = brush_range$y,
                                                            expand = TRUE) +
                                ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(
                                  ~((.-range1[1])/s)+range2[1],
                                  name = parse(text = axis_lab_2())
                                )) +
                                ggplot2::scale_color_manual(
                                  values = setNames(
                                    sonoma_color_palette(3)[c(1,3)],
                                    c(param_rct(), sec_param_rct())
                                  ),
                                  breaks = c(param_rct(), sec_param_rct())
                                ) +
                                theme_sonoma() +
                                ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(
                                  size = c(1, 3),
                                  linetype =
                                    c(1, NA),
                                  shape =
                                    c(NA, 21)
                                )))
                              # 1hr plots are plotted as lines
                            } else {
                              TSplot <- ggplot2::ggplot() +
                                ggplot2::geom_line(
                                  data = data_param1_rct(),
                                  ggplot2::aes(
                                    x = !!dplyr::sym(x),
                                    y = !!dplyr::sym(y),
                                    color = param_rct(),
                                    linetype = param_rct()
                                  ),
                                  size=linesize
                                ) +
                                ggplot2::labs(
                                  y = parse(text = axis_lab_1()),
                                  x = '',
                                  color = '',
                                  linetype = ''
                                ) +
                                ggplot2::scale_x_datetime() +
                                tidyquant::coord_x_datetime(xlim = brush_range$x,
                                                            ylim = brush_range$y,
                                                            expand = TRUE) +
                                ggplot2::geom_line(
                                  data = data_param2_rct(),
                                  ggplot2::aes(
                                    x = !!dplyr::sym(x),
                                    y = ((!!dplyr::sym(y) - range2[1])*s)+range1[1],
                                    color = sec_param_rct(),
                                    linetype = sec_param_rct()
                                  ),
                                  size=linesize
                                ) +
                                ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(
                                  ~((.-range1[1])/s)+range2[1],
                                  name = parse(text = axis_lab_2())
                                )) +
                                ggplot2::scale_linetype_manual(
                                  values = setNames(
                                    c('solid', 'twodash'),
                                    c(param_rct(), sec_param_rct())
                                  ),
                                  breaks = c(param_rct(), sec_param_rct())
                                ) +
                                ggplot2::scale_color_manual(
                                  values = setNames(
                                    sonoma_color_palette(2),
                                    c(param_rct(), sec_param_rct())
                                  ),
                                  breaks = c(param_rct(), sec_param_rct())
                                ) +
                                theme_sonoma() +
                                ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size =
                                                                                                    1)))
                            }
                          } else {
                            req(nrow(data_param1_rct()) > 0)
                            TSplot <- ggplot2::ggplot() +
                              ggplot2::geom_line(data = data_param1_rct(),
                                                 ggplot2::aes(x = !!dplyr::sym(x),
                                                              y = !!dplyr::sym(y)),
                                                 size=linesize,
                                                 color = sonoma_color_palette(1)[1]) +
                              ggplot2::labs(y = parse(text = axis_lab_1()),
                                            x = '') +
                              scale_x_datetime() +
                              tidyquant::coord_x_datetime(xlim = brush_range$x,
                                                          ylim = brush_range$y,
                                                          expand = TRUE) +
                              theme_sonoma()
                          }

                          TSplot
                        })
                        return(brush_range)
                      })
}

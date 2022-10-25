#' Network Completeness Plot Server
#'
#' Used alongside networkCompletenessUI() to add a network completeness
#' plot to app. For each x-value (site), a vertical segment from seg_min to seg_max is
#' plotted, typically representing max and min completeness values across
#' parameters for a site. Overlaid on these segments are points representing
#' completeness value for each parameter measured at a site. A certain x-value
#' can be highlighted with an arrow and label. A horizontal line may be added
#' at a given y-intercept. A reactive filter column and value may be passed to
#' filter plotted data.
#'
#' @param id string ID that links networkCompletenessUI() to networkCompletenessServer()
#' @param input_data reactive Reactive data frame
#' @param x string Column name of x-axis value
#' @param segment boolean (optional) If TRUE (default), a vertical segment will
#'                        be drawn. Requires seg_min, seg_max values to be filled.
#' @param seg_min string (optional) Required if segment is TRUE.
#'                       Column name of segment minimum value
#' @param seg_max string (optional) Required if segment is TRUE.
#'                       Column name of segment maximum value
#' @param y string Column name of y-axis value of points
#' @param color string (optional) Column name of value to color points by
#' @param param_col string (optional) Column name for parameter name
#' @param param_code_col string (optional) Column name for parameter code.
#'                              Should be specified if passing a subset of
#'                              parameter codes in `param_code_list`.
#' @param param_code_list character vector (optional) List of parameter codes to plot.
#' @param table_cols character vector (optional) Columns to include in table
#'                                    that appears when plot is brushed. If not
#'                                    specified, all columns are included.
#' @param table_col_labels character vector (optional) Specific column labels to
#'                                          print in table. Must match in order
#'                                          with 'table_cols'. If not specified,
#'                                          column names in input_data will
#'                                          be used.
#' @param xlab string (optional) Label for x-axis. Default = 'Rank ordered site completeness'.
#' @param ylab string (optional) Label for y-axis. Default = 'Completeness (%)'.
#' @param title_rct reactive (optional) Plot title. Must be reactive. Default = reactive({NULL}).
#' @param hline_int integer (optional) Y-intercept of horizontal line. Default = NA.
#' @param hline_ann string (optional) Annotation for optional horizontal line. Default = ''.
#' @param hline_color string (optional) Color of hline (if hline int specified)
#' @param filter_col string (optional) Column name of column to filter data by. Default = NULL.
#' @param filter_rct reactive (optional) Value to filter data by. Default = reactive({NULL}).
#' @param highlight_col string (optional) Column name of column that determines segment to highlight. Default = NULL.
#' @param highlight_rct reactive (optional) Value to highlight. Default = reactive({NULL}).
#' @param dragbox_x_only boolean (optional) Default = F. If TRUE, table will be filtered
#'                                          only by the x-range of the dragbox (e.g.
#'                                          all y values in the selected x-range will
#'                                          be shown in the table).
#' @param corner_ann string (optional) Annotation in plot corner when no value is highlighted.
#' @param shape_values vector (optional) A vector of the full set of parameters
#'                                       to assign a standard shape to. Useful
#'                                       if input_data is reactively filtered
#'                                       and consistent shapes are desired.
#'                                       Must contain all possible values for
#'                                       parameter that may appear in the plot.
#' @param color_values vector (optional) A vector of the full set of values
#'                                       in the specified color column
#'                                       to assign a standard color to. Useful
#'                                       if input_data is reactively filtered
#'                                       and consistent colors are desired.
#'                                       Must contain all possible values for
#'                                       color that may appear in the plot.
#' @param plot_specs object (optional) Additional ggplot elements to add to the
#'                          plot (e.g. ylim(0,100)). Can be multiple elements
#'                          added together.
#' @param subset_graph_col string (optional) If specified, input_data must contain
#'                                a boolean column that indicates a subset of rows
#'                                that may appear on the graph. Only data with a
#'                                value of TRUE in this column will be graphed,
#'                                though all data values will appear in table that meet
#'                                the brush specifications. e.g. Can be used to plot
#'                                only the lowest POC at a site, while showing
#'                                information on all POCs in the details table.
#'
#' @return
#' @importFrom shiny moduleServer reactive renderPlot renderTable
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_hline annotate aes labs theme scale_size scale_shape_manual arrow unit
#' @importFrom dplyr filter mutate distinct select arrange
#' @importFrom stats setNames
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
networkCompletenessServer <- function(id,
                                      input_data,
                                      x,
                                      segment = TRUE,
                                      seg_min = NULL,
                                      seg_max = NULL,
                                      y,
                                      color = NULL,
                                      param_col = NULL,
                                      param_code_col = NULL,
                                      param_code_list = NULL,
                                      table_cols = NULL,
                                      table_col_labels = NULL,
                                      xlab = 'Rank ordered site completeness',
                                      ylab = 'Completeness (%)',
                                      title_rct = reactive({NULL}),
                                      hline_int = NA_real_,
                                      hline_ann = '',
                                      hline_color = 'red',
                                      filter_col = NULL,
                                      filter_rct = shiny::reactive({NULL}),
                                      highlight_col = NULL,
                                      highlight_rct = shiny::reactive({NULL}),
                                      dragbox_x_only = F,
                                      corner_ann = '',
                                      shape_values = NULL,
                                      color_values = NULL,
                                      plot_specs = NULL,
                                      subset_graph_col = NULL
                                      ) {
  assertthat::assert_that(is.reactive(input_data),
                          msg = paste('Input data must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(is.reactive(filter_rct),
                          msg = paste('`filter_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(is.reactive(highlight_rct),
                          msg = paste('`highlight_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))

  shiny::moduleServer(id, {
    function(input, output, session) {

      ns <- shiny::NS(id)

        data <- shiny::reactive({
          data <- input_data() %>%
            distinct()
          if(!is.null(filter_rct())) {
            assertthat::assert_that(!is.null(filter_col),
                                    msg = paste('`filter_rct` provided without',
                                                '`filter_col`. Both must be',
                                                'specified.'))
            data <- data %>%
              dplyr::filter(!!sym(filter_col) == filter_rct())
          }
          if (!is.null(param_code_list)) {
            assertthat::assert_that(!is.null(param_code_col),
                                    msg = paste('`param_list` provided without',
                                                '`param_col`. Both must be',
                                                'specified.'))
            data <- data %>%
              dplyr::filter(!!dplyr::sym(param_code_col) %in% param_code_list)
          }
          return(data)
          })


        chosen_site <- shiny::reactive({
          if(!is.null(highlight_rct())) {
            req(data())
            assertthat::assert_that(!is.null(highlight_col),
                                    msg = paste('`highlight_rct` provided without',
                                                '`highlight_col`. Both must be',
                                                'specified.'))
            chosen_site <- data() %>%
              dplyr::filter(!!sym(highlight_col) == highlight_rct()) %>%
              dplyr::mutate(seg_length = !!sym(seg_max) - !!sym(seg_min))
          } else {
            chosen_site <- input_data() %>%
              dplyr::filter(!!sym(x) == 'No Value Here!!!')
          }
          return(chosen_site)
        })

        if (is.null(color)) {
          color_val <- NULL
        } else {
          color_val <- dplyr::sym(color)
        }

        output$NetworkCompleteness <- shiny::renderPlot({
          unique_params <- unique(input_data()[[param_col]])
          if (is.null(shape_values)) {
            shape_vals_all <- setNames(c(16, 15, 17, 0,
                                     1, 18,  2, 15, 9)[1:length(unique_params)], unique_params)
          } else {
            assertthat::assert_that(all(unique_params %in% shape_values),
                                          msg = "shape_values does not contain all parameters in input_data")
            shape_vals_all <- setNames(c(16, 15, 17, 0,
                                         1, 18,  2, 15, 9)[1:length(shape_values)],
                                       shape_values)
          }

          shape_vals_plot <- shape_vals_all[unique(data()[[param_col]])]


          if (!is.null(color)) {
            unique_colors <- unique(input_data()[[color]])
            if (is.null(color_values)) {
              color_vals_all <- setNames(sonoma_color_palette(length(unique_colors)), unique_colors)
            } else {
              assertthat::assert_that(all(unique_colors %in% color_values),
                                            msg = "color_values does not contain all values in the assigned color column in input_data")
              color_vals_all <- setNames(sonoma_color_palette(length(color_values)),
                                         color_values)
            }
            color_vals_plot <- color_vals_all[unique(data()[[color]])]
          } else {
            color_vals_plot <- NULL
          }

          if (!is.null(subset_graph_col)) {
            plot_data <- data() %>%
              filter(!!sym(subset_graph_col) == TRUE)
          } else {
            plot_data <- data()
          }

          plot <- ggplot2::ggplot(data=plot_data)
          if (segment) {
              plot <- plot +
                geom_segment(aes(x = !!sym(x), xend = !!sym(x),
                                 y = !!sym(seg_min), yend = !!sym(seg_max)),
                             size = 1,
                             alpha = 0.5,
                             arrow = NULL,
                             color= 'gray')
          }
          plot <- plot +
                    ggplot2::geom_point(ggplot2::aes(x = !!sym(x),
                                   y = !!sym(y),
                                   color = !!color_val,
                                   shape = !!sym(param_col)),
                               size = 3) +
            ggplot2::labs(x = xlab,
                         y = ylab,
                         title = title_rct()) +
            ggplot2::geom_hline(yintercept = hline_int,
                               color = hline_color,
                               linetype='dashed') +
            ggplot2::annotate("text",
                             x = 0,
                             y = hline_int-3,
                             hjust = "inward",
                             label = hline_ann,
                             color = hline_color,
                             size = 5) +
            ggplot2::scale_color_manual(values = color_vals_plot) +
            ggplot2::scale_shape_manual(values = shape_vals_plot) +
                    theme_sonoma() +
            ggplot2::theme(legend.position = 'bottom', legend.box='horizontal') +
            ggplot2::guides(shape = guide_legend(nrow=3,
                                                 override.aes = list(size=3)),
                            color = guide_legend(nrow=3))

          if (nrow(chosen_site()) > 0) {
            plot <- plot +
              ggplot2::geom_segment(
                data = chosen_site(),
                ggplot2::aes(x = !!sym(x),
                    xend = !!sym(x),
                    y = !!sym(seg_max) - seg_length - 20,
                    yend = !!sym(seg_max) - seg_length - 3),
                color = 'red',
                size = .75,
                alpha = 0.5,
                lineend='butt',
                linejoin='mitre',
                arrow = ggplot2::arrow(length = ggplot2::unit(.15, 'inches'))) +
              annotate("text",
                       x = chosen_site()[[1,x]],
                       y = chosen_site()[[1,seg_max]] - chosen_site()[[1,'seg_length']] - 22,
                       label = chosen_site()[[1, highlight_col]],
                       color = 'red',
                       size = 5) +
              ggplot2::ylim(c(min(0, chosen_site()[[1,seg_max]] - chosen_site()[[1,'seg_length']] - 25),
                              105))
          } else {
            plot <- plot +
              ggplot2::annotate("text",
                       x = -Inf,
                       y = -Inf,
                       label = corner_ann,
                       color = 'Black',
                       size = 7,
                       hjust = -0.03,
                       vjust = -0.5)
          }
          if (!is.null(plot_specs)) {
            plot <- plot + plot_specs
          }
          plot
          })


        observeEvent(input$NCDoubleclick, {

            if(is.null(table_cols)) {
              table_cols <- colnames(data())
            }

            if(is.null(table_col_labels)) {
              table_col_labels <- table_cols
            }

            assertthat::assert_that(length(table_cols) == length(table_col_labels),
                                    msg = "Length of requested table cols must equal length of labels.")

            if (!is.null(input$NCBrush)){
              data_select <- data() %>%
                          filter(dplyr::between(!!sym(x),
                                         input$NCBrush[['xmin']] - 0.1,
                                         input$NCBrush[['xmax']] + 0.1))
              if (!dragbox_x_only) {
                data_select <- data_select %>% filter(
                                 dplyr::between(!!sym(y),
                                                input$NCBrush[['ymin']] - 0.1,
                                                input$NCBrush[['ymax']] + 0.1))
                }

              data_select <- data_select %>%
                dplyr::distinct() %>%
                dplyr::arrange(!!dplyr::sym(x)) %>%
                dplyr::mutate_if(is.numeric, round, 2)

              if (!is.null(subset_graph_col)) {
                data_select <- data_select %>%
                  mutate(across(!(!!sym(subset_graph_col)), as.character)) %>%
                  mutate(across(!(!!sym(subset_graph_col)), ~ case_when(!(!!sym(subset_graph_col)) ~ paste0("<em>",.,"</em>"), TRUE ~ .)))

                html_cap <- "<em>Italicized rows represent collocated measurements not displayed in the plot.</em>"
              } else {
                html_cap <- NULL
              }

              data_select <- data_select %>%
                dplyr::select(table_cols) %>%
                  stats::setNames(., table_col_labels)

              if (nrow(data_select) == 0) {
                data_select <- data.frame()
              }
            } else {
              data_select <- data.frame()
            }
        output$NCSelect <- shiny::renderTable({data_select},
                                              sanitize.text.function=function(x){x},
                                              caption = html_cap, caption.placement = "top")
        })

        observeEvent(data(),
                     {
                       output$NCSelect <- NULL
                     })

    }
  })
}

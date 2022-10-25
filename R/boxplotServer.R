#' Boxplot Server Function
#'
#' Use alongside boxplotUI() to add boxplot to app.
#' Upper box boundary is 75th percentile, lower boundary is 25th percentile.
#' Whiskers extend to +/- 1.5*IQR beyond the upper/lower box boundaries in either
#' direction.
#'
#' Optional features include annotations, and an hline.
#' Data should be reactive, If specified, the plot's coordinates may be
#' flipped (e.g. `x` and `x_lab` will appear on the y-axis, and boxes will be
#' horizontally oriented.)
#'
#' @param id string ID label that links boxplotUI() to boxplotServer()
#' @param data reactive Reactive dataframe
#' @param x string Column name of data categories to place on x-axis
#' @param y string Column name of values used to calculate boxes per x value
#' @param agg_ann string (optional) Column name with values to aggregate and
#'                                  use as labels per each x value
#' @param agg_fun FUN (optional) Function used to aggregate agg_ann when
#'                               calculating labels per box (e.g. sum, min, max)
#' @param fill string (optional) Column name of fill value of each box
#' @param ann string (optional, default = NULL) Column name of text annotation
#'                   for each box
#' @param x_lab string (optional) X-axis label
#' @param y_lab string (optional) Y-axis label
#' @param fill_lab string (optional) Legend label of fill category
#' @param title string (optional) Title of plot
#' @param hline_int int (optional) X intercept of hline if one is desired. If
#'                       not specified, no hline will be drawn.
#' @param hline_ann string (optional) Label for hline
#' @param coord_flip (optional) If TRUE (defualt), coordinates are flipped
#'                              (boxes are oriented horizontally).
#' @param position_dodge numeric (optional) The offset between boxes plotted for
#'                                          the same x-value. Default is 0.5.
#' @param position_dodge_ann numeric (optional) The offset between labels
#'                                   plotted for the same x-value. Default=0.75.
#' @param ann_vjust numeric (optional) The vertical adjustment of optional box
#'                          labels. Default is 1.
#' @param plot_specs (optional) obj Additional GGplot elements to add to plot
#'                          e.g. theme(axis.text.x = element_text(angle = 90)).
#'                          If multiple elements are to be added, must be
#'                          a vector (e.g. c(ylim(0,4),
#'                          hline(y_intercept = 0, color = 'red))).
#'
#' @return
#' @importFrom ggplot2 ggplot geom_boxplot coord_flip aes scale_y_continuous geom_hline geom_text labs position_dodge2
#' @importFrom shiny renderPlot moduleServer
#' @importFrom assertthat assert_that
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by summarize
#' @export
#'
#' @examples
boxplotServer <- function(id,
                          data,
                          x,
                          y,
                          fill = NULL,
                          ann = NULL,
                          agg_ann = NULL,
                          agg_fun = NULL,
                          x_lab='',
                          y_lab='',
                          fill_lab = '',
                          title='',
                          hline_int = NA_real_,
                          hline_ann = NA,
                          coord_flip = FALSE,
                          position_dodge = 0.5,
                          position_dodge_ann = 0.75,
                          ann_vjust = 1,
                          plot_specs = NULL) {
  # Check arguments
  assertthat::assert_that(is.reactive(data),
                          msg = paste0('`data` argument must be reactive.',
                                       'Wrap in `reactive({})` within function',
                                       'call.'))
  shiny::moduleServer(id, {
    function(input, output, session) {

      if (is.null(fill)) {
        fill_val <- NULL
      } else {
        fill_val <- dplyr::sym(fill)
      }

      output$Boxplot <- shiny::renderPlot({

        if (!is.null(agg_ann)) {
        plot_ann <- data() %>%
            dplyr::group_by(!!dplyr::sym(x), !!fill_val) %>%
            dplyr::summarize(ypos = max(!!dplyr::sym(ann)),
                      label = agg_fun(!!sym(agg_ann)))
        print(plot_ann)
        }

        plot <- ggplot2::ggplot() +
          ggplot2::geom_boxplot(data=data(),
                                ggplot2::aes(x = !!dplyr::sym(x),
                                             y = !!dplyr::sym(y),
                                             fill = !!fill_val),
                                position = ggplot2::position_dodge2(position_dodge)) +
          ggplot2::scale_y_continuous() +
          ggplot2::labs(y = y_lab,
                        x = x_lab,
                        title = title,
                        fill = fill_lab) +
          ggplot2::geom_hline(yintercept = hline_int,
                              color = 'gray',
                               size = 1,
                               linetype = 'dashed') +
          ggplot2::geom_text(aes(x=-Inf,
                             y=hline_int,
                             label=hline_ann),
                             color='darkgray',
                             hjust = "inward",
                             vjust = ann_vjust,
                             size=8) +
          scale_fill_sonoma() +
          theme_sonoma()
        if (!is.null(agg_ann)) {
          plot <- plot +
            ggplot2::geom_text(data=plot_ann,
                               ggplot2::aes(x = !!dplyr::sym(x),
                                            y = ypos,
                                            label = label),
                               vjust = ann_vjust,
                               size = 6,
                               show.legend = FALSE,
                               position = ggplot2::position_dodge2(position_dodge_ann))
        }
        if (coord_flip) {
          plot <- plot + ggplot2::coord_flip()
        }
        if (!is.null(plot_specs)) {
          plot <- plot + plot_specs
        }
        plot
      })
    }
  })
}

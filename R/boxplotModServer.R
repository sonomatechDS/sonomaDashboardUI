#' Boxplot Mod Server Function
#'
#' Use alongside boxplotModUI() to add modified boxplot to app.
#'  Optional features include annotations, and an hline.
#'
#' Modified boxplot allows user to specify extent of box limits (lower and upper)
#' and whisker limits (ymin and ymax)vby passing columns names with minimum and
#' maximum extent values. Data should be reactive, and include columns with "upper",
#' "lower", "ymin", "ymax" and "median" values already calculated.
#' E.g. "upper" might be the 90th percentile for an x value and "lower"
#' could be the 10th percentile.  If specified, the plot's coordinates may be
#' flipped (e.g. `x` and `x_lab` will appear on the y-axis, and boxes will be horizontally
#' oriented.)
#'
#' @param id string ID label that links boxplotUI() to boxplotServer()
#' @param data reactive Reactive dataframe
#' @param x string Column name of data to place on x-axis
#' @param lower string Column name of lower border value of each box
#' @param upper string Column name of upper border value of each box
#' @param ymin string Column name of lower whisker extent for each box
#' @param ymax string Column name of upper whisker extent for each box
#' @param median string Column name of median value of each box
#' @param x_group string (optional) Column name of column to concatenate
#'                                  with x-column to make each x value
#'                                  unique (e.g. duration)
#' @param fill string (optional) Column name of fill value of each box
#' @param color string (optional) Column name of color value of each box (outline)
#' @param ann string (optional, default = NULL) Column name of text annotation
#'                   for each box
#' @param ann_position_adjust (optional) default = 0.4 The y-displacement of
#'                            annotations, if ann is specified. Increase value
#'                            to move annotations further from box.
#' @param x_lab string (optional) X-axis label
#' @param y_lab string (optional) Y-axis label
#' @param color_lab string (optional) Legend label of color category
#' @param fill_lab string (optional) Legend label of fill category
#' @param title string (optional) Title of plot
#' @param hline_int int (optional) X intercept of hline if one is desired. If
#'                       not specified, no hline will be drawn.
#' @param coord_flip (optional) If TRUE (defualt), coordinates are flipped (boxes are
#'                               oriented horizontally).
#'
#' @return
#' @importFrom ggplot2 ggplot geom_boxplot coord_flip aes scale_y_continuous geom_hline geom_text labs
#' @importFrom shiny renderPlot moduleServer
#' @importFrom assertthat assert_that
#' @importFrom magrittr `%>%`
#' @export
#'
#' @examples
boxplotModServer <- function(id,
                          data,
                          x,
                          ymin,
                          lower,
                          upper,
                          ymax,
                          median,
                          fill = NULL,
                          color = NULL,
                          x_group = NULL,
                          ann = NULL,
                          ann_position_adjust = 0.4,
                          x_lab='',
                          y_lab='',
                          fill_lab = '',
                          color_lab = '',
                          title='',
                          hline_int = NA_real_,
                          coord_flip = TRUE) {
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
        fill_val <- sym(fill)
      }

      if (is.null(color)) {
        color_val <- NULL
      } else {
        color_val <- sym(color)
      }
      # fill_val <- dplyr::if_else(is.null(fill), NULL, sym(fill))
      # color_val <- dplyr::if_else(is.null(color), NULL, sym(color))

      output$Boxplot <- shiny::renderPlot({
        if (!is.null(x_group)){
          plot_data <- data() %>%
            dplyr::mutate(x_dur = paste0(!!sym(x),'; ', !!sym(x_group)))
        } else {
          plot_data <- data()
        }



        plot <- ggplot2::ggplot(data=plot_data) +
          ggplot2::geom_boxplot(
            ggplot2::aes(x = x_dur,
                         lower = !!sym(lower),
                         ymin = !!sym(ymin),
                         ymax = !!sym(ymax),
                         upper = !!sym(upper),
                         middle = !!sym(median),
                         fill = !!fill_val,
                         color=!!color_val),
            stat = 'identity') +
          ggplot2::scale_y_continuous() +
          #ggplot2::coord_flip() +
          ggplot2::labs(y = y_lab,
                        x = x_lab,
                        title = title,
                        fill = fill_lab,
                        color = color_lab) +
          ggplot2::geom_hline(yintercept = hline_int,
                              color = 'gray',
                              size = 1,
                              linetype = 'dashed') +
          scale_fill_sonoma() +
          theme_sonoma()
        if (!is.null(ann)) {
          plot <- plot +
            ggplot2::geom_text(ggplot2::aes(x = x_dur,
                                            y = !!sym(upper)+ann_position_adjust,
                                            label = paste(!!sym(ann), 'Sites'),
                                            size = 14),
                               show.legend = FALSE)
        }
        if (coord_flip) {
          plot <- plot + ggplot2::coord_flip()
        }
        plot
      })
    }
  })
}

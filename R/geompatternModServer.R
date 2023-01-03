#' Boxplot/Barplot with pattern server function
#' 
#' Creates a barplot or boxplot with pattern fill depending on number of variables
#'
#' @param id id that links server to UI function
#' @param data reactive data.frame
#' @param x x-axis column name as a string
#' @param y y-axis column name as a string
#' @param fill_col fill column name as a string
#' @param legend_title title of legend
#' @param geom_plot_type either "bar" or "box"
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param title plot title
#' @param caption plot caption (optional)
#' @param subtitle plot subtitle (optional)
#' @param pattern_map pattern map (from pattern_mapping() function)
#' @param color_map color map (from color_mapping() function)
#' @param breaks vector of breaks to map onto map pattern and colors map vectors (should be same length as pattern_map and color_map)
#' @param second_axis boolean, display second identical y-axis?
#' @param coord_flip boolean, flip figure?
#' @param bar_stack boolean, should the bar plot be stacked or dodged?
#' @param sort_x_by # optional vector to sort the x-axis (should be the same length as x-axis column)
#' @param ... # any argument passed to geom_bar_pattern or geom_boxplot_pattern
#'
#' @return
#' @importFrom shiny reactive moduleServer renderPlot
#' @importFrom dplyr sym
#' @importFrom ggpattern geom_boxplot_pattern geom_bar_pattern scale_pattern_manual
#' @importFrom ggplot2 ggplot aes labs position_dodge2 guide_legend sec_axis derive scale_y_continuous scale_fill_manual scale_color_manual coord_flip theme unit
#' @export
#'
#' @examples
geompatternModServer <- function(
    id, 
    data, # data frame to pass to plotting function
    x,    # x-axis column
    y,    # y-axis column
    fill_col, # column to group data             
    legend_title = "", # title of legend
    geom_plot_type = c("bar", "box"), # geom type
    xlab = "", # required x-axis label
    ylab = "", # required y-axis label
    title = "", # optional plot title
    caption = "", # optional caption
    subtitle = "", # optional subtitle
    pattern_map, # pattern map (from pattern_mapping())
    color_map,  # color map (from color_mapping())
    breaks,  # a vector of breaks to map onto map pattern and colors map vectors (should be same length as pattern_map and color_map)
    second_axis = FALSE, # display second identical y-axis
    coord_flip = FALSE,  # flip figure
    bar_stack = FALSE, # should the bar plot be stacked or dodged?
    sort_x_by, # optional vector to sort the x-axis (should be the same length as x-axis column)
    ... # any argument passed to geom_bar_pattern or geom_boxplot_pattern
) {
  assertthat::assert_that(is.reactive(data), msg = paste0("`data` argument must be reactive.", 
                                                          "Wrap in `reactive({})` within function", "call."))
  shiny::moduleServer(id, {
    function(input, output, session) {
      output$geom_pattern <- shiny::renderPlot({
        if (is.null(sort_x_by)) {
          plot <- ggplot2::ggplot(data = data(),
                                  ggplot2::aes(x = !!dplyr::sym(x),
                                               y = !!dplyr::sym(y),
                                               fill = !!dplyr::sym(fill_col),
                                               pattern = !!dplyr::sym(fill_col)))
        } else {
          plot <- ggplot2::ggplot(data = data(),
                                  ggplot2::aes(x = factor(!!dplyr::sym(x), levels = sort_x_by),
                                               y = !!dplyr::sym(y),
                                               fill = !!dplyr::sym(fill_col),
                                               pattern = !!dplyr::sym(fill_col)))
        }
        if (is.null(title) | is.null(caption) | is.null(subtitle)) {
          plot <- plot +
            ggplot2::labs(x = xlab, y = ylab) +
            theme_sonoma()
        } else {
          plot <- plot +
            ggplot2::labs(x = xlab, y = ylab, title = title, caption = caption, subtitle = subtitle) +
            theme_sonoma()
        }
        if (geom_plot_type == "bar") {
          if (bar_stack == FALSE) {
            plot <- plot + ggpattern::geom_bar_pattern(pattern_density = 0.45,
                                                       pattern_spacing = 0.025,
                                                       pattern_key_scale_factor = 0.8,
                                                       color = "black",
                                                       pattern_fill = "white",
                                                       position = ggplot2::position_dodge2(preserve = "total", padding = 0.05),
                                                       ...)
          } else {
            plot <- plot + ggpattern::geom_bar_pattern(pattern_density = 0.45,
                                                       pattern_spacing = 0.025,
                                                       pattern_key_scale_factor = 0.8,
                                                       color = "black",
                                                       pattern_fill = "white",
                                                       position = "stack",
                                                       ...)
          }
        } else if (geom_plot_type == "box") {
          plot <- plot + ggpattern::geom_boxplot_pattern(pattern_density = 0.45,
                                                         pattern_spacing = 0.025,
                                                         pattern_key_scale_factor = 0.8,
                                                         color = "black",
                                                         pattern_fill = "white",
                                                         ...)
        }
        plot <- plot +
          ggplot2::scale_fill_manual(legend_title, values = color_map, breaks = breaks) +
          ggpattern::scale_pattern_manual(legend_title, values = pattern_map, breaks = breaks) +
          ggplot2::guides(pattern = ggplot2::guide_legend(),
                          fill = ggplot2::guide_legend())
        if (coord_flip == TRUE) {
          plot <- plot + ggplot2::coord_flip()
        }
        if (second_axis == TRUE) {
          plot <- plot + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~., name = derive(), breaks = derive(), labels = derive(), guide = derive()))
        }
        plot + theme(legend.key.size = unit(1.25, 'cm'))
      })
    }
  })
}

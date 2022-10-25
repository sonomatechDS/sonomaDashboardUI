#' STi ggplot discrete color scheme
#'
#' Color palette defined in sonoma_color_palettea(). Applies to 'color' aesthetic
#' in ggplot object.
#'
#' @param reverse boolean If TRUE, reverses order of color palette
#'
#' @return
#' @importFrom ggplot2 discrete_scale
#' @export
#'
#' @examples
scale_color_sonoma <- function(reverse = FALSE) {
  ggplot2::discrete_scale(aesthetics = 'color',
                          scale_name = 'sonoma_discrete',
                          palette = sonoma_color_palette)
}

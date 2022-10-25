#' STi ggplot theme
#'
#' Centered & bold title, 14 base font size, white background,
#' major and minor gridlines, legend on right, black plot border
#'
#' @return
#' @importFrom ggplot2 theme theme_bw "%+replace%"
#' @export
#'
#' @examples
theme_sonoma <- function() {
  ggplot2::theme_bw() %+replace%
    theme(text = ggplot2::element_text(size = 18),
          plot.title = ggplot2::element_text(hjust = 0.5,
                                    face = 'bold',
                                    margin = ggplot2::margin(0,0,0.2,0,'cm')),
          legend.position = 'right',
          legend.box = NULL,
          legend.title=ggplot2::element_text(size=16),
          legend.text=ggplot2::element_text(size=16),
          axis.text = ggplot2::element_text(size = 16),
          legend.margin = ggplot2::margin(0.1,0.1,0.1,0.1, 'cm'),
          plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
          panel.border = ggplot2::element_rect(size = 0.1,
                                      color = 'black',
                                      fill = NA)
    )
}

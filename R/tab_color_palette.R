#' STi tab color palette
#'
#' @param n integer (optional) An integer (1-3) specifying
#'                            the number of colors in the palette to return.
#' @param a double (optional) An alpha value (0-1) to adjust the opacity of the
#'                            color palette
#' @param cb boolean (optional) Default = False, whether or not the palette
#'                   should be colorblind friendly.
#' @param font boolean (optional) Default = False, whether or not to return the
#'                  font color for the relevant tab colors (ordered).
#' @param show boolean (optional, defalut = FALSE) If TRUE, outputs color
#'                     palette in plots tab
#'
#' @return string list
#' @importFrom grDevices rgb
#' @importFrom scales show_col
#' @export
#'
#' @examples
#' sonoma_color_palette(3)
tab_color_palette <- function(n, a=1, cb = F, font = F, show = F) {
  # Check parameters
  assertthat::assert_that((a >= 0 & a <= 1), msg = 'a must be between 0 and 1 (inclusive).')
  assertthat::assert_that((n < 4), msg = 'Max of 3 tab colors.')
  if (cb) {
      if (!font) {
        colors <- list('pink' = grDevices::rgb(216, 27, 96,
                                               maxColorValue=255, alpha = a*255),
                       'purple' = grDevices::rgb(3, 4, 94,
                                                maxColorValue=255, alpha = a*255),
                       'yellow' = grDevices::rgb(255, 193, 7,
                                                 maxColorValue=255, alpha = a*255))
      } else {

      colors <- list('white' = grDevices::rgb(255, 255, 255,
                                             maxColorValue=255, alpha = a*255),
                    'white' = grDevices::rgb(255, 255, 255,
                                             maxColorValue=255, alpha = a*255),
                     'black' = grDevices::rgb(0, 0, 0,
                                               maxColorValue=255, alpha = a*255))
      }
  } else {
    if (!font) {
      colors <- list('rose' = grDevices::rgb(160, 26, 88,
                                             maxColorValue=255, alpha = a*255),
                     'purple' = grDevices::rgb(105, 80, 176,
                                               maxColorValue=255, alpha = a*255),
                     'teal' = grDevices::rgb(64, 127, 144,
                                               maxColorValue=255, alpha = a*255))
    } else {

      colors <- list('white' = grDevices::rgb(255, 255, 255,
                                            maxColorValue=255, alpha = a*255),
                    'white' = grDevices::rgb(255, 255, 255,
                                             maxColorValue=255, alpha = a*255),
                    'white' = grDevices::rgb(255, 255, 255,
                                             maxColorValue=255, alpha = a*255))
    }
  }

  if (show) {
    scales::show_col(unlist(colors[1:n],use.names = FALSE))
  }
  return(unlist(colors[1:n], use.names = FALSE))
}

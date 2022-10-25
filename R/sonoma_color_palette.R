#' STi graphics colorblind palette
#'
#' @param c integer (optional) An integer (1-9) specifying
#'                            the number of colors in the palette to return.
#' @param a double (optional) An alpha value (0-1) to adjust the opacity of the
#'                            color palette
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
sonoma_color_palette <- function(c, a = 1, show = FALSE) {
  # Check parameters
  assertthat::assert_that((a >= 0 & a <= 1), msg = 'a must be between 0 and 1 (inclusive).')

  colors <- list('blue' = grDevices::rgb(0, 114, 178,
                                         maxColorValue=255, alpha = a*255),
                 'green' = grDevices::rgb(0, 158, 115,
                                          maxColorValue=255, alpha = a*255),
                 'orange' = grDevices::rgb(230, 159, 0,
                                           maxColorValue=255, alpha = a*255),
                 'purple' = grDevices::rgb(204, 121, 167,
                                           maxColorValue=255, alpha = a*255),
                 'red' = grDevices::rgb(213, 94, 0,
                                        maxColorValue=255, alpha = a*255),
                 'skyblue' = grDevices::rgb(86, 180, 223,
                                            maxColorValue=255, alpha = a*255),
                 'yellow' = grDevices::rgb(240, 228, 66,
                                           maxColorValue=255, alpha = a*255),
                 'gray' = grDevices::rgb(153, 153, 153,
                                         maxColorValue=255, alpha = a*255),
                 'black' = grDevices::rgb(0, 0, 0,
                                          maxColorValue=255, alpha = a*255))
  if (show) {
    scales::show_col(unlist(colors[1:c],use.names = FALSE))
  }
  return(unlist(colors[1:c], use.names = FALSE))
}

# sonoma_color_palette <- function(c, a = 1, show = FALSE) {
#   colors <- list('blue' = grDevices::rgb(64, 129, 189,
#                                          maxColorValue=255, alpha = a*255),
#                  'green' = grDevices::rgb(123, 187, 89,
#                                           maxColorValue=255, alpha = a*255),
#                  'orange' = grDevices::rgb(247, 150, 70,
#                                            maxColorValue=255, alpha = a*255),
#                  'purple' = grDevices::rgb(128, 100, 162,
#                                            maxColorValue=255, alpha = a*255),
#                  'red' = grDevices::rgb(192, 80, 77,
#                                         maxColorValue=255, alpha = a*255),
#                  'aqua' = grDevices::rgb(75, 172, 198,
#                                          maxColorValue=255, alpha = a*255),
#                  'yellow' = grDevices::rgb(255, 214, 41,
#                                            maxColorValue=255, alpha = a*255),
#                  'navy' = grDevices::rgb(31, 73, 125,
#                                          maxColorValue=255, alpha = a*255),
#                  'gray' = grDevices::rgb(191, 191, 191,
#                                          maxColorValue=255, alpha = a*255))
#   if (show) {
#     scales::show_col(unlist(colors[1:c],use.names = FALSE))
#   }
#   return(unlist(colors[1:c], use.names = FALSE))
# }

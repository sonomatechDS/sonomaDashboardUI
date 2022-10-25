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
sonoma_cb_palette <- function(c, a = 1, show = FALSE) {
  # Check parameters
  assertthat::assert_that((a >= 0 & a <= 1), msg = 'a must be between 0 and 1 (inclusive).')

  colors <- list('cyan' = grDevices::rgb(136, 204, 238,
                                         maxColorValue=255, alpha = a*255),
                 'green' = grDevices::rgb(17, 119, 51,
                                          maxColorValue=255, alpha = a*255),
                 'sand' = grDevices::rgb(221, 204, 119,
                                           maxColorValue=255, alpha = a*255),
                 'wine' = grDevices::rgb(136, 34, 85,
                                           maxColorValue=255, alpha = a*255),
                 'indigo' = grDevices::rgb(51, 34, 136,
                                        maxColorValue=255, alpha = a*255),
                 'teal' = grDevices::rgb(68, 170, 153,
                                            maxColorValue=255, alpha = a*255),
                 'olive' = grDevices::rgb(153, 153, 51,
                                           maxColorValue=255, alpha = a*255),
                 'rose' = grDevices::rgb(204, 102, 119,
                                         maxColorValue=255, alpha = a*255),
                 'purple' = grDevices::rgb(170, 68, 153,
                                          maxColorValue=255, alpha = a*255),
                 'gray' = grDevices::rgb(221, 221, 221,
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

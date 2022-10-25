#' STi graphics colorblind palette for tables
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
#' sonoma_table_cb_palette(3)
sonoma_table_cb_palette <- function(c, a = 1, show = FALSE) {
  assertthat::assert_that((a >= 0 & a <= 1), msg = 'a must be between 0 and 1 (inclusive).')

  colors <- list('pale cyan' = grDevices::rgb(204, 238, 255,
                                          maxColorValue=255, alpha = a*255),
                 'pale green' = grDevices::rgb(204, 221, 170,
                                           maxColorValue=255, alpha = a*255),
                 'pale yellow' = grDevices::rgb(238, 238, 187,
                                           maxColorValue=255, alpha = a*255),
                 'pale red' = grDevices::rgb(255, 204, 204,
                                        maxColorValue=255, alpha = a*255),
                 'pale grey' = grDevices::rgb(221, 221, 221,
                                            maxColorValue=255, alpha = a*255),
                 'pale blue' = grDevices::rgb(187, 204, 238,
                                              maxColorValue=255, alpha = a*255),
                 'white' = grDevices::rgb(0, 0, 0,
                                          maxColorValue=255, alpha = a*255))
  if (show) {
    scales::show_col(unlist(colors[1:c],use.names = FALSE))
  }
  if (c > 7) {
    r_col <- unlist(c(colors, colors[1:c-7]), use.names = F)
  } else {
    r_col <- unlist(colors[1:c], use.names = FALSE)
  }

  return(r_col)
}

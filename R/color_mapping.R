#' Create color map
#'
#' @param num_col integer, number of unique values to assign a color to
#' @param exclude_achromatic boolean, exclude black and gray colors?
#'
#' @return
#' @importFrom dplyr between
#' @export
#'
#' @examples
color_mapping <- function(num_col, # number of unique patterns
         exclude_achromatic = FALSE # exclude black and grey colors
         ) {
  if (exclude_achromatic == FALSE) {
    if (num_col <= 9) {
      pat_col_map <- sonoma_color_palette(num_col)
    } else if (between(num_col, 10, 18)) {
      pat_col_map <-
        c(sonoma_color_palette(9),
          sonoma_color_palette(num_col - 9))
    } else if (num_col > 18) {
      pat_col_map <-
        c(rep(sonoma_color_palette(9), 2),
          sonoma_color_palette(num_col - 18))
    }
  } else {
    if (num_col <= 7) {
      pat_col_map <- sonoma_color_palette(num_col)
    } else if (between(num_col, 7, 14)) {
      pat_col_map <-
        c(sonoma_color_palette(7),
          sonoma_color_palette(num_col - 7))
    } else if (num_col > 14) {
      pat_col_map <-
        c(rep(sonoma_color_palette(7), 2),
          sonoma_color_palette(num_col - 14))
    }
  }
  return(pat_col_map)
}

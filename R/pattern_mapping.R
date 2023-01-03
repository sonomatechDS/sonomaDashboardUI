#' Create pattern map
#'
#' @param num_col integer, number of unique values to assign a pattern to
#' @param exclude_achromatic boolean, exclude black and gray colors?
#'
#' @return
#' @importFrom dplyr between
#' @export
#'
#' @examples
pattern_mapping <- function(num_col, # number of unique patterns
         exclude_achromatic = FALSE # exclude black and grey colors
         ) {
  if (exclude_achromatic == FALSE) {
    if (num_col <= 9) {
      pat_map <- rep('none', num_col)
    } else if (between(num_col, 10, 18)) {
      pat_map <- c(rep('none', 9), rep('stripe', num_col - 9))
    } else if (num_col > 18) {
      pat_map <-
        c(rep('none', 9),
          rep('stripe', 9),
          rep('circle', num_col - 18))
    }
  } else {
    if (num_col <= 7) {
      pat_map <- rep('none', num_col)
    } else if (between(num_col, 7, 14)) {
      pat_map <- c(rep('none', 7), rep('stripe', num_col - 7))
    } else if (num_col > 14) {
      pat_map <-
        c(rep('none', 7),
          rep('stripe', 7),
          rep('circle', num_col - 14))
    }
  }
  return(pat_map)
}

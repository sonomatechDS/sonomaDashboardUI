#' Convert units to formatted expression
#'
#' @param parameter character vector A string or vector of parameter names to be
#'                                   converted
#'
#' @return character vector Formatted expressions
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples
param_to_expression <- function(parameter) {
  parameter <- trimws(parameter, which = 'both')
  parameter <- paste0('`', parameter, '`')
  # parameter <- stringr::str_replace_all(parameter, '\\[', '\\*`\\[')
  # parameter <- stringr::str_replace_all(parameter, ']', ']`\\*')
  # parameter <- gsub('^((\\d,)+\\d)', "`\\1`\\*", parameter, perl=T)
  # parameter <- gsub('(?<=[a-z]\\-)((\\d,)+\\d)', "`\\1`", parameter, perl=T)
  # parameter <- gsub('[a-zA-Z](\\()', "\\*\\1", parameter, perl=T)
  # parameter <- gsub('(\\))[a-zA-Z]', "\\1\\*", parameter, perl=T)
  # parameter <- stringr::str_replace_all(parameter, 'Benzene\\,', '`Benzene,`*')
  # #parameter <- gsub('(?<!`\\[(\\d,)+)((\\d,)+\\d)', "`\\1`\\*", parameter, perl=T)
  # parameter <- stringr::str_replace_all(parameter, '`PM2.5', 'PM[2.5]~`')
  parameter <- gsub('`PM2.5','PM[2.5]*`', parameter, perl=T)
  parameter <- gsub(' PM2.5 ','`~PM[2.5]~`', parameter, perl=T)
  parameter <- gsub('`NOy','NO[y]*`', parameter, perl=T)
  parameter <- gsub(' NOy ','`~NO[y]~`', parameter, perl=T)
  parameter <- gsub(' NOz ','`~NO[z]~`', parameter, perl=T)
  parameter <- gsub('`NOz`', 'NO[z]', parameter, perl=T)
  parameter <- gsub('`NOz','NO[z]*`', parameter, perl=T)
  parameter <- gsub(' NOx ','`~NO[x]~`', parameter, perl=T)
  parameter <- gsub('`NOx','NO[x]*`', parameter, perl=T)
  parameter <- gsub('`NO2','NO[2]*`', parameter, perl=T)
  parameter <- gsub(' NO2 ','`~NO[2]~`', parameter, perl=T)
  # parameter <- stringr::str_replace_all(parameter, 'NOy', '`*NO[y]*`')
  # parameter <- stringr::str_replace_all(parameter, 'NOx', '`~NO[x]~`')
  # parameter <- stringr::str_replace_all(parameter, 'NOz', '`~NO[z]~`')
  # parameter <- stringr::str_replace_all(parameter, 'NO2', '`~NO[2]~`')
  parameter <- stringr::str_replace_all(parameter, '`PM10-2.5', 'PM[10-2.5]*`')
  #parameter <- stringr::str_replace_all(parameter, '`PM10-2.5', 'PM[10-2.5]~`')
  parameter <- stringr::str_replace_all(parameter, '`PM10', 'PM[10]*`')
  parameter <- stringr::str_replace_all(parameter, ' PM10 ', '`~PM[10]~`')
  #parameter <- gsub("(?<=[0-9])([a-zA-Z])", " \\1", parameter, perl = TRUE)
  parameter <- gsub('(?<=\\d)um', '`~mu*m*`',parameter, perl=T)
  parameter <- stringr::str_replace_all(parameter, '&', 'and')


  return(parameter)
}

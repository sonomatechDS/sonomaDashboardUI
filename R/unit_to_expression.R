#' Convert units to formatted expression
#'
#' @param unit character vector A string or vector of unit values to be
#'                              converted
#'
#' @return character vector Formatted expressions
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples
unit_to_expression <- function(unit) {
  unit <- dplyr::case_when(#unit == 'Parts per million' ~ 'ppm',
                           unit == "Micrograms/cubic meter (LC)" ~ 'mu*g/m^3~(LC)',
                           unit == "Millibars" ~ 'mb',
                           unit == "Micrograms/cubic meter (25 C)" ~ 'mu*g/m^3~(25~degree*C)',
                           #unit == "Parts per billion" ~ 'ppb',
                           unit == "Degrees Fahrenheit" ~ '~degree*F',
                           unit == "Degrees Celsius" ~ '~degree*C',
                           unit == "Percent relative humidity"~ '%~RH',
                           unit == "Langleys/minute" ~ "Ly/min",
                           TRUE ~ unit)
  unit <- trimws(unit, which = 'both')
  unit <- stringr::str_replace_all(unit, '0m', '0 m')
  unit <- stringr::str_replace_all(unit, 'Parts per billion', 'ppb')
  unit <- stringr::str_replace_all(unit, 'Parts per million', 'ppm')
  unit <- stringr::str_replace_all(unit, 'Parts per 100 million', 'pp100m')
  unit <- stringr::str_replace_all(unit, 'Parts per ten million', 'pp10m')
  unit <- stringr::str_replace_all(unit, '[Cc]ubic feet', 'ft^3')
  unit <- stringr::str_replace_all(unit, '[Cc]entigrade', 'degree*C')
  unit <- stringr::str_replace_all(unit, '[Cc]entimeters?', 'cm')
  unit <- stringr::str_replace_all(unit, '[Mm]eters?', 'm')
  unit <- stringr::str_replace_all(unit, '[Mm]illimeters?', 'mm')
  unit <- stringr::str_replace_all(unit, '[Ss]econds?', 's')
  unit <- stringr::str_replace_all(unit, '[Ii]nche?s?', '`in`')
  unit <- stringr::str_replace_all(unit, ' in ', '~`in`~')
  unit <- stringr::str_replace_all(unit, '1C', '1~degree*C')
  unit <- stringr::str_replace_all(unit, ' C$', '~degree*C')
  unit <- stringr::str_replace_all(unit, 'square m', 'm^2')
  unit <- stringr::str_replace_all(unit, 'sq m(?![a-zA-Z])', 'm^2')
  unit <- stringr::str_replace_all(unit, 'square cm', 'cm^2')
  unit <- stringr::str_replace_all(unit, 'sq cm', 'cm^2')
  unit <- stringr::str_replace_all(unit, 'cubic cm', 'cm^2')
  unit <- stringr::str_replace_all(unit, 'sq `in`', '`in`^2')
  #unit <- stringr::str_replace_all(unit, '/ m', '/m')
  unit <- stringr::str_replace_all(unit, '%', '`%`')
  unit <- stringr::str_replace_all(unit, ' ', '~')
  unit <- stringr::str_replace_all(unit, ',', '')

  return(unit)
}

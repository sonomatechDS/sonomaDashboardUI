#' Date of Last Update Footer
#'
#' Paste at bottom of UI to add a note listing the most recent deployment date
#'
#' @param dt_df data.frame Must contain two columns, `weekly` and `quarterly`,
#'                         with the most recent date of data update per
#'                         download cadence in the first row.
#'
#' @return
#' @importFrom shiny fluidRow column tagList
#' @importFrom htmltools tags div
#' @importFrom lubridate year
#' @export
#'
#' @examples
footerDateofDataUpdateUI <- function(dt_df) {

  curr_year <- lubridate::year(Sys.Date())
  weekly_yr <- lubridate::year(Sys.Date()) - 2
  qly_yr <- 2016

  weekly_dt_update <- dt_df$weekly[1]
  qly_dt_update <- dt_df$quarterly[1]


  t <- shiny::tagList(
    htmltools::div(style= 'padding-top: 50px',
    shiny::fluidRow(shiny::column(10, offset=1, align = 'center',
                                htmltools::tags$em(paste("Data from", weekly_yr, '-', curr_year, 'last updated', weekly_dt_update)))
  ),
  shiny::fluidRow(shiny::column(10, offset=1, align = 'center',
                                htmltools::tags$em(paste("Data from", qly_yr, '-', weekly_yr-1 , 'last updated', qly_dt_update)))
  )))
  return(t)
}

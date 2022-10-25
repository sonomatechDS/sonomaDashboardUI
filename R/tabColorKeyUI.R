#' Tab color key UI
#'
#' Creates key for tab colors that updates depending on coloblind accessible
#' selection.
#'
#' @param id string Links call to tabColorKeyUI to tabColorKeyServer function
#'
#' @return
#' @export
#'
#' @examples
tabColorKeyUI <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns('colorKey'))
}

#' Leaflet Map UI Function
#'
#' Used alongside sitemapServer() to add a map of aqs sites (wrapped in a fluidRow)
#' to an app. Map will include tooltip with aqs site code, and optionally a
#' radiobutton filter of sites and/or legend with site categories.
#'
#' @param id string ID label that links sitemapUI() to sitemapServer()
#' @param filter_col string (optional, default = NULL) Column to filter sites
#'                          shown on map by. Should only have 2 values. If specified,
#'                          fluidRow with radiobuttons labeled by `filter_labels`
#'                          will be added above map. If specified, `filter_labels`
#'                          must also be specified.
#' @param filter_labels character vector (optional, default = NULL) Must be
#'                                specified if `filter_col` is filled. Labels applied to
#'                                radio buttons that specify sites to be displayed
#'                                on map. Initial value is 1st label in vector.
#'                                Should be length 2.
#' @param top_message string Text to display above map. Default message is
#'                           "Click a circle on the map to select a site". If no
#'                           message desired, input an empty string.
#'
#' @return
#' @importFrom shiny fluidRow column radioButtons tagList
#' @importFrom htmltools div
#' @importFrom leaflet leafletOutput
#' @importFrom magrittr `%>%`
#' @importFrom shinycssloaders withSpinner
#' @export
#'
#' @examples
sitemapUI <- function(id,
                      filter_col = NULL,
                      filter_labels = NULL,
                      top_message = "Click a circle on the map to select a site") {
  ns <- shiny::NS(id)

  if (!is.null(filter_col)) {
    row1 <- shiny::fluidRow(
      shiny::column(5, offset = 1,
                    shiny::fluidRow(
                      htmltools::div(
                        style = paste0(
                          'padding-top:50px; ',
                          'padding-left:20px; ',
                          'font-weight:bold;'
                        ),
                        top_message
                      )
                    )),
      shiny::column(
        5,
        align = 'right',
        htmltools::div(
          style = 'padding-top:15px',
          shiny::radioButtons(
            inputId = ns("MapFilter"),
            label = "Choose sites to display on map",
            inline = TRUE,
            choiceNames = c(filter_labels[1],
                            filter_labels[2]),
            choiceValues = c('MapFilter1',
                             'MapFilter2'),
            selected = 'MapFilter1'
          )
        )
      )
    )
  } else {
    row1 <- shiny::fluidRow("  ")
  }

  shiny::tagList(row1,
                 shiny::fluidRow(
                   shiny::column(
                     10,
                     offset = 1,
                     leaflet::leafletOutput(outputId = ns('Map'),
                                            width = "100%") %>%
                       shinycssloaders::withSpinner(),
                   )
                 ))

}

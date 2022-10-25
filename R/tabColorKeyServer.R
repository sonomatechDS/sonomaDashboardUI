#' Tab color key
#'
#' Creates key for tab colors that updates depending on coloblind accessible
#' selection.
#'
#' @param id string ID to link server to UI
#' @param n integer Number of unique tab colors
#' @param labels character vector Labels to give to each respective tab color
#' @param cb boolean If T, colorblind accessible palette is used
#'
#' @return
#' @importFrom shiny moduleServer renderUI fluidRow tagList column tagAppendChild
#' @importFrom htmltools div
#' @export
#'
#' @examples
tabColorKeyServer <- function(id, n, labels, cb = reactive({FALSE})) {
  shiny::moduleServer(id,
                      function(input, output, session) {

    output$colorKey <- shiny::renderUI({
      key_title <- htmltools::div('Tab Color Key:',
         style = 'font-weight: bold; padding-left:5px; text-align:left !important;')

      boxes <- shiny::tagList()
      for (i in 1:n) {
        new_box <- shiny::fluidRow(
          shiny::column(8, align = 'left', offset = 1,
                 labels[i]),
          shiny::column(2, align = 'left',
                 htmltools::div(style = paste0('width:10px; ',
                                               'height:10px; ',
                                               'outline:1px solid black; ',
                                               'background:',
                                               tab_color_palette(n=i, cb=cb())[i],
                                               '; ',
                                               'margin-top:4px; ',
                                               'margin-right:7px'))))
        boxes <- shiny::tagAppendChild(boxes, new_box)
      }
      color_key <- shiny::tagList(
        htmltools::div(style='width:150px; outline:1px solid black; margin-bottom:10px',
                       htmltools::div(style = 'font-weight: bold; padding-left:5px; text-align:left !important;',
                                      'Tab Color Key:'),
                       htmltools::div(style='width:150px; outline:1px solid black;',
                                      boxes
                       )))
      color_key
      })
  })
}

#' Header table for selected map site
#'
#' Displays sspecified columns from data passed to sitemapServer function
#' for site clicked on map. Must be set to output element with a unique ID,
#' with a call to mapSelectHeaderUI specifying the same ID.
#' e.g. output$header <- mapSelectHeader()
#'
#' @param data_row reactive Row of data to be displayed, e.g. the $sitedata
#'                          output of sitemapServer()
#' @param select_cols character vector Columns to display in table
#' @param col_labels character vector Column labels to display in table
#'
#' @return function Call to DT::renderDataTable
#' @importFrom DT datatable renderDataTable JS
#' @importFrom dplyr select
#' @export
#'
#' @examples
mapSelectHeader <- function(id,
                            data_row,
                            select_cols,
                            col_labels) {
  shiny::moduleServer(id, {
    function(input, output, session) {

      req(data_row())

      callback <- "$('table.dataTable.display tbody tr:odd').css('background-color', '#F8F8F8');"


      output$mapHeaderTable <- DT::renderDataTable({
        data_row() %>%
          dplyr::select(all_of(select_cols)) %>%
          DT::datatable(options = list(dom='t',
                                       ordering=F),
                        colnames = col_labels,
                        rownames=FALSE,
                        class = list(stripe = FALSE),
                        callback = DT::JS(callback))})
    }
    })
}

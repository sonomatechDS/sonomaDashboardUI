#' #' Data Table Server Function
#' #'
#' #' Use alongside tableUI() to add (optionally) interactive
#' #' data table to app.
#' #'
#' #' @param id string ID label that links tablueUI() to tableServer()
#' #' @param input_data data.frame Data to display in table
#' #' @param page_length integer (optional, default = 5) Number of items to display
#' #'                            at once in table (if < nrow(input_data), table
#' #'                            is paginated)
#' #' @param select_cols character (optional, default = NULL) Vector of columns
#' #'                              to including in data table.
#' #' @param col_label_names character (optional, default = NULL) Vector of column
#' #'                                  header names to appear in data frame.
#' #'                                  Must match number of columns in input_data
#' #'                                  or `select_cols`` (if specified).
#' #' @param filter_col string (optional, default = NULL) Name of column to apply
#' #'                          reactive filter to...reactive filter value passed
#' #'                          in `filter_aqs_rct`. If specified, filter_rct must
#' #'                          be supplied. If not specified, table has no
#' #'                          reactive filtering.
#' #' @param filter_rct reactive (optional, default = NULL) Reactive value
#' #'                                to filter `filter_col` by. If specified,
#' #'                                filter_col must be supplied.If not specified,
#' #'                                table has no reactive filtering.
#' #' @param caption string (optional, default = NULL) Caption for data table.
#' #' @param pdf_button boolean (optional, default = TRUE) If true, "download
#' #'                            table" button added.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' tableServer <- function(id,
#'                         input_data,
#'                         page_length = 5,
#'                         select_cols = NULL,
#'                         col_label_names = NULL,
#'                         arrange_col = NULL,
#'                         filter_col = NULL,
#'                         filter_rct = NULL,
#'                         caption = NULL,
#'                         pdf_button = TRUE) {
#'   moduleServer(id, {
#'     function(input, output, session) {
#'
#'       if (is.null(filter_aqs_rct)) {
#'         filter_aqs_rct <- reactive({NULL})
#'       }
#'
#'       if (is.null(select_cols)) {
#'         select_cols <- colnames(input_data)
#'       }
#'
#'       if (pdf_button) {
#'         options <- list(
#'           pageLength = 5,
#'           autoWidth = FALSE,
#'           "dom" = 'T<"clear">lBfrtip',
#'           buttons = list(
#'             list(
#'               extend = 'pdf',
#'               orientation = 'landscape',
#'               pageSize = 'LEGAL',
#'               text = 'Download Table'
#'             )
#'           ),
#'           initComplete = JS(
#'             paste0(
#'               "function(settings, json)",
#'               " {$(this.api().table().header",
#'               "()).css({'background-color': ",
#'               "'#407EC9', 'color': '#fff', ",
#'               "'word-wrap': 'break-word'});}"
#'             )
#'           )
#'         )
#'       } else {
#'         options <- list()
#'       }
#'
#'       input_data <- input_data %>%
#'         select(select_cols) %>%
#'         arrange(arrange_col) %>%
#'         setNames(., col_label_names)
#'
#'       if(!is.null(filter_col)) {
#'         filter_col <- col_label_names[grep(filter_col, old_colnames)]
#'       }
#'
#'       data <- reactiveValues(df = input_data)
#'
#'       observeEvent(filter_aqs_rct(), {
#'         data$df <- input_data %>%
#'           dplyr::filter(!!sym(filter_col) == filter_aqs_rct())
#'       })
#'
#'
#'       output$Table <- DT::renderDataTable({
#'         data$df %>%
#'           datatable(
#'             rownames = FALSE,
#'             caption = caption,
#'             selection = list(mode = "single"),
#'             extensions = c("FixedColumns", "FixedHeader"),
#'             options = options
#'           )
#'       })
#'     }
#'   })
#' }

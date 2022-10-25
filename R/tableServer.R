#' Data Table Server Function
#'
# Use alongside tableUI() to add (optionally) interactive
# data table to app.
#'
#' @param id string ID label that links tablueUI() to tableServer()
#' @param input_data reactive Reactive dataframe
#' @param page_length integer (optional, default = 5) Number of items to display
#'                            at once in table (if < nrow(input_data), table
#'                            is paginated)
#' @param select_cols character (optional, default = NULL) Vector of columns
#'                              to including in data table.
#' @param col_label_names character (optional, default = NULL) Vector of column
#'                                  header names to appear in data frame.
#'                                  Must match number of columns in input_data
#'                                  or `select_cols`` (if specified).
#' @param arrange_col string (optional, default = 1st column) Column to arrange
#'                           table by.
#' @param filter_col string (optional, default = NULL) Name of column to apply
#'                          reactive filter to...reactive filter value passed
#'                          in `filter_rct`. If specified, filter_rct must
#'                          be supplied. If not specified, table has no
#'                          reactive filtering.
#' @param filter_rct reactive list (optional, default = NULL) Reactive list of values
#'                                to filter `filter_col` by. If specified,
#'                                filter_col must be supplied.If not specified,
#'                                table has no reactive filtering.
#' @param color_col string (optional, default = NULL) Column that determines row
#'                         colors. If not specified, columns will be alternating
#'                         grays.
#' @param color_order character vector (optional) A vector with the order of
#'                                     color values that appear in the legend.
#'                                     All values in color_col must be specified.
#'                                     1st specified will be blue, then green,
#'                                     the yellow, etc (according to sonoma_
#'                                     color_palette).
#' @param caption_rct reactive string (optional, default = NULL) Caption for data table.
#' @param pdf_button boolean (optional, default = TRUE) If true, "download
#'                            table" button added.
#' @param cb reactive (optional) If not NULL, colorblind palette will be applied
#' @param load_all_data boolean (optional) If TRUE (default), all data in table
#'                      will be loaded and available for download. If false,
#'                      only first page (visible) data is loaded and available
#'                      for download. Not recommended for large datasets.
#' @param download_filename string (optional) Defaults to 'data', the filename
#'                                 desired for downloaded table data.
#' @param dom_spec sting (optional) Default = 'Bfrtip', establishes built-in
#'                       table control elements. https://datatables.net/reference/option/dom
#'
#' @return
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive is.reactive
#' @importFrom DT JS datatable formatStyle styleEqual
#' @importFrom dplyr select arrange filter sym pull
#' @importFrom stats setNames
#' @importFrom magrittr `%>%`
#' @importFrom rlang `:=`
#' @export
#'
#' @examples
tableServer <- function(id,
                        input_data,
                        page_length = 5,
                        select_cols = NULL,
                        col_label_names = NULL,
                        arrange_col = NULL,
                        filter_col = NULL,
                        filter_rct = shiny::reactive({
                          list(NULL)
                        }),
                        color_col = NULL,
                        color_order = NULL,
                        caption_rct = reactive({NULL}),
                        pdf_button = TRUE,
                        cb = shiny::reactive({NULL}),
                        load_all_data = TRUE,
                        download_filename = 'data',
                        dom_spec = 'Bfrtip') {
  # Check parameters


  params <-
    list(
      id,
      input_data,
      page_length,
      select_cols,
      col_label_names,
      filter_col,
      filter_rct,
      caption_rct,
      pdf_button
    )

  classes <- purrr::map(params, class)
  rm(params)
  assertthat::assert_that(
    'reactive' %in% classes[[2]] | 'data.table' %in% classes[[2]],
    msg = paste0(
      '`input_data` should be reactive.',
      'Wrap in reactive({}) in function call.'
    )
  )
  assertthat::assert_that(classes[[3]] == 'numeric',
                          msg = paste0('`page_length` should be an integer.'))
  if (!is.null(filter_rct)) {
    assertthat::assert_that(shiny::is.reactive(filter_rct),
                            msg = '`filter_rct` must be a reactive value (e.g. reactive({value}).')
  }

  if (!is.null(color_col)) {
    assertthat::assert_that(
      color_col %in% select_cols,
      msg = paste0(
        'Specified `color_col` is not included',
        ' in `select_cols`. Add `color_col` to',
        ' list of selected columns.'
      )
    )
  }

  shiny::moduleServer(id, {
    function(input, output, session) {

      cols <- reactiveValues(select=select_cols,
                             labels=col_label_names,
                             arrange=arrange_col)

      color_vals <- reactiveValues(colors=NULL)

      observe({

          if (is.null(cols$select)) {
            cols$select <- colnames(input_data())
          }

        if (is.null(cols$labels)) {
          cols$labels <- cols$select
        }

        if (is.null(cols$arrange)) {
          cols$arrange <- cols$select[[1]]
        }

        if (!is.null(color_col)) {

          unique_colors <- input_data() %>% pull(!!sym(color_col)) %>% unique()

          if (is.null(color_order)) {
            colorGroups <- sort(unique_colors)
          } else {
            colorGroups <- color_order[color_order %in% unique_colors]
          }

          if (is.null(cb())) {
            pal <- setNames(sonoma_color_palette(length(
              colorGroups
            ), a = 0.5), colorGroups)
          } else {
            pal <- setNames(sonoma_table_cb_palette(length(
              colorGroups
            ), a = 0.5), colorGroups)
          }
          color_vals$colors <- pal
          }
      })

      data <- reactive({
        assertthat::assert_that(length(cols$select) == length(cols$labels),
                                msg = paste("`select_cols` and `col_label_names` must",
                                            "be the same length."))
          data <- input_data()

          if (any(!is.null(unlist(filter_rct())))) {
          assertthat::assert_that(!is.null(filter_col),
                                  msg = 'If `filter_rct` is specified, `filter_col` must also be specified.')
          assertthat::assert_that(is.list(filter_rct()),
                                  msg = '`filter_rct` must be a reactive list. Wrap in list() in argument call.')

          for (i in 1:length(filter_col)) {

            if (is.null(filter_rct()[[i]])) {
              next
            } else {

            data <- data %>%
              dplyr::filter(!!dplyr::sym(filter_col[i]) %in% filter_rct()[[i]])
             }
            }
          }

          data <- data %>%
            dplyr::select(all_of(cols$select)) %>%
            dplyr::arrange(!!sym(cols$arrange))
        return(data)
      })

      if (pdf_button) {
        options <- list(
          dom = dom_spec,
          autoWidth = FALSE,
          pageLength = page_length,
          buttons = list(
            list(
              extend = 'collection',
              buttons = list(
                list(extend = 'csv', filename = download_filename),
                list(extend = 'excel', filename = download_filename),
                list(extend = 'pdf', filename = download_filename, orientation = 'landscape', 'pageSize' = 'legal')),
              text = 'Download Table'
            )
          )
        )
      } else {
        options <- list(pageLength=page_length)
      }


      if (is.null(all_of(color_col))) {
        output$Table <- DT::renderDataTable(server = !load_all_data,
          {
          table <- data() %>%
            DT::datatable(
              class = "stripe row-border order-column",
              rownames = FALSE,
              escape = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                caption_rct()
              ),
              colnames = cols$labels,
              selection = list(mode = "single"),
              extensions = c("FixedColumns", "FixedHeader", "Buttons"),
              options = options
            )
            table
        })
      } else {

        output$Table <- DT::renderDataTable(server = !load_all_data,
          {
            req(color_vals$colors)
          table_data <- data() %>%
            mutate(!!color_col := as.factor(!!dplyr::sym(color_col)))

          colors_filtered <- levels(table_data[[color_col]])

          table <- table_data %>%
            arrange(factor(!!sym(color_col),
                           levels = color_order[color_order %in% colors_filtered]),
                    !!sym(cols$arrange)) %>%
            DT::datatable(
              class = "row-border order-column",
              rownames = FALSE,
              escape = FALSE,
              caption = caption_rct(),
              colnames = cols$labels,
              selection = list(mode = "single"),
              extensions = c("FixedColumns", "FixedHeader", "Buttons"),
              options = options
            ) %>%
            DT::formatStyle(
              columns = select_cols,
              valueColumns = color_col,
              backgroundColor = DT::styleEqual(levels = colors_filtered,
                                               values = color_vals$colors[colors_filtered])
            )
          table
        })
      }
    }
  })
}

#' Scatter plot Server
#'
#' Used alongside scatterUI() to add a scatter plot to app. Takes in a
#' long-formatted dataframe with a column for Parameter, Unit, and
#' Measurement. Spreads the dataframe by the parameter column
#' using `key_col` as the key (typically a timestamp).
#' Optionally accepts a brush (e.g. from a timeseries plot) and filters
#' the `key_col` accordingly. Optionally plots a 1-to-1 a-b line and a
#' linear regression line.
#'
#' @param id string ID that links scatterUI() to scatterServer()
#' @param data_param1_rct reactive Reactive dataframe with columns for parameter,
#'                              measurement and unit with a key used to join
#'                              data1 to data2. Should only contain data for
#'                              param_1_rct.
#'                              spreading the dataframe by parameter.
#' @param data_param2_rct reactive Reactive dataframe with columns for parameter,
#'                              measurement and unit with a key used to join
#'                              data1 to data2. Should only contain data for
#'                              param_2_rct.
#' @param param_col string Column name of column with parameter values
#' @param poc_col string Column name of column iwth poc number
#' @param measurement_col string Column name of column with measurement values
#' @param key_col string Column name of column with datetime values in posixct format.
#' @param unit_col string Column name of column with unit values
#' @param param_rct reactive Parameter value to plot on x-axis
#' @param sec_param_rct reactive Parameter value to plot on y-axis
#' @param brush reactive (optional) Brush object used to adjust displayed data.
#'                                  Must have elements named xmin, xmax,
#'                                  ymin, ymax.
#' @param legend_pos numeric vector (optional) Default = c(1,0), offset from
#'                                  upper-left corner that legend should be
#'                                  positioned in.
#'
#' @return
#' @importFrom shiny reactive moduleServer req reactiveValues observe renderPlot is.reactive validate need
#' @importFrom magrittr `%>%`
#' @importFrom ggplot2 geom_point ggplot aes labs
#' @importFrom dplyr select filter full_join mutate
#' @importFrom tidyr spread
#' @importFrom stats as.formula lm coef
#' @export
#'
#' @examples
scatterServer <- function(id,
                          data_param1_rct,
                          data_param2_rct,
                          # data_1hr_rct,
                          param_col,
                          measurement_col,
                          key_col,
                          unit_col,
                          poc_col,
                          param_rct,
                          sec_param_rct,
                          brush = shiny::reactive({NULL}),
                          legend_pos = c(0,1)
                          ) {

  assertthat::assert_that(shiny::is.reactive(data_param1_rct),
                          msg = paste('Input data must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(data_param2_rct),
                          msg = paste('Input data must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(param_rct),
                          msg = paste('`param_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  assertthat::assert_that(shiny::is.reactive(sec_param_rct),
                          msg = paste('`sec_param_rct` must be reactive. Wrap in',
                                      'shiny::reactive({}) in function call.'))
  shiny::moduleServer(id, {
    function(input, output, session) {
      units_pocs <- shiny::reactiveValues(poc1 = 1, poc2 = 1,
                                          unit1=NULL,unit2=NULL)

      observe({
        req(sec_param_rct())
        if (param_rct() == sec_param_rct()) {
          shinyjs::showElement('sameParams')
          shinyjs::hideElement('diffParams')
        } else {
          shinyjs::hideElement('sameParams')
          shinyjs::showElement('diffParams')
        }
      })


      data <- shiny::reactive({
        shiny::req(class(sec_param_rct())=='character')

        shiny::req(is.data.frame(data_param1_rct()),
                   is.data.frame(data_param2_rct()),
                   nrow(data_param1_rct()) > 0,
                   nrow(data_param2_rct()) > 0)
        shiny::req(sec_param_rct() != "")

        # observe({
        #   if (param_rct() == sec_param_rct()) {
        #     shinyjs::showElement('sameParams')
        #     shinyjs::hideElement('diffParams')
        #   } else {
        #     shinyjs::hideElement('sameParams')
        #     shinyjs::showElement('diffParams')
        #   }
        # })

        data1 <- data_param1_rct() %>%
          dplyr::select(param_col, measurement_col, poc_col, key_col) %>%
          dplyr::mutate(poc_col = as.factor(!!sym(poc_col)))


        data2 <- data_param2_rct() %>%
          dplyr::select(param_col, measurement_col, poc_col, key_col) %>%
          mutate(poc_col = as.factor(!!sym(poc_col)))
#
#
#         units_pocs$poc1 <- length(unique(data1$poc))
#         units_pocs$poc2 <- length(unique(data2$poc))

        data <- dplyr::full_join(data1, data2, by = key_col) %>%
          filter(!is.na(poc_col.x),
                 !is.na(poc_col.y))
        return(data)
      })



      shiny::observe({
        shiny::req(is.data.frame(data_param1_rct()),
                   is.data.frame(data_param2_rct()),
                   sec_param_rct())

        units_pocs$unit1 <- data_param1_rct() %>%
          dplyr::filter(!!sym(param_col)== param_rct()) %>%
          dplyr::select(unit_col) %>%
          unique() %>%
          dplyr::pull()

        units_pocs$unit2 <- data_param2_rct() %>%
          dplyr::filter(!!sym(param_col)== sec_param_rct()) %>%
          dplyr::select(unit_col) %>%
          unique() %>%
          dplyr::pull()
      })


      brush_range <-
        shiny::reactiveValues(x = NULL, y = NULL)

      shiny::observe({
        brush_vals <- brush()

        if (!is.null(brush_vals)) {
          brush_range$x <- as.POSIXct(c(brush_vals$xmin, brush_vals$xmax), origin = '1970-01-01')
          brush_range$y <-
            c(brush_vals$ymin, brush_vals$ymax)
        } else {
          brush_range$x <- NULL
          brush_range$y <- NULL
        }
      })

      shiny::observeEvent(data_param1_rct(), {
        brush_range$x <- NULL
        brush_range$y <- NULL
      })

      output$Scatter <- shiny::renderPlot({

        shiny::req(is.data.frame(data()))
        shiny::req(is.character(sec_param_rct()))

        if (is.null(brush_range$x)) {
            brush_range$x <- c(min(data()[[key_col]], na.rm=TRUE),
                               max(data()[[key_col]], na.rm=TRUE))
            brush_range$y <-
              c(min(data()[[param_rct()]], na.rm=TRUE),
                max(data()[[param_rct()]], na.rm=TRUE))
        }

        plot_data <- data() %>%
          dplyr::filter(between(!!sym(key_col),
                                brush_range$x[1],
                                brush_range$x[2]))

        shiny::validate(shiny::need(nrow(plot_data) > 0,
                                    "Scatterplot not available. No concurrent observations exist."))

        axis_lab_1 <- paste0(param_to_expression(
          param_rct()
          ),
                             '~`in`~',
                             unit_to_expression(units_pocs$unit1))

        axis_lab_2 <- paste0(param_to_expression(
          sec_param_rct()
          ),
                             '~`in`~',
                             unit_to_expression(units_pocs$unit2))

        param1_val <- paste0(measurement_col, '.x')
        param2_val <- paste0(measurement_col, '.y')

        plot <- ggplot2::ggplot(data=plot_data,
                                ggplot2::aes(x = !!sym(param1_val),
                                             y = !!sym(param2_val)))

          plot <- plot + ggplot2::geom_point() +
            ggplot2::labs(x = parse(text = axis_lab_1),
                          y = parse(text = axis_lab_2)) +
            scale_color_sonoma() +
            theme_sonoma()
        # }

        if (!is.null(input$OneToOne)) {
          if (input$OneToOne){
            plot <- plot +
              ggplot2::geom_abline(slope = 1,
                                   intercept = 0,
                                   color = sonoma_color_palette(3)[3],
                                   size = 2)
          }
        }

        if (!is.null(input$LinearRegression)) {
          if (input$LinearRegression){


            lm_eqn <- function(df){
              m <- stats::lm(stats::as.formula(paste(param2_val, '~', param1_val)), data = df)
              eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                               list(a = format(round(unname(stats::coef(m)[1]), 2), nsmall = 2),
                                    b = format(round(unname(stats::coef(m)[2]),2), nsmall = 2),
                                    r2 = format(round(summary(m)$r.squared,2), nsmall= 2)))
              as.character(as.expression(eq));
            }
            plot <- plot +
              ggplot2::geom_smooth(method='lm',
                                   formula= y~x,
                                   color=sonoma_color_palette(4)[4],
                                   size = 2,
                                   se=F) +
              geom_text(x = -Inf, y = Inf, label = lm_eqn(plot_data), parse = TRUE, hjust = -0.1, vjust = 1, size = 5)
          }
        }
        plot
      })
    }
  })
}

#' Map Server Function
#'
#' Use alongside sitemapUI() to add map of aqs sites to app. Map will include
#' tooltip with aqs site code, and optionally a
#' radiobutton filter of sites and/or legend with site categories.
#'
#' @param id string ID label that links sitemapUI() to sitemapServer()
#' @param data reactive data.frame Reactive data with location info to be plotted on map. Must
#'                        contain columns lat, lng, and AQS_SITECODE corresponding
#'                        to lattude, longitude, and aqs site code respectively.
#' @param label_col string Column with labels for each circle
#' @param filter_col string (optional, default = NULL) Column to filter sites
#'                          shown on map by. Should only have 2 values. If specified,
#'                          fluidRow with radiobuttons labeled by `filter_labels`
#'                          will be added above map. If specified, `filter_labels`
#'                          must also be specified.
#' @param color_col string (optional, default = NULL) Column used to color site
#'                          markers on map.
#' @param color_order character vector (optional) A vector with the order of
#'                                     color values that appear in the legend.
#'                                     All values in color_col must be specified.
#'                                     1st specified will be blue, then green,
#'                                     the yellow, etc (according to sonoma_
#'                                     color_palette).
#'
#' @param cb reactive (optional) If not NULL, colorblind palette will be applied
#' @param click_buffer numeric (optional) Buffer around a click to select the
#'                             closest map marker. Default = 0.003 degrees.
#' @param add_polygon boolean (optional) Defalt = FALSE. Set to TRUE if a polygon
#'                    will be added to the map in addition to cirle_markers. If TRUE,
#'                    polygon_df and polygon_label_col must be specified.
#' @param polygon_df reactive SpatialPolygonsDataFrame A reactive spatial dataframe,
#'                   must be specified if
#'                   add_polygon is TRUE. A SpatialPolygonsDataFrame, e.g. as
#'                   imported by rgdal's readOGR function, with polygon geometries
#'                   and an attribute for polygon_label_col.
#' @param polygon_label_col string (optional) Must be specified if add_polygon
#'                          is TRUE. A string specifying the attribute in
#'                          polygon_df to label polygons by on the map.
#' @param polygon_color string (optional) Default = 'navy'. Color for the
#'                      polygon outline.
#' @param polygon_weight integer (optional) Weight of the polygon outline.
#'                               Default = 3.
#'
#' @return list List with selected filter value ($radio), and selected label ($sitecode)
#' @importFrom assertthat assert_that
#' @importFrom shiny moduleServer reactive reactiveValues observeEvent
#' @importFrom DT JS datatable
#' @importFrom dplyr select pull filter mutate left_join between all_of
#' @importFrom magrittr `%>%`
#' @importFrom leaflet leaflet addTiles addCircleMarkers colorFactor
#' @export
sitemapServer <- function(id,
                          data,
                          filter_col = NULL,
                          color_col = NULL,
                          color_order = NULL,
                          label_col,
                          cb = reactive({NULL}),
                          click_buffer = 0.003,
                          add_polygon = FALSE,
                          polygon_df = reactive({NULL}),
                          polygon_label_col = NULL,
                          polygon_color = 'navy',
                          polygon_weight = 3) {
  assertthat::assert_that(shiny::is.reactive(data),
                          msg = "Input data must be a reactive data frame")
  shiny::moduleServer(id,
               function(input, output, session) {

                 # Establish objects to return
                 map <- shiny::reactiveValues(sitecode = NULL,
                                              radio = NULL,
                                              sitedata = NULL)

                 # Establish Filter Values
                 filter <- shiny::reactiveValues(value = NULL)

                 sites_2_display <- shiny::reactive({

                   sites <- data()
                   if (!is.null(filter_col)) {

                     filter_values <- sites %>%
                       dplyr::select(dplyr::all_of(filter_col)) %>%
                       unique() %>%
                       dplyr::pull()

                     if(input$MapFilter == 'MapFilter1') {
                       sites <- sites %>%
                         dplyr::filter(!!sym(filter_col) == filter_values[1])
                       filter$value <- filter_values[1]

                     }
                     else{
                       sites <- sites %>%
                         dplyr::filter(!!sym(filter_col) == filter_values[2])
                       filter$value <- filter_values[2]
                     }
                   }
                   return(sites)
                 })





                 output$Map<- leaflet::renderLeaflet({
                   assertthat::assert_that(all(c('lat', 'lng') %in% colnames(sites_2_display())),
                                           msg = "Dataframe must have columns lat, lng")


                   if (!is.null(color_col)) {

                     # Establish list of unique colors
                     unique_colors <- data() %>%
                       pull(!!sym(color_col)) %>%
                       unique()

                     if (is.null(cb())) {
                       pal <- sonoma_color_palette
                     } else {
                       pal <- sonoma_cb_palette
                     }

                     if (is.null(color_order)) {

                       color_order <- unique_colors
                       color_func <- leaflet::colorFactor(pal(length(unique_colors)), color_order)
                     } else {
                       color_order <- color_order[color_order %in% unique_colors]
                       assertthat::assert_that(all(sort(color_order) == sort(unique_colors)),
                                               msg = "color_order must contain all unique values in color_col in the input data.")

                       color_func <- leaflet::colorFactor(pal(length(color_order)), levels = color_order, ordered = T)
                     }
                   } else {
                     color_func <- function(x) {return('#0072B2FF')}

                   }

                   m <- leaflet::leaflet() %>%
                     leaflet::addTiles() # Add default OpenStreetMap map tiles

                   if (add_polygon) {
                     assertthat::assert_that(!is.null(polygon_df()),
                                             msg = "If add_polygon is TRUE, polygon_df (a reactive geo-dataframe) must be specified")
                     assertthat::assert_that(!is.null(polygon_label_col),
                                             msg = "If add_polygon is TRUE, polygon_label_col must be specified")
                     m <- m %>%
                       leaflet::addPolygons(data = polygon_df(),
                                            fillColor = "transparent",
                                            color = polygon_color,
                                            weight = polygon_weight,
                                            label = polygon_df()[[polygon_label_col]])
                   }

                   m <- m %>%
                     leaflet::addCircleMarkers(data = sites_2_display(),
                                               color = color_func(sites_2_display()[[color_col]]),
                                               label = sites_2_display()[[label_col]],
                                               lng = ~lng, lat = ~lat)
                   if (!is.null(color_col)) {
                     color_order_filter <- color_order[color_order %in% unique(sites_2_display()[[color_col]])]
                     m <- m %>%
                       leaflet::addLegend(colors = color_func(color_order_filter), labels = factor(color_order_filter, levels = color_order_filter))
                   }
                   m
                 })

                 shiny::observeEvent(input$Map_marker_click, {
                   marker_click <- input$Map_marker_click

                   sitedata <- data() %>%
                     dplyr::filter(dplyr::between(lat, marker_click$lat-click_buffer,
                                                  marker_click$lat+click_buffer),
                            dplyr::between(lng, marker_click$lng-click_buffer,
                                           marker_click$lng+click_buffer))

                   map$sitedata <- sitedata

                   map$sitecode <- sitedata %>%
                     dplyr::select(label_col) %>%
                     dplyr::pull()
                 })

                 shiny::observeEvent(c(input$Map_click, input$MapFilter), {

                   map$radio <- filter$value

                   map_click <- input$Map_click
                   if (!is.null(map_click$lat)){
                     sites_near_click <- data() %>%
                       dplyr::filter(dplyr::between(lat, map_click$lat-click_buffer,
                                             map_click$lat+click_buffer),
                                     dplyr::between(lng, map_click$lng-click_buffer,
                                             map_click$lng+click_buffer))
                   } else {
                       sites_near_click <- data.frame()
                   }

                   if (nrow(sites_near_click) == 0){
                     map$sitecode <- NULL
                     map$sitedata <- NULL
                   }
                 })
                 return(map)
               })
}






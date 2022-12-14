% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pollutionRoseTabServer.R
\name{pollutionRoseTabServer}
\alias{pollutionRoseTabServer}
\title{Pollution rose tab UI}
\usage{
pollutionRoseTabServer(
  id,
  input_data,
  map_data,
  wd_col,
  ws_col,
  conc_col_1,
  conc_col_2,
  conc_col_3,
  conc_col_4,
  site_rct,
  site_col,
  unit_str_1 = NULL,
  unit_str_2 = NULL,
  unit_str_3 = NULL,
  unit_str_4 = NULL,
  title_1 = shiny::reactive({
     "PM[2.5]"
 }),
  title_2 = shiny::reactive({
     "O[3]"
 }),
  title_3 = shiny::reactive({
     "Fill title_3!"
 }),
  title_4 = shiny::reactive({
     "Fill title_4!"
 }),
  year_filter = TRUE,
  year_col = NULL,
  season_filter = TRUE,
  season_col = NULL,
  dow_filter = FALSE,
  dow_col = NULL,
  hour_filter = FALSE,
  hour_col = NULL,
  breaks_conc_1 = NULL,
  breaks_aqi_1,
  breaks_conc_2 = NULL,
  breaks_aqi_2,
  breaks_conc_3 = NULL,
  breaks_aqi_3 = NULL,
  breaks_conc_4 = NULL,
  breaks_aqi_4 = NULL,
  aqipalette
)
}
\arguments{
\item{id}{string ID used to link pollutionRoseTabUI() to pollutionRoseTabServer()}

\item{input_data}{reactive data.frame Dataframe with concentration data for
all input parameters as well as a column with wind
direction, and a column with wind speed (speed can be
all NAs if plotting pollution rose)}

\item{map_data}{reactive data.frame Dataframe with lat and lng columns
with the location of program sites,
to be plotted on a leaflet map.}

\item{wd_col}{string Column name for column with wind direction values}

\item{ws_col}{string Column name for column with wind speed values}

\item{conc_col_1}{string Column name for column with concentration values
for title_1}

\item{conc_col_2}{string Column name for column with concentration values
for title_2}

\item{conc_col_3}{string Column name for column with concentration values
for title_3}

\item{conc_col_4}{string Column name for column with concentration values
for title_4}

\item{site_rct}{reactive string AQS sitecode for selected site}

\item{site_col}{string Column name for column with AQS sitecode values}

\item{unit_str_1}{string Unit of title_1. Can be an expression.}

\item{unit_str_2}{(optional) string Unit of optional title_2. Can be an expression.}

\item{unit_str_3}{(optional) string Unit of optional title_3. Can be an expression.}

\item{unit_str_4}{(optional) string Unit of optional title_4. Can be an expression.}

\item{title_1}{reactive string Name of first parameter to include in plot title
(can be an expression if subscripts needed).}

\item{title_2}{reactive string (optional) Name of optional second parameter to include in plot title
(can be an expression if subscripts needed).}

\item{title_3}{reactive string (optional) Name of optional second parameter to include in plot title
(can be an expression if subscripts needed).}

\item{title_4}{reactive string (optional) Name of optional second parameter to include in plot title
(can be an expression if subscripts needed).}

\item{year_filter}{boolean If true, multi-select year input included and
plots may be filtered by year.}

\item{year_col}{string (optional) Column name of column with year values if year_filter specified.}

\item{season_filter}{boolean If true, multi-select season input included and
plots may be filtered by season.}

\item{season_col}{string (optional) Column name of column with season values if season_filter specified.}

\item{dow_filter}{boolean If true, multi-select day of week input included and
plots may be filtered by day of week.}

\item{dow_col}{string (optional) Column name of column with day of week values if season_filter specified.}

\item{hour_filter}{boolean If true, multi-select hour input included and
plots may be filtered by hour}

\item{hour_col}{string (optional) Column name of column with hour values if season_filter specified.}

\item{breaks_conc_1}{vector (optional) Manual scale breaks in legend for first
parameter. If not specified, sequence of
6 equal breaks will be created with endpoints
at the max and min concentration value.}

\item{breaks_aqi_1}{vector Manual scale breaks for AQI designations for first
parameter.}

\item{breaks_conc_2}{(optional) Manual scale breaks in legend for optional second
parameter. If not specified, sequence of
6 equal breaks will be created with endpoints
at the max and min concentration value.}

\item{breaks_aqi_2}{(optional) vector Manual scale breaks for AQI designations for
optional second parameter.}

\item{breaks_conc_3}{vector (optional) Manual scale breaks in legend for first
parameter. If not specified, sequence of
6 equal breaks will be created with endpoints
at the max and min concentration value.}

\item{breaks_aqi_3}{vector Manual scale breaks for AQI designations for first
parameter.}

\item{breaks_conc_4}{(optional) Manual scale breaks in legend for optional second
parameter. If not specified, sequence of
6 equal breaks will be created with endpoints
at the max and min concentration value.}

\item{breaks_aqi_4}{(optional) vector Manual scale breaks for AQI designations for
optional second parameter.}

\item{aqipalette}{vector Standard color palette for aqi categories with elements:
- aqipalette$greenGood,
- aqipalette$yellowModerate,
- aqipalette$orangeUSG,
- aqipalette$redUnhealthy,
- aqipalette$purpleVeryUnhealthy,
- aqipalette$maroonHazardous}
}
\description{
Used alongside pollutionRoseTabUI() to add a collection of reactive
elements to an app, including multiselects for seasons and years, a select
for metric type (aqi or concentration), and up to 2 pollution roses for
two specified parameters.
}

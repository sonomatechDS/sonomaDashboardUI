% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseriesServer.R
\name{timeseriesServer}
\alias{timeseriesServer}
\title{Timeseries Server Function}
\usage{
timeseriesServer(
  id,
  data_param1_rct,
  data_param2_rct = shiny::reactive({
     NULL
 }),
  x,
  y,
  param_col,
  unit_col,
  param_rct,
  sec_param_rct = shiny::reactive({
     NULL
 }),
  sec_duration_rct = shiny::reactive({
     NULL
 }),
  linesize = 0.5,
  pointsize = 3
)
}
\arguments{
\item{id}{string ID label that links timeseriesUI() to timeseriesServer()}

\item{data_param1_rct}{reactive Reactive hourly data}

\item{data_param2_rct}{reactive (optional) Reactive data (either 1hr or 24hr)
for the optionally specified sec_param_rct.
Should have the same columns as
data_param1_rct.}

\item{x}{string Column name of datetime value (POSIXct format)}

\item{y}{string Column name of concentration values}

\item{param_col}{string Column name of parameter values}

\item{unit_col}{string Column name of unit values}

\item{param_rct}{reactive String value of primary parameter to display}

\item{sec_param_rct}{reactive (optional) String value of secondary parameter
to display. If not specified, only 1 parameter
(param_rct) is displayed.}

\item{sec_duration_rct}{reactive (optional) Duration of secondary parameter.
Either '1 HOUR' or other. If \code{sec_param_rct}
is specified, this must also be specified.}

\item{linesize}{numeric (optional) Size of plotted line (default = 0.5)}

\item{pointsize}{numeric (optional) Size of plotted points (default = 3)}
}
\value{
reactive List with elements \code{x} and \code{y}, each a vector with the min
and max x and y values (respectively) of the brush.
}
\description{
Used alongside timeseriesUI() to add a timeseries of a 1hr parameter
and optionally a secondary 1hr OR 24hr parameter (wrapped in a fluidRow)
to an app. Timeseries will include zoom functionality, where a box
can be dragged across the plot area and double-clicked. To reset zoom,
double-click the plot area again.
}
\details{
1Hr data and 24Hr data should have the same x, y, and units column names.
A secondary 1Hr parameter will be displayed as a dotted line. A secondary
24Hr parameter will be displayed as points.
}

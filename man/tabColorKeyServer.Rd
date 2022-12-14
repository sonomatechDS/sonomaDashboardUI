% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabColorKeyServer.R
\name{tabColorKeyServer}
\alias{tabColorKeyServer}
\title{Tab color key}
\usage{
tabColorKeyServer(
  id,
  n,
  labels,
  cb = reactive({
     FALSE
 })
)
}
\arguments{
\item{id}{string ID to link server to UI}

\item{n}{integer Number of unique tab colors}

\item{labels}{character vector Labels to give to each respective tab color}

\item{cb}{boolean If T, colorblind accessible palette is used}
}
\description{
Creates key for tab colors that updates depending on coloblind accessible
selection.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/open_and_write.R
\name{open_and_write}
\alias{open_and_write}
\title{Open and write file}
\usage{
open_and_write(path, text, save_and_close = TRUE)
}
\arguments{
\item{path}{string Name of file or full file path with name of file at end (
ending with '.R' file suffix)}

\item{text}{string Text to insert into file}

\item{save_and_close}{boolean If TRUE, saves and closes file (default). If FALSE,
file remains open in RStudio.}
}
\description{
Adapted from source: https://community.rstudio.com/t/using-rstudioapi-to-create-a-new-file-and-insert-text/41549/3
}

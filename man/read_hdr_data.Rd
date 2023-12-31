% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xl_data_hdr.R
\name{read_hdr_data}
\alias{read_hdr_data}
\title{List Excel worksheets as tibbles}
\usage{
read_hdr_data(
  fp,
  wks,
  col_start = "B",
  col_stop = "B",
  row_hdr = 1L,
  row_start = 6L,
  row_stop = 6L
)
}
\arguments{
\item{fp}{\if{html}{\out{<chr>}} file paths}

\item{wks}{\if{html}{\out{<chr>}} worksheet names}

\item{col_start}{\if{html}{\out{<chr>}} initial column of data per worksheet}

\item{col_stop}{\if{html}{\out{<chr>}} final column of data per worksheet}

\item{row_hdr}{\if{html}{\out{<int>}} row containing column names per worksheet}

\item{row_start}{\if{html}{\out{<int>}} initial row of data per worksheet}

\item{row_stop}{\if{html}{\out{<int>}} final row of data per worksheet}
}
\value{
\if{html}{\out{<lst>}} list of tibbles
}
\description{
Given a set of Excel worksheets, return a list of
corresponding tibbles.  Designed for worksheets in
which column names may be separated from data by
intervening rows.
}
\examples{
\dont_run{
  read_hdr_data("./my-xl-file.xlsx", "Sheet1", "A", "B")
}


}

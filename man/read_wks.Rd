% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xl_data_hdr.R
\name{read_wks}
\alias{read_wks}
\title{Render a single Excel worksheet as a tibble}
\usage{
read_wks(
  wks,
  fN = NULL,
  fp = NULL,
  col_start = "B",
  col_stop = "B",
  row_hdr = 1L,
  row_start = 6L,
  row_stop = 6L
)
}
\arguments{
\item{wks}{\if{html}{\out{<chr>}} name of the worksheet}

\item{fp}{\if{html}{\out{<chr>}} full file path (see Description)}

\item{col_start}{\if{html}{\out{<chr>}} initial column of data}

\item{col_stop}{\if{html}{\out{<chr>}} final column of data}

\item{row_hdr}{\if{html}{\out{<int>}} row of column names}

\item{row_start}{\if{html}{\out{<int>}} initial row of data}

\item{row_stop}{\if{html}{\out{<int>}} final row of data}

\item{fn}{\if{html}{\out{<chr>}} file name, including file extension}
}
\value{
\if{html}{\out{<lst>}} list of tibbles
}
\description{
Render a named worksheet in a specified Excel file as a tibble.
The file is identified either by the full file-path (fp) if given,
or else as here("data", "xlsx", fn), where fn is the given file name.
intervening rows.
}
\examples{
\dont_run{
  read_hdr_data("./my-xl-file.xlsx", "Sheet1", "A", "B")
}

}

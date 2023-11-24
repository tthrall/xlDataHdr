### 
## 
#  xl_data_hdr.R
#    Facilitate the reading of Excel files in which
#    column names may be separated from data by 
#    intervening rows.
## 
### 

## 
#  make_xl_range_vec()
## 
#' @title Construct Excel ranges from end-points
#' 
#' @description
#' Given a set of Excel end-points, retrun the corresponding 
#' Excel ranges
#' 
#' @param col_start <chr> initial column
#' @param col_stop <chr> final column
#' @param row_start <int> initial row
#' @param row_stop <int> final row
#' 
#' @import tibble
#' 
#' @return <chr> vector of elements of the form "A1:B2"
#' 
#' @examples
#' make_xl_range_vec("A", "B", 1, 2)
#' make_xl_range_vec(c("A", "B"), c("B", "C"), 1, 2)
#' 
make_xl_range_vec <- function(
    col_start = "B", # <chr> initial column
    col_stop  = "B", # <chr> final column
    row_start = 6L,  # <int> initial row
    row_stop  = 6L   # <int> final row
) {
  range_tbl <- tibble::tibble(
    cr_start = paste0(col_start, row_start), 
    cr_stop  = paste0(col_stop,  row_stop), 
    cr_range = paste0(cr_start, ":", cr_stop)
  )
  return(range_tbl$cr_range)
}

## 
#  make_hdr_range()
## 
#' @title Excel range of columns in a single row
#' 
#' @description
#' Given a pair of column end-points in a single row 
#' return the corresponding Excel range.  Can be used 
#' to extract column names.
#' 
#' @param col_start <chr> initial column
#' @param col_stop <chr> final column
#' @param row_hdr <int> desired row, e.g., containing column names
#' 
#' @import tibble
#' 
#' @return <chr> single string of the form "A1:B2"
#' 
#' @examples
#' make_hdr_range("A", "Z")
#' make_hdr_range("A", "Z", 2)
#' 
make_hdr_range <- function(
    col_start = "B", # <chr> initial column
    col_stop  = "B", # <chr> final column
    row_hdr   = 1L   # <int> desired row
) {
  return(make_xl_range_vec(
    col_start = col_start, 
    col_stop  = co_stop, 
    row_start = row_hdr, 
    row_stop  = row_hdr
  ))
} 

## 
#  read_hdr_data()
## 
#' @title List Excel worksheets as tibbles
#' 
#' @description 
#' Given a set of Excel worksheets, return a list of 
#' corresponding tibbles.  Designed for worksheets in 
#' which column names may be separated from data by 
#' intervening rows.
#' 
#' @param fp <chr> file paths
#' @param wks <chr> worksheet names
#' @param col_start <chr> initial column of data per worksheet
#' @param col_stop <chr> final column of data per worksheet
#' @param row_hdr <int> row containing column names per worksheet
#' @param row_start <int> initial row of data per worksheet
#' @param row_stop <int> final row of data per worksheet
#' 
#' @import tibble
#' @import readxl
#' 
#' @return <lst> list of tibbles
#' 
#' @examples 
#' \dont_run{
#'   read_hdr_data("./my-xl-file.xlsx", "Sheet1", "A", "B")
#' }
#' 
#' 
read_hdr_data <- function(
  fp,  # <chr> paths to the Excel files to be read
  wks, # <chr> names of Excel worksheets to be read
  col_start = "B", # <chr> initial column of data per wks
  col_stop  = "B", # <chr> final column of data per wks
  row_hdr   = 1L,  # <int> row number per wks of column names
  row_start = 6L,  # <int> initial row of data per wks
  row_stop  = 6L   # <int> final row of data per wks
) {
  # range of data per worksheet
  rg_data <- make_xl_range_vec(
    col_start = col_start, 
    col_stop  = co_stop, 
    row_start = row_start, 
    row_stop  = row_stop)
  
  # range of column names per worksheet
  rg_hdr <- make_xl_range_vec(
    col_start = col_start, 
    col_stop  = co_stop, 
    row_start = row_hdr, 
    row_stop  = row_hdr)
  
  # params tibble to ensure one value per (fp, wks)
  xl_params_tbl <- tibble::tibble(
    fp      = fp, 
    wks     = wks, 
    rg_data = rg_data, 
    rg_hdr  = rg_hdr)
  
  # read successive worksheets
  tbl_lst <- list()
  idx     <- 1L
  for (w in xl_params_tbl$wks) {
    # column names
    hdr_tbl <- xl_params_tbl$fp [[idx]] |> 
      readxl::read_xlsx(
        sheet     = w, 
        col_names = TRUE, 
        range     = xl_params_tbl$rg_hdr [[idx]] )
    
    # data 
    tbl_lst [[idx]] <- xl_params_tbl$fp [[idx]] |> 
      readxl::read_xlsx(
        sheet     = w, 
        col_names = FALSE, 
        range     = xl_params_tbl$rg_data [[idx]] )
    
    names(tbl_lst [[idx]]) <- names(hdr_tbl)
    idx <- idx + 1L
  }
  names(tbl_lst) <- xl_params_tbl$wks
  return(tbl_lst)
} 

## 
#  read_wks()
## 
#' @title Render a single Excel worksheet as a tibble
#' 
#' @description 
#' Render a named worksheet in a specified Excel file as a tibble.
#' The file is identified either by the full file-path (fp) if given, 
#' or else as here("data", "xlsx", fn), where fn is the given file name.
#' intervening rows.
#' 
#' @param wks <chr> name of the worksheet
#' @param fn <chr> file name, including file extension
#' @param fp <chr> full file path (see Description)
#' @param col_start <chr> initial column of data
#' @param col_stop <chr> final column of data
#' @param row_hdr <int> row of column names
#' @param row_start <int> initial row of data
#' @param row_stop <int> final row of data
#' 
#' @import assertthat
#' @import here
#' 
#' @return <lst> list of tibbles
#' 
#' @examples 
#' \dont_run{
#'   read_hdr_data("./my-xl-file.xlsx", "Sheet1", "A", "B")
#' }
#' 
read_wks <- function(
    wks,        # <chr> name of the worksheet
    fN = NULL,  # <chr> Excel file name, including extension
    fp = NULL,  # <chr> full file path (see Description)
    col_start = "B", # <chr> initial column of data
    col_stop  = "B", # <chr> final column of data
    row_hdr   = 1L,  # <int> row number of column names
    row_start = 6L,  # <int> initial row of data
    row_stop  = 6L   # <int> final row of data
) {
  # my default file-path
  if (is.null(fp)) {
    assertthat::assert_that(! is.null(fn))
    fp <- here::here("data", "xlsx", fn)
  }
  
  tbl_lst <- read_hdr_data(
    fp  = fp, 
    wks = wks, 
    col_start = col_start, 
    col_stop  = col_stop, 
    row_hdr   = row_hdr, 
    row_start = row_start, 
    row_stop  = row_stop
  )
  
  return(tbl_lst [[1]])
}


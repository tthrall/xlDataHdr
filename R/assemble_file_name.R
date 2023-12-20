## 
#  assemble_file_name()
## 
#' @title Construct a time-stamped file-name
#' 
#' @description
#' Construct a file-name following a time-stamp naming convention.
#' 
#' @param prefix <chr> initial part of file-name
#' @param suffix <chr> file extension
#' @param dt_chr <chr> date as "yyyy-mm-dd" or other format
#' @param hr_chr <chr> time as "hhmm" or other format
#' 
#' @import assertthat
#' 
#' @return <chr> file-name
#' 
#' @examples
#' assemble_file_name(prefix = "my_xl_data")
#' assemble_file_name(prefix = "my-tsv-data", suffix = "tsv")
#' assemble_file_name(prefix = "initial-data", dt_chr = "2019-01-31")
#' assemble_file_name(prefix = "current-xl-data", dt_chr = "2024-01-31", hr_chr = "1330")
#' 
assemble_file_name <- function(
    prefix,          # <chr> initial part of file-name
    suffix = "xlsx", # <chr> file extension
    dt_chr = "",     # <chr> date as "yyyy-mm-dd"
    hr_chr = ""      # <chr> time as "hhmm"
) {
  # require prefix and suffix
  assertthat::assert_that(
    ! is.null(prefix), 
    ! is.null(suffix))
  
  # require date if time is specified
  if (hr_chr != "") {
    assert_that(dt_chr != "")
    fn <- paste0(
      prefix, "_", 
      dt_chr, "_", 
      hr_chr, ".", 
      suffix)
  } else {
    if (dt_chr != "") {
      fn <- paste0(
        prefix, "_", 
        dt_chr, ".", 
        suffix)
    } else {
      fn <- paste0(
        prefix, ".", 
        suffix)
    }}
  return(fn)
}
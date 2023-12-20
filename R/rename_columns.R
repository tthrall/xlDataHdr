###
##
#  rename_columns.R
#    -- Rename the columns of a data frame.
#
#  required packages:
#
#    assertthat::
#    readxl::
#    tibble::
#
##
###

##
#  rename_columns()
##
#' @title Rename the columns of a data frame
#'
#' @description
#' The solution presented here was given in
#' "https://stackoverflow.com/a/43742442/3927208"
#'
#' @param df <tbl> data frame whose columns are to be renamed
#' @param lookup_tbl <tbl> tibble containing (old, new) pairs of names
#' @param old_names <idx> column name or index of old names in lookup_tbl
#' @param new_names <idx> column name or index of new names in lookup_tbl
#'
#' @import tibble
#'
#' @return <df> data frame with new column names
#'
#' @examples
#' \dontrun{
#'   df_tst <- tibble(
#'     A = 1:3,
#'     B = 2L * A,
#'     C = 3L * A)
#'   lu_tst <- tibble(
#'     old = LETTERS[1:3],
#'     new = letters[1:3])
#'   new_df <- rename_columns(df_tst, lu_tst, "old", "new")
#' }
#'
rename_columns <- function(
    df,         # <tbl> data frame whose columns are to be renamed
    lookup_tbl, # <tbl> tibble containing (old, new) pairs of names
    old_names,  # <idx> column name or index of old names in lookup_tbl
    new_names   # <idx> column name or index of new names in lookup_tbl
) {
  new_df <- df
  names(new_df) <-
    lookup_tbl[[new_names]] [match(
      names(df),
      lookup_tbl[[old_names]]
    )]
  return(new_df)
}

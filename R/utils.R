#' Identify whether all values of a vector are numeric or not
#'
#' @param x Character vector
#'
#' @keywords internal
#'
is_num <- function(x) {
  !all(is.na(suppressWarnings(as.numeric(x))))
}

#' Add a running ID to duplicated names
#'
#' @param col_name Character vector of column names
#'
#' @returns the `col_name` character vector with IDs for duplicated names
#'
#' @keywords internal
#'
add_suffix_to_duplicate <- function(col_name){
  dupl <- table(col_name) %>%
    .[. > 1]

  if(length(dupl > 0)) {
    for(i in 1:length(dupl)) {
      col_name[col_name == names(dupl[i])] <-
        paste0(names(dupl[i]), c('', 1:(dupl[i]-1)))
    }
  }

  return(col_name)
}

#' Transform x to a matrix with n columns and fill up with NA values
#'
#' @param x character vector or NULL
#' @param n Number of elements
#'
#' @keywords internal
#'
as_mtx_null <- function(x, n) {
  if(is.null(x)) {
    matrix(rep(NA_character_, n), ncol = n)
  } else {
    x <- x[1:n]
    matrix(x, nrow = n) %>%
      t(.)
  }
}

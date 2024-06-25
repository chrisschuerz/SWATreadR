#' Turn a .con input table into a long format table
#'
#' @param con_tbl .con input table in (default) wide format
#'
#' @returns The .con input table in a long format (each object connectivity
#'   will be a new row). Because of name duplicate the column name 'obj_id'
#'   of the defined unit is renamed to 'obj_id_'.
#'
#' @importFrom dplyr filter left_join rename select %>%
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
pivot_con_long <- function(con_tbl) {
  if(any(con_tbl$out_tot > 0)) {
    cons <- con_tbl %>%
      select(., id, obj_typ_1:ncol(.)) %>%
      pivot_longer(
        cols = -id,
        cols_vary = "fastest",
        names_to = c(".value", "i"),
        names_pattern = "(.*)_(.*)$"
      ) %>%
      filter(!is.na(obj_id))
    con_tbl %>%
      select(1:out_tot) %>%
      rename(obj_id_ = obj_id) %>%
      left_join(., cons, by = 'id')
  }
}

#' Turn a .con input table in its long format back to its initial wide format.
#'
#' @param con_tbl .con input table in long format.
#'
#' @returns The .con input table in a wide format (each object will be one
#' row, and all connectivities are in the same row). 'obj_id_' which is
#' renamed by `pivot_con_long` is renamed to 'obj_id' again.
#'
#' @importFrom dplyr distinct left_join rename select %>%
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
pivot_con_wide <- function(con_tbl) {
  if('obj_id_' %in% names(con_tbl)) {
    cons <- con_tbl %>%
      select(id, i:frac) %>%
      pivot_wider(names_from = i,
                  names_glue = "{.value}_{i}",
                  names_vary = 'slowest',
                  values_from = c(obj_typ, obj_id, hyd_typ, frac),
                  values_fill = list(obj_typ = '', obj_id = NA,
                                     hyd_typ = '', frac = NA))

    con_tbl %>%
      select(1:out_tot) %>%
      distinct() %>%
      rename(obj_id = obj_id_) %>%
      left_join(., cons, by = 'id')
  } else {
    stop('This is not a .con table in its long format!')
  }
}

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

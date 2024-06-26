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

#' Convert the elements columns in SWAT input format into vectors of individual
#' integer values
#'
#' @param tbl SWAT+ input table which has the columns elem_tot and following
#'   elements columns.
#'
#' @returns The `tbl` where the columns `elem_tot` and `elements_*` are replaced
#'   by a list column with integer vectors providing all individual element IDs.
#'
#' @importFrom dplyr select %>%
#' @importFrom purrr map map_chr pmap
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect starts_with
#'
#' @export
#'
elements_to_vector <- function(tbl) {
  if(!all(c('elem_tot', 'elements_1') %in% names(tbl))) {
    stop("Table must contain the columns 'elem_tot' and 'elements_1, elements_2, etc...")
  }
  elements <- tbl %>%
    select(starts_with('elements_')) %>%
    pmap(., c) %>%
    map(., as.character) %>%
    map(., ~ paste(.x, collapse = ',')) %>%
    map_chr(., ~ str_replace_all(.x, ',-', ':')) %>%
    paste0('c(', ., ')') %>%
    map(., ~ eval(parse(text=.x)))

  tbl <- select(tbl, -starts_with('elem'))
  tbl$elements <- elements

  return(tbl)
}

#' Convert the elements list column into the columns elem_tot and elements_*
#' which are the default SWAT+ format.
#' integer values
#'
#' @param tbl SWAT+ input table which has the list column elements.
#'
#' @returns The `tbl` where the list column `elements` is converted into the
#'   columns `elem_tot` and `elements_*`.
#'
#' @importFrom dplyr bind_cols select %>%
#' @importFrom purrr list_rbind map
#'
#' @export
#'
vector_to_elements <- function(tbl) {
  if(!'elements' %in% names(tbl)) {
    if(!typeof(tbl$elements) == 'list') {
      stop("Table must contain the list column 'elements'.")
    }
  }
  elements <- tbl$elements %>%
    map(., ~ values_to_elements(.x)) %>%
    list_rbind()

  elem_tot <- apply(elements, 1, sum_elements)

  tbl <- tbl %>%
    select(-elements) %>%
    bind_cols(., elem_tot = elem_tot, elements)

  return(tbl)
}

#' Convert the information on available runs for the simulated variables into
#' strings that are printed
#'
#' @param tbl overview table that provides meta data for all simulation runs for
#'   all variables saved in the data bases
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map2 map2_chr
#' @keywords internal
#'
values_to_elements <- function(vals) {
  vals <- sort(vals)
  diff_vals <- diff(vals)

  end_seq   <- unique(c(vals[diff_vals != 1], vals[length(vals)]))
  start_seq <- unique(c(vals[1], vals[which(diff_vals != 1) + 1]))

  map2(start_seq, end_seq, ~build_element_sequence(.x, .y)) %>%
    unlist() %>%
    as_tibble_row(., .name_repair = ~ paste0('elements_', 1:length(.)))
}

#' Build the element value sequence for a pair of start and end value.
#'
#' @param strt Numeric start value of sequence
#' @param end  Numeric end value of sequence
#'
#' @keywords internal
#'
build_element_sequence <- function(strt, end) {
  if(strt == end) {
    strt
  } else {
    c(strt, - end)
  }
}

#' Sum the elements which are not NA
#'
#' @param elem Element entries.
#'
#' @keywords internal
#'
sum_elements <- function(elem) {
  sum(!is.na(elem))
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

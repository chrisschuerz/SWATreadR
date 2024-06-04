#' Write SWAT+ input file which has a tabular structure.
#'
#' @param tbl SWAT input table in tibble (data.frame) format.
#' @param file_path Write path of the SWAT+ input file.
#' @param fmt Character vector of format strings to define the print format of
#'   each table column.
#'
#' @returns Writes a text file table in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_int map2_df
#' @importFrom readr write_lines
#' @importFrom stringr str_remove str_replace str_replace_all
#'
#' @keywords internal
#'
write_tbl <- function(tbl, file_path, fmt) {
  if(length(fmt) < ncol(tbl)) {
    # To account for optional description columns
    fmt <- c(fmt, rep('%s', ncol(tbl) - length(fmt)))
  }

  tbl <- map2_df(tbl, fmt, ~ sprintf(.y, .x))

  fmt_names <- fmt %>%
    str_remove(., '\\.[:digit:]+') %>%
    str_replace(., 'f|d', 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_names, .) %>%
    paste(., collapse = '  ')

  file_lines <- tbl %>%
    apply(., 1, paste, collapse = '  ') %>%
    str_replace_all(., '  NA', '    ')

  file_head <- paste('SWAT+ input file written with SWATreadR at', Sys.time())

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}

#' Write SWAT+ input file which has a tabular structure with a definition line
#' for each parameter table section (e.g. management.sch, plant.ini, soils.sol).
#'
#' @param tbl SWAT input table in tibble (data.frame) format.
#' @param file_path Write path of the SWAT+ input file.
#' @param fmt_def Character vector of format strings to define the print format
#'   of each table column which is part of the definition line.
#' @param fmt_par Character vector of format strings to define the print format
#'   of each table column which is part of the parameter table.
#'
#' @returns Writes a text file table in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr list_c map map_int map2_df
#' @importFrom readr write_lines
#' @importFrom stringr str_remove str_replace str_replace_all
#'
#' @keywords internal
#'
write_tbl2 <- function(tbl, file_path, fmt_def, fmt_par) {
  n_def <- length(fmt_def)
  n_col <- ncol(tbl)

  fmt_names <- c(fmt_def, fmt_par) %>%
    str_remove(., '\\.[:digit:]+') %>%
    str_replace(., 'f|d', 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_names, .) %>%
    paste(., collapse = '  ')

  split_var <- factor(tbl[[1]])
  tbl <- tbl %>%
    map2_df(., c(fmt_def, fmt_par), ~sprintf(.y, .x)) %>%
    split(., split_var)

  def_lines <- tbl %>%
    map_df(., ~ .x[1,1:n_def]) %>%
    apply(., 1, paste, collapse = '  ')

  n_shift <- nchar(def_lines[1]) + 2

  par_lines <- tbl %>%
    map(., ~ .x[,(n_def+1):n_col]) %>%
    map(., ~ apply(.x, 1, paste, collapse = '  ')) %>%
    map(., ~ paste0(sprintf(paste0('%', n_shift, 's'), ''), .x))

  file_lines <- map2(def_lines, par_lines, ~ c(.x, .y)) %>%
    list_c(.) %>%
    str_replace_all(., '  NA', '    ')

  file_head <- paste('SWAT+ input file written with SWATreadR at', Sys.time())

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}

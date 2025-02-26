#' Read a SWAT+ input file which has a tabular structure.
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names (optional) Column names vector.
#' @param n_skip Number of header rows to skip. Default is 1.
#'
#' @returns The SWAT+ input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr recode %>%
#' @importFrom purrr map2_df
#' @importFrom tibble add_column tibble
#'
#' @keywords internal
#'
read_tbl <- function(file_path, col_names = NULL, col_types = NULL, n_skip = 1,
                     has_unit = FALSE, keep_attr = FALSE) {
  if (file.exists(file_path)) {
    tbl <- fread(file_path, skip = n_skip + 1 + has_unit, header = FALSE)
    if (is.null(col_names)) {
      col_names <- fread(file_path, skip = n_skip, nrows = 1, header = F) %>%
        unlist(.) %>%
        unname(.) %>%
        add_suffix_to_duplicate(.)
    }
    if ('description' %in% col_names & ncol(tbl) == length(col_names) - 1) {
      tbl <- add_column(tbl, description = '')
    } else if (ncol(tbl) > length(col_names)) {
      col_names_add <- paste0('v_', 1:(ncol(tbl) - length(col_names)))
      col_names <- c(col_names, col_names_add)
      warning("Number of columns of '", basename(file_path),"' > column names.\n",
              "Column names ", paste(col_names_add, collapse = ', '),
              ' were assigned to columns at the end.\n')
    } else if (ncol(tbl) < length(col_names)) {
      col_names_rmv <- col_names[(ncol(tbl) + 1):length(col_names)]
      col_names <- col_names[1:ncol(tbl)]
      warning("Number of columns of '", basename(file_path),"' < column names.\n",
              "Column names ", paste(col_names_rmv, collapse = ', '),
              ' were removed.\n')
    }

    names(tbl) <- col_names
    tbl <- tibble(tbl)

    if(is.null(keep_attr)) keep_attr <- FALSE

    if(keep_attr & n_skip > 1) {
      tbl_attr <- readLines(file_path, n = n_skip)
      attr(tbl, 'header') <- tbl_attr[2:n_skip]
    }

  } else {
    if (is.null(col_names)) {
      stop("File '", basename(file_path), "' does not exist and no 'col_names' ",
           'were provided to generate empty table.')
    }

    tbl <- tibble(!!!rep(NA, length(col_names)),
                  .rows = 0, .name_repair = ~ col_names)
  }

  if(!is.null(col_types)) {
    col_types <- unlist(strsplit(col_types, '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')
    tbl <- map2_df(tbl, col_types, ~ as(.x, .y))
  }

  return(tbl)
}

#' Read a SWAT+ input file which has a tabular structure with a definition line
#' for each parameter table section (e.g. management.sch, plant.ini, soils.sol).
#'
#' @param file_path Path of the SWAT+ input file.
#' @param def_names Vector of column names for the entries in the definition
#'   line.
#' @param par_names Vector of parameter names of the parameter table.
#' @param id_num ID vector to define the columns which are numerical values.
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_int map_chr map2 map2_df map_df set_names
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
read_tbl2 <- function(file_path, def_names, par_names, id_num = NULL) {
  n_def <- length(def_names)
  n_par <- length(par_names)

  file_line <- fread(file_path, skip = 2, sep = NULL, sep2 = NULL,
                     header = FALSE) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_replace_all(., '\t', ' ') %>%
    str_split(., '[:space:]+')

  n_elem <- map_int(file_line, length)
  file_line  <- file_line[n_elem != 1]
  n_elem <- n_elem[n_elem != 1]
  def_pos <- which(n_elem == n_def)

  pos_start <- def_pos + 1
  pos_end <- c(def_pos[2:length(def_pos)] - 1, length(file_line))
  no_entry <- pos_start > pos_end
  pos_start[no_entry] <- length(file_line) + 1
  pos_end[no_entry] <- length(file_line) + 1

  par_tbl <- map2(pos_start, pos_end, ~ file_line[.x:.y]) %>%
    map(., unlist) %>%
    map(., ~ as_mtx_null(.x, n_par)) %>%
    map(., ~ as_tibble(.x, .name_repair = ~ par_names))

  n_op <- map_int(par_tbl, nrow)

  par_tbl <- bind_rows(par_tbl)

  def_tbl <- map(def_pos, ~ file_line[[.x]]) %>%
    map(., unlist) %>%
    map(., ~ as_mtx_null(.x, n_def)) %>%
    map(., ~ as_tibble(.x, .name_repair = ~ def_names)) %>%
    map2(., n_op, ~ .x[rep(1, .y), ]) %>%
    bind_rows(.)

  tbl <- bind_cols(def_tbl, par_tbl)

  if(!is.null(id_num)) {
    tbl[,id_num] <- map_df(tbl[,id_num], as.numeric)
  }

  return(tbl)
}

#' Read a SWAT+ connecitivity (*.con) input file.
#'
#' @param file_path Path of the SWAT+ input file.
#'
#' @returns The connecitivity input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr across mutate
#' @importFrom purrr set_names
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyselect matches starts_with
#'
#' @keywords internal
#'
read_con <- function(file_path) {
  obj_names <- c("id", "name", "gis_id", "area", "lat", "lon", "elev",
                 "obj_id", "wst", "cst", "ovfl", "rule", "out_tot")
  con_names <- c("obj_typ", "obj_id", "hyd_typ", "frac")

  if(file.exists(file_path)) {
    con_mtx <- fread(file_path, skip = 2, sep = NULL, sep2 = NULL, header = F) %>%
      unlist(.) %>%
      unname(.) %>%
      str_trim(.) %>%
      str_split(., '[:space:]+', simplify = T)

    n_con <- (dim(con_mtx)[2]-length(obj_names)) / length(con_names)
    if(n_con > 0) {
      rep_ids <- 1:n_con
    } else {
      rep_ids <- NULL
    }

    con_names <- paste(rep(con_names, n_con),
                       rep(rep_ids, each = length(con_names)),
                       sep = '_')

    col_types <- unlist(strsplit(c('iciddddiciiii', rep('cicd', n_con)), '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')

    con_tbl <- as_tibble(con_mtx, validate = NULL,
                         .name_repair = ~ c(obj_names, con_names)) %>%
      map2_df(., col_types, ~ as(.x, .y))

    # id_int <- c(1,3,8,13, 15 + (rep_ids - 1)*4)
    # con_tbl[ , id_int] <- map_df(con_tbl[ , id_int], as.integer)
    #
    # id_dbl <- c(4:7, 17 + (rep_ids - 1)*4)
    # con_tbl[ , id_dbl] <- map_df(con_tbl[ , id_dbl], as.numeric)
  } else {
    con_tbl <- tibble(!!!rep(NA, length(obj_names)),
                      .rows = 0, .name_repair = ~ obj_names)

    col_types <- unlist(strsplit('iciddddiciiii', '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')

    con_tbl <- map2_df(con_tbl, col_types, ~ as(.x, .y))
  }

  return(con_tbl)
}

#' Read the n column of a tabular SWAT+ input file which are defined by the
#' column positions `id_col_sel`. This is useful if e.g. last columns with
#' description cause issues with reading due to blanks in the description text.
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names (optional) Character column names vector.
#' @param n_skip Number of header rows to skip. Default is 1.
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_df map_lgl map_int
#' @importFrom stringr str_replace str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @export
#'
read_linewise <- function(file_path, col_names = NULL, n_skip = 1) {
  if (is.null(col_names)) {
    col_names <- fread(file_path, skip = n_skip, nrows = 1, header = F) %>%
      unlist(.) %>%
      unname(.) %>%
      add_suffix_to_duplicate(.)

    col_names[length(col_names)] <- paste0(col_names[length(col_names)], '_*')
  }

  file_line <- fread(file_path, skip = n_skip + 1, sep = NULL, sep2 = NULL,
                     header = FALSE) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_replace_all(., '\t', ' ') %>%
    str_split(., '[:space:]+')

  max_elems <- max(map_int(file_line, length))

  if(length(col_names) > max_elems) {
    col_names <- col_names[1:max_elems]
  } else if (length(col_names) < max_elems) {
    col_name_rep <- col_names[length(col_names)]
    id_add <- as.character(1:(max_elems - length(col_names) + 1))
    col_name_rep <- str_replace(col_name_rep, '\\*$', id_add)
    col_names <- c(col_names[1:(length(col_names) - 1)], col_name_rep)
  } else {
    col_names <- str_replace(col_names, '\\*$', '')
  }

  tbl <- file_line %>%
    map(., ~ as_mtx_null(.x, max_elems)) %>%
    map_df(., ~ as_tibble(.x, .name_repair = ~ col_names))

  col_num <- map_lgl(tbl, is_num)
  tbl[,col_num] <- map_df(tbl[,col_num], as.numeric)

  return(tbl)
}

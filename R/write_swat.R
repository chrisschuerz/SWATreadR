#' Write SWAT+ input files.
#'
#' @param tbl SWAT input table
#' @param file_path Write path of the SWAT+ file.
#' @param overwrite Should original file be overwritten or renamed before writing?
#'
#' @returns Writes a text file table in the file path.
#'
#' @export
#'
write_swat <- function(tbl, file_path, overwrite = FALSE) {
  file_name <- basename(file_path)
  dir_path  <- dirname(file_path)

  write_type <- lookup_write_type(file_name)

  if (file_name %in% list.files(path = dir_path) & overwrite == FALSE) {
    file_name_new <- paste0(file_name, '_repl_', format(Sys.time(), '%Y%m%d%H%m'))
    cat(paste0("Renaming existing file '", file_name, "' to '", file_name_new,"' to avoid overwrite."))
    file.rename(file_path, paste0(dir_path, '/', file_name_new))
  }

  if(is.null(write_type[[1]])) {
    stop("File with the name '", file_name, "' is not supported!")
  } else if (write_type$type == 'tbl') {
    if(!is.null(write_type$add_lines)) {
      if(write_type$add_lines == 'n_row') {
        write_type$add_lines <- as.character(nrow(tbl))
        write_type$write_col_names <- TRUE
      } else if (write_type$add_lines == 'header') {
        write_type$add_lines <- attr(tbl, 'header')
        write_type$write_col_names <- FALSE
      }
    } else {
      write_type$add_lines <- NULL
      write_type$write_col_names <- TRUE
    }

    write_tbl(tbl = tbl, file_path = file_path, fmt = write_type$fmt,
              add_lines = write_type$add_lines,
              write_col_names = write_type$write_col_names)
  } else if (write_type$type == 'tbl2') {
    write_tbl2(tbl = tbl,
               file_path = file_path,
               fmt_def = write_type$fmt_def,
               fmt_par = write_type$fmt_par)
  } else if (write_type$type == 'con') {
    n_con <- (ncol(tbl) - 13) / 4
    fmt_con <- c('%8d', '%-16s', '%8d', rep('%12.5f', 4),
                 '%8d', '%16s', rep('%8s', 3), '%8d',
                 rep(c('%12s', '%8d', '%12s', '%12.5f'), n_con))
    write_tbl(tbl = tbl, file_path = file_path, fmt = fmt_con)
  } else if (write_type$type == 'not') {
    stop("'", file_name, "' is not implemented yet!")
  }

}

#' Look up the function type and additional input arguments to read a SWAT+ file.
#'
#' @param file_name Write path of the SWAT+ file.
#'
#' @returns A list of arguments:
#'  - `type` is used to select the write function for the file.
#'  - `n_skip` and `has_unit` as arguments for the function `write_tbl`
#'  - or `def_names`, `par_names` and `id_num` as arguments for the
#'    function `write_tbl2`
#'  - `n_skip` as arguments for the function `write_tbl_n`
#'
#' @keywords internal
#'
lookup_write_type <- function(file_name) {
  file_sfx  <- substr(file_name, nchar(file_name) - 2, nchar(file_name))

  if (file_name %in% c('crop_yld_yr.txt', 'crop_yld_yr.txt',
                       'hydcon.out', 'mgt.out')) {
    stop("'", file_name, "' is not implemented yet!")
  } else if (file_name %in% c('files_out.out', 'flow_duration_curve.out')) {
    file_name <- 'out_without_units'
  } else if (file_sfx %in% c('txt', 'out')) {
    file_name <- 'out_with_units'
  } else if (file_sfx %in% c('pcp', 'tmp', 'hmd', 'slr', 'wnd')) {
    file_name <- 'weather_input'
  }

  write_lookup <- list(
    # Input files
    'aqu_catunit.ele'   = list(type = 'tbl', fmt = c('%8d', '%-16s', '%12s', '%10d', rep('%12.5f', 3))),
    'aqu_cha.lin'       = list(type = 'not'),
    'aquifer.aqu'       = list(type = 'tbl', fmt = c('%8d', '%-16s', '%16s', rep('%12.5f', 15))),
    'aquifer.con'       = list(type = 'con'),
    'atmodep.cli'       = list(type = 'not'),
    'bmpuser.str'       = list(type = 'tbl', fmt = c('%-16s', '%8d', rep('%12.5f', 6))),
    'cal_parms.cal'     = list(type = 'not'), #*** add value when write
    'calibration.cal'   = list(type = 'not'),
    'chandeg.con'       = list(type = 'con'),
    'channel-lte.cha'   = list(type = 'tbl', fmt = c('%8d', '%-16s', rep('%16s', 4))),
    'chem_app.ops'      = list(type = 'tbl', fmt = c('%-16s', '%16s', '%16s',  rep('%12.5f', 5))),
    'cntable.lum'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 4),'%-64s', '%-32s', '%-16s')),
    'codes.bsn'         = list(type = 'tbl', fmt = c('%16s', '%16s', rep('%8s', 22))),
    'cons_practice.lum' = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 2))),
    'exco.con'          = list(type = 'con'),
    'fertilizer.frt'    = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 5), '%16s')),
    'field.fld'         = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 3))),
    'file.cio'          = list(type = 'not'),
    'filterstrip.str'   = list(type = 'tbl', fmt = c('%-16s', '%8d', rep('%12.5f', 3))),
    'fire.ops'          = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 2))),
    'grassedww.str'     = list(type = 'tbl', fmt = c('%-16s', '%8d', rep('%12.5f', 6))),
    'graze.ops'         = list(type = 'tbl', fmt = c('%-16s', '%16s', rep('%12.5f', 4))),
    'harv.ops'          = list(type = 'tbl', fmt = c('%-16s', '%16s', rep('%12.5f', 3))),
    'hmd.cli'           = list(type = 'tbl', fmt = '%s'),
    'hru.con'           = list(type = 'con'),
    'hru-data.hru'      = list(type = 'tbl', fmt = c('%8d', '%-16s', rep('%16s', 8))),
    'hydrology.hyd'     = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 14))),
    'hydrology.res'     = list(type = 'tbl', fmt = c('%-16s', rep('%8d', 2), rep('%12.5f', 8))),
    'hydrology.wet'     = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 10))),
    'hyd-sed-lte.cha'   = list(type = 'tbl', fmt = c('%-16s', '%16s', rep('%12.5f', 21))),
    'initial.aqu'       = list(type = 'tbl', fmt =  c('%-16s', rep('%16s', 5))),
    'initial.cha'       = list(type = 'tbl', fmt = c('%-16s', rep('%16s', 5))),
    'initial.res'       = list(type = 'tbl', fmt = c('%-16s', rep('%16s', 5))),
    'irr.ops'           = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 7))),
    'landuse.lum'       = list(type = 'tbl', fmt = c('%-20s', rep('%16s', 13))),
    'ls_unit.def'       = list(type = 'tbl', fmt = c('%8d', '%16s', '%12.5f', '%8d', '%10d**'), add_lines = 'n_row'),
    'ls_unit.ele'       = list(type = 'tbl', fmt = c('%8d', '%-16s', '%16s', '%10d', rep('%12.5f', 3))),
    'lum.dtl'           = list(type = 'not'),
    'management.sch'    = list(type = 'tbl2',
                               fmt_def = c('%-24s', rep('%9.0f', 2)),
                               fmt_par = c('%16s', rep('%8.0f', 2), '%12.5f',
                                           rep('%16s', 2), '%12.5f')),
    'nutrients.cha'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 19), rep('%8d', 2), rep('%12.5f', 17))),
    'nutrients.res'       = list(type = 'tbl', fmt = c('%-16s', rep('%8d', 2), rep('%12.5f', 10))),
    'nutrients.sol'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 11))),
    'object.cnt'          = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 2), rep('%8d', 18))),
    'object.prt'          = list(type = 'tbl', fmt = c('%12d', '%12s', '%12d',  '%12s',  '%20s')),
    'om_water.ini'        = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 19))),
    'ovn_table.lum'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 3))),
    'parameters.bsn'      = list(type = 'tbl', fmt = c(rep('%12.5f', 43), '%8d')),
    'path_hru.ini'        = list(type = 'not'),
    'pcp.cli'             = list(type = 'tbl', fmt = '%s'),
    'pest_hru.ini'        = list(type = 'not'),
    'pesticide.pes'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 13))),
    'plant.ini'           = list(type = 'tbl2',
                                 fmt_def = c('%-16s', '%8.0f', '%10.0f'),
                                 fmt_par = c('%16s', '%12s', rep('%12.5f', 6))),
    'plants.plt'          = list(type = 'tbl', fmt = c('%16s', '%-16s', '%-16s', rep('%12.5f', 50))),
    'print.prt'           = list(type = 'not'),
    'res_rel.dtl'         = list(type = 'not'),
    'reservoir.con'       = list(type = 'con'),
    'reservoir.res'       = list(type = 'tbl', fmt = c('%8d', '%-16s', rep('%16s', 4))),
    'rout_unit.con'       = list(type = 'con'),
    "rout_unit.def"       = list(type = 'tbl', fmt = c('%8d', '%16s', '%8d', '%10d**')),
    'rout_unit.ele'       = list(type = 'tbl', fmt = c('%8d', '%-16s', '%12s', '%8d', '%12.5f', '%16d')),
    'rout_unit.rtu'       = list(type = 'tbl', fmt = c('%8d', '%-16s', rep('%16s', 4))),
    'salt_aqu.ini'        = list(type = 'not'),
    'salt_channel.ini'    = list(type = 'not'),
    'salt_hru.ini'        = list(type = 'not'),
    'sediment.res'        = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 6))),
    'septic.sep'          = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 9))),
    'septic.str'          = list(type = 'tbl', fmt = c('%-16s', rep('%8d', 9), rep('%12.5f', 2), rep('%8d', 2)), rep('%12.5f', 21)),
    'slr.cli'             = list(type = 'tbl', fmt = '%s'),
    'snow.sno'            = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 8))),
    'soil_plant.ini'      = list(type = 'tbl', fmt = c('%-16s', '%12.5f', rep('%16s', 5))),
    'soils.sol'           = list(type = 'tbl2',
                                 fmt_def = c('%-16s', '%8d', '%8s', rep('%12.5f', 3), '%-16s'),
                                 fmt_par = rep('%12.5f', 14)),
    'sweep.ops'           = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 2))),
    'tiledrain.str'       = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 8))),
    'tillage.til'         = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 5))),
    'time.sim'            = list(type = 'tbl', fmt = rep('%8d', 5)),
    'tmp.cli'             = list(type = 'tbl', fmt = '%s'),
    'topography.hyd'      = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 5))),
    'treatment.trt'       = list(type = 'not'),
    'urban.urb'           = list(type = 'tbl', fmt = c('%-16s', rep('%12.5f', 10))),
    'weather-sta.cli'     = list(type = 'tbl', fmt = c('%-16s', rep('%16s', 8))),
    'weather-wgn.cli'     = list(type = 'not'),
    'wetland.wet'         = list(type = 'tbl', fmt = c('%8s', '%-16s', rep('%16s', 5))),
    'wnd.cli'             = list(type = 'tbl', fmt = '%s'),
    'weather_input'       = list(type = 'tbl', fmt = c('%-4d', '%8d', '%7.3f**'), add_lines = 'header')
  )
  return(write_lookup[[file_name]])
}

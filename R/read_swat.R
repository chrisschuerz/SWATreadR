#' Read SWAT+ input and output files.
#'
#' @param file_path Path to the SWAT+ file.
#'
#' @returns The SWAT+ input or output file in a tibble.
#'
#' @export
#'
read_swat <- function(file_path) {
  file_name <- basename(file_path)
  dir_path  <- dirname(file_path)

  read_type <- lookup_read_type(file_name)

  if (!file_name %in% list.files(path = dir_path)) {
    stop("File with the name '", file_name, "' was not found in\n",
         dir_path)
  }

  if(is.null(read_type[[1]])) {
    stop("File with the name '", file_name, "' is not supported!")
  } else if (read_type$type == 'tbl') {
    tbl <- read_tbl(file_path, n_skip = read_type$n_skip)
  } else if (read_type$type == 'tbl2') {
    tbl <- read_tbl2(file_path = file_path,
                     def_names = read_type$def_names,
                     par_names = read_type$par_names,
                     id_num = read_type$id_num)
  } else if (read_type$type == 'con') {
    tbl <- read_con(file_path)
  } else if (read_type$type == 'line') {
    tbl <- read_linewise(file_path,
                         col_names = read_type$col_names,
                         n_skip = read_type$n_skip)
  } else if (read_type$type == 'not') {
    stop("'", file_name, "' is not implemented yet!")
  }

  return(tbl)

}

#' Look up the function type and additional input arguments to read a SWAT+ file.
#'
#' @param file_name Path to the SWAT+ file.
#'
#' @returns A list of arguments:
#'  - `type` is used to select the read function for the file.
#'  - `n_skip` and `has_unit` as arguments for the function `read_tbl`
#'  - or `def_names`, `par_names` and `id_num` as arguments for the
#'    function `read_tbl2`
#'  - `n_skip` as arguments for the function `read_tbl_n`
#'
#' @keywords internal
#'
lookup_read_type <- function(file_name) {
  file_sfx  <- substr(file_name, nchar(file_name) - 2, nchar(file_name))

  if (file_name %in% c('crop_yld_yr.txt', 'crop_yld_yr.txt',
                       'hydcon.out', 'mgt.out')) {
    stop("'", file_name, "' is not implemented yet!")
  } else if (file_name %in% c('files_out.out', 'flow_duration_curve.out')) {
    file_name <- 'out_without_units'
  } else if (file_sfx %in% c('txt', 'out')) {
    file_name <- 'out_with_units'
  }

  read_lookup <- list(
    # Input files
    'aqu_catunit.ele'   = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'aqu_cha.lin'       = list(type = 'not'),
    'aquifer.aqu'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'aquifer.con'       = list(type = 'con'),
    'atmodep.cli'       = list(type = 'not'),
    'bmpuser.str'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'cal_parms.cal'     = list(type = 'tbl', n_skip = 2), #*** add value when write
    'calibration.cal'   = list(type = 'not'),
    'chandeg.con'       = list(type = 'con', n_skip = 1, has_unit = FALSE),
    'channel-lte.cha'   = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'chem_app.ops'      = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'cntable.lum'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'codes.bsn'         = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'cons_practice.lum' = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'exco.con'          = list(type = 'con', n_skip = 1, has_unit = FALSE),
    'fertilizer.frt'    = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'field.fld'         = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'file.cio'          = list(type = 'not'),
    'filterstrip.str'   = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'fire.ops'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'grassedww.str'     = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'graze.ops'         = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'harv.ops'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hmd.cli'           = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hru.con'           = list(type = 'con', n_skip = 1, has_unit = FALSE),
    'hru-data.hru'      = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hydrology.hyd'     = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hydrology.res'     = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hydrology.wet'     = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'hyd-sed-lte.cha'   = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'initial.aqu'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'initial.cha'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'initial.res'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'irr.ops'           = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'landuse.lum'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'ls_unit.def'       = list(type = 'line',
                               col_names = c('id', 'name', 'area', 'elem_tot', 'elements_*'),
                               n_skip = 2),
    'ls_unit.ele'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'lum.dtl'           = list(type = 'not'),
    'management.sch'    = list(type = 'tbl2',
                               def_names = c('name', 'numb_ops', 'numb_auto'),
                               par_names = c('op_typ', 'mon', 'day', 'hu_sch',
                                             paste0('op_data', 1:3)),
                               id_num    =  c(2:3, 5:7, 10)),
    'nutrients.cha'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'nutrients.res'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'nutrients.sol'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'object.cnt'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'object.prt'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'om_water.ini'        = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'ovn_table.lum'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'parameters.bsn'      = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'path_hru.ini'        = list(type = 'not'),
    'pcp.cli'             = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'pest_hru.ini'        = list(type = 'not'),
    'pesticide.pes'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'plant.ini'           = list(type = 'tbl2',
                                 def_names = c('pcom_name', 'plt_cnt', 'rot_yr_ini'),
                                 par_names = c('plt_name', 'lc_status', 'lai_init',
                                               'bm_init', 'phu_init', 'plnt_pop',
                                               'yrs_init', 'rsd_init'),
                                 id_num    =  c(2:3, 6:11)),
    'plants.plt'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'print.prt'           = list(type = 'not'),
    'res_rel.dtl'         = list(type = 'not'),
    'reservoir.con'       = list(type = 'con'),
    'reservoir.res'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'rout_unit.con'       = list(type = 'con'),
    "rout_unit.def"       = list(type = 'line',
                                 col_names = c('id', 'name', 'elem_tot', 'elements_*'),
                                 n_skip = 1),
    'rout_unit.ele'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'rout_unit.rtu'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'salt_aqu.ini'        = list(type = 'not'),
    'salt_channel.ini'    = list(type = 'not'),
    'salt_hru.ini'        = list(type = 'not'),
    'sediment.res'        = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'septic.sep'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'septic.str'          = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'slr.cli'             = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'snow.sno'            = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'soil_plant.ini'      = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'soils.sol'           = list(type = 'tbl2',
                                 def_names = c('name', 'nly', 'hyd_grp', 'dp_tot',
                                               'anion_excl', 'perc_crk', 'texture'),
                                 par_names = c('dp', 'bd', 'awc', 'soil_k',
                                               'carbon', 'clay', 'silt', 'sand',
                                               'rock', 'alb', 'usle_k', 'ec',
                                               'caco3', 'ph'),
                                 id_num    =  c(2, 4:6, 8:21)),              # !!! Is tbl2 but have to adapt for soil
    'sweep.ops'           = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'tiledrain.str'       = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'tillage.til'         = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'time.sim'            = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'tmp.cli'             = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'topography.hyd'      = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'treatment.trt'       = list(type = 'not'),
    'urban.urb'           = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'weather-sta.cli'     = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'weather-wgn.cli'     = list(type = 'not'),
    'wetland.wet'         = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    'wnd.cli'             = list(type = 'tbl', n_skip = 1, has_unit = FALSE),
    # Output files
    'out_with_units'      = list(type = 'tbl', n_skip = 1, has_unit = TRUE),
    'out_without_units'      = list(type = 'tbl', n_skip = 1, has_unit = TRUE)
    )
  return(read_lookup[[file_name]])
}

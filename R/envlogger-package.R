
### TO DOs ----
# work on plot_env
# add links across functions
# push

# remove data from first day when joining by id
# tides

# mares viagens


#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL

template_metadata <- tibble::tribble(
  ~field, ~rep_env, ~rep_other, ~type, ~info,
  "id"            , TRUE, FALSE,  "chr" , "max 10 chr",
  "serial"        , TRUE, FALSE,  "chr" , "device serial number (typically hexadecimal)",
  "type"          , TRUE, FALSE,  "chr" , "logging device type",
  "v_log"         , TRUE, FALSE,  "dbl" , "logging device version",
  "v_app"         , TRUE, FALSE,  "dbl" , "download software version",
  "tdiff"         , TRUE, FALSE,  "int" , "logger RTC drift in seconds (obs - ref)",
  "dev"           , TRUE, FALSE,  "chr" , "brand and model of device used for download",
  "dev_nm"        , TRUE, FALSE,  "chr" , "id of device used for download",
  "dev_os"        , TRUE, FALSE,  "chr" , "OS of device used for download",

  "skip"          , FALSE, TRUE,  "int" , "skip x lines to get to the first line of data",
  "time_format"   , FALSE, TRUE,  "chr" , "if not standard provide time format (e.g. %m-%d%-%Y %H:%M)",
  "sep_dec_comma" , FALSE, TRUE,  "lgl" , "is the decimal separator a comma?",

  "offset_time"   , FALSE, FALSE, "int" , "shift timestamps by x seconds (pos or neg)",
  "offset_diff"   , FALSE, FALSE, "int" , "shift tdiff by x seconds (pos or neg)",
  "purge_temp_min", FALSE, FALSE, "dbl" , "discard data below x temperature (pos or neg)",
  "purge_temp_max", FALSE, FALSE, "dbl" , "discard data above x temperature (pos or neg)",
  "purge_time_bef", FALSE, FALSE, "dttm", "discard data before x; format = 1970-01-01 00:00:00",
  "purge_time_aft", FALSE, FALSE, "dttm", "discard data after x; format = 1970-01-01 00:00:00",
  "purge_na_time" , FALSE, FALSE, "lgl" , "discard data when timestamps are NA?",
  "purge_na_data" , FALSE, FALSE, "lgl" , "discard data when values are NA?",
  "split_time_gap", FALSE, FALSE, "lgl" , "split data when gaps found?"
) %>%
  tibble::add_column(new_val = NA) %>%
  dplyr::relocate(new_val, .after = field)

metadata_cols_env   <- template_metadata %>%
  dplyr::filter(rep_env) %>%
  dplyr::pull(field)

metadata_cols_other <- template_metadata %>%
  dplyr::filter(rep_other) %>%
  dplyr::pull(field)

metadata_cols_fix   <- template_metadata %>%
  dplyr::filter(!rep_env, !rep_other) %>%
  dplyr::pull(field)

col_types <- tibble::tribble(
  ~type, ~col_type, ~fun,
  "chr",  "character", function(x) as.character(x),
  "dbl",  "double",    function(x) as.numeric(x),
  "list", "list",      function(x) x,
  "lgl",  "logical",   function(x) as.logical(x),
  "dttm", "POSIXct",   function(x) lubridate::as_datetime(as.character(x), format = "%Y-%m-%d %H:%M:%S")
)

envlogger_types <- c("t", "th", "tp", "tw", "tl", "thl")

env_cols <- c("id", "serial", "type", "v_log", "press", "hum", "data", "xts", "nrow", "min", "max", "t0", "t1")

parse_id_standards <- list(
  cctbon = list(
    div = "1111123445",
    fields = list(
      site = "fct",
      lvl  = c("l", "m", "t", "s"),
      exp  = c("c", "h"),
      rep1 = "num",
      rep2 = "chr"
    )
  )
)

summarise_funs <- list(
  min = function(x) min(x, na.rm = TRUE),
  q10 = function(x) stats::quantile(x, 0.10, na.rm = TRUE),
  q25 = function(x) stats::quantile(x, 0.25, na.rm = TRUE),
  avg = function(x) mean(x, na.rm = TRUE),
  q50 = function(x) stats::quantile(x, 0.50, na.rm = TRUE),
  q75 = function(x) stats::quantile(x, 0.75, na.rm = TRUE),
  q90 = function(x) stats::quantile(x, 0.90, na.rm = TRUE),
  max = function(x) max(x, na.rm = TRUE)
)

# list of original files for use in env_example
# env_example(delete_new_metadata_files = TRUE) %>% purrr::list_c() %>% fs::path_file() %>% dput()
example_files_to_keep <- c(
  "log_2024-06-05_063724.csv",
  "log_2025-01-31_093212.csv",
  "log_2024-05-25_095701.csv",
  "log_2024-01-12_082832_bad_col.csv",
  "log_2025-01-14_070601.csv",
  "670000002f037841.txt",
  "4c0000002418fb41.txt",
  "4c0000002418fb41_meta.csv",
  "humidsc01a-0482_ac00_f27d_01-20250506_095113.csv",
  "humidsh01a-04f4_4100_ef28_0b-20250506_095423.csv",
  "improper_file_name.csv",
  "improper_file_name_meta.csv",
  "issuemh04a_0434_ef00_4b29_0a-20240116_132500.csv",
  "issuemh04a_0434_ef00_4b29_0a-20240116_132500_meta.csv",
  "issuemc01a-04ad_e500_302d_06-20240525_101116_no_data.csv",
  "issuemc02a-04b4_3e00_c90f_0e-20240525_101121_low.csv",
  "issuemh01a-04d3_5e00_b818_02-20240525_095727_gap.csv",
  "issuemh02a-0449_0c00_2310_05-20240525_095804_1970.csv",
  "nozzzmc01a-0425_e500_051e_0b-20240605_063737.csv",
  "nozzzmc02a-04da_2800_d33a_0d-20240605_063925.csv",
  "nozzzmh01a-04fd_0b00_3805_0e-20240605_064147.csv",
  "nozzzmh02a-04a5_bb00_521c_0f-20240605_064246.csv",
  "pressmc01b-0405_f000_5411_01-20250131_093703.csv",
  "ptzzwmc01a-04ad_e500_302d_06-20240525_101015.csv",
  "ptzzwmc02a-04b4_3e00_c90f_0e-20240525_101121.csv",
  "ptzzwmh01a-04d3_5e00_b818_02-20240525_095727.csv",
  "ptzzwmh02a-0449_0c00_2310_05-20240525_095804.csv",
  "ptzzymc01a-04ce_ce00_4114_06-20240112_084047.csv",
  "ptzzymc02a-04cb_8f00_e758_05-20240112_083906.csv",
  "ptzzymh01a-04dc_6700_dd59_0f-20240112_082904.csv",
  "ptzzymh02a-04cb_cc00_1507_0c-20240112_083030.csv",
  "ptzzymc01a-04ce_ce00_4114_06-20250114_111522.csv",
  "ptzzymc02a-14cb_8f00_e758_05-20250114_111020.csv",
  "ptzzymh01a-04dc_6700_dd59_0f-20250114_111804.csv",
  "ptzzymh02a-04cb_cc00_1507_0c-20250114_111701.csv",
  "unsupported_file_1.csv",
  "unsupported_file_2.csv",
  "unsupported_file_3.txt"
)

# visible binding for global variables
## append_issues
path = NULL
## cctbon_summarise
sh = lvl = mic = id = temp = NULL
## create_metadata_file
paths = NULL
## create_metadata_file_single
rep_env = rep_other = new_val = NULL
## env_check_qual
t0 = t1 = data = tmin = tmax = f_qual_issues = f_qual_good = NULL
## env_example
met = fn = keep = path = NULL
## env_fix
temp = tdiff = id = serial = t0 = path = NULL
## env_full_days
data = full_days = NULL
## env_generate_fixes
path_meta = NULL
## env_header_val
chr = dbl = txt = NULL
## env_interpolate
data = int_t0 = int_t0_floor = int_t0_margin = int_t0_ceiling = int_t1 = int_t1_ceiling = int_t1_margin = int_t1_floor = corr_t0 = corr_t1 = . = NULL
## env_join_by_id
id = serial = t0 = t1 = int = int_lag = data = press = hum = sgmnt = type = v_log = f_qual_checked = f_drift_correc = f_interpolated = f_bound_serial = NULL
## env_join_by_serial
id = serial = t0 = t1 = data = int = int_lead = . = int_lag = overlap_mins = overlap = type = v_log = press = hum = sgmnt = f_qual_checked = f_drift_correc = f_interpolated = path = NULL
## env_ls
. = l = is_env = int = has_met = is_log = is_met = lgl = path = NULL
## env_rtc_drift
type = dev_os = v_app = v_log = is_android = is_old_app = is_just_t = is_v2.4 = is_other = is_fix = tdiff = data = NULL
## parse_id
i = i1 = i2 = bad = NULL
## plot_env
mic = pressure = id = temp = press = val = type = sh = avg = t0 = t1 = .data = facet_c = facet_r = NULL
## read_env
path_meta = run2 = issue = path_data = step = fn = fixed = data = xts = NULL
## read_env_all
met = rep_env = field = rep_other = id = serial = t0 = int = rep_no_data = path = NULL
## read_env_log
id = time_diff_s = long = code = samp_int_s = samp_res_c = samples = start_time = device = name = time = action = status = serial = v_log = lat = lon = int = res = tdiff = pass = start = dev = NULL
## read_env_metadata
field = new_val = fn = ok = path = NULL
## read_rep_env
data = . = id = serial = t0 = t1 = path = NULL
## read_rep_env_data
time = pressure = press = humidity = NULL
## read_rep_other
tdiff = data = id = serial = t0 = path = NULL


# x <- "cctbon_summarise: no visible binding for global variable ‘sh’
#    cctbon_summarise: no visible binding for global variable ‘lvl’
#    cctbon_summarise: no visible binding for global variable ‘mic’
#    cctbon_summarise: no visible binding for global variable ‘id’
#    cctbon_summarise: no visible binding for global variable ‘temp’
#    parse_id: no visible binding for global variable ‘i1’
#    parse_id: no visible binding for global variable ‘i2’
#    parse_id: no visible binding for global variable ‘i’
#    parse_id: no visible binding for global variable ‘bad’
#    create_metadata_file: no visible binding for global variable ‘paths’
#    create_metadata_file_single: no visible binding for global variable ‘rep_env’
#    create_metadata_file_single: no visible binding for global variable ‘rep_other’
#    create_metadata_file_single: no visible binding for global variable ‘new_val’
#    env_check_qual: no visible binding for global variable ‘t0’
#    env_check_qual: no visible binding for global variable ‘t1’
#    env_check_qual: no visible binding for global variable ‘data’
#    env_check_qual: no visible binding for global variable ‘tmin’
#    env_check_qual: no visible binding for global variable ‘tmax’
#    env_check_qual: no visible binding for global variable ‘f_qual_issues’
#    env_check_qual: no visible binding for global variable ‘f_qual_good’
#    env_example: no visible binding for global variable ‘met’
#    env_example: no visible binding for global variable ‘fn’
#    env_example: no visible binding for global variable ‘keep’
#    env_fix: no visible binding for global variable ‘temp’
#    env_fix: no visible global function definition for ‘head’
#    env_fix: no visible global function definition for ‘tail’
#    env_fix: no visible binding for global variable ‘tdiff’
#    env_fix: no visible binding for global variable ‘id’
#    env_fix: no visible binding for global variable ‘serial’
#    env_fix: no visible binding for global variable ‘t0’
#    env_full_days: no visible binding for global variable ‘data’
#   env_full_days: no visible binding for global variable ‘full_days’
#   env_generate_fixes: no visible binding for global variable ‘path_meta’
#   env_header_val: no visible binding for global variable ‘chr’
#   env_header_val: no visible binding for global variable ‘dbl’
#   env_header_val: no visible binding for global variable ‘txt’
#   env_interpolate: no visible binding for global variable ‘data’
#   env_interpolate: no visible binding for global variable ‘int_t0’
#   env_interpolate: no visible binding for global variable ‘int_t0_floor’
#   env_interpolate: no visible binding for global variable ‘int_t0_margin’
#   env_interpolate: no visible binding for global variable ‘int_t0_ceiling’
#   env_interpolate: no visible binding for global variable ‘int_t1’
#   env_interpolate: no visible binding for global variable ‘int_t1_ceiling’
#   env_interpolate: no visible binding for global variable ‘int_t1_margin’
#   env_interpolate: no visible binding for global variable ‘int_t1_floor’
#   env_interpolate: no visible binding for global variable ‘corr_t0’
#   env_interpolate: no visible binding for global variable ‘corr_t1’
#   env_interpolate: no visible binding for global variable ‘.’
#   env_join_by_id: no visible binding for global variable ‘id’
#   env_join_by_id: no visible binding for global variable ‘serial’
#   env_join_by_id: no visible binding for global variable ‘t0’
#   env_join_by_id: no visible binding for global variable ‘t1’
#   env_join_by_id: no visible binding for global variable ‘int’
#   env_join_by_id: no visible binding for global variable ‘int_lag’
#   env_join_by_id: no visible binding for global variable ‘data’
#   env_join_by_id: no visible binding for global variable ‘press’
#   env_join_by_id: no visible binding for global variable ‘hum’
#   env_join_by_id: no visible binding for global variable ‘sgmnt’
#   env_join_by_id: no visible binding for global variable ‘type’
#   env_join_by_id: no visible binding for global variable ‘v_log’
#   env_join_by_id: no visible binding for global variable ‘f_qual_checked’
#   env_join_by_id: no visible binding for global variable ‘f_drift_correc’
#   env_join_by_id: no visible binding for global variable ‘f_interpolated’
#   env_join_by_id: no visible binding for global variable ‘f_bound_serial’
#   env_join_by_serial: no visible binding for global variable ‘id’
#   env_join_by_serial: no visible binding for global variable ‘serial’
#   env_join_by_serial: no visible binding for global variable ‘t0’
#   env_join_by_serial: no visible binding for global variable ‘t1’
#   env_join_by_serial: no visible binding for global variable ‘data’
#   env_join_by_serial: no visible binding for global variable ‘int’
#   env_join_by_serial: no visible binding for global variable ‘int_lead’
#   env_join_by_serial: no visible binding for global variable ‘.’
#   env_join_by_serial: no visible binding for global variable ‘int_lag’
#   env_join_by_serial: no visible binding for global variable ‘overlap_mins’
#   env_join_by_serial: no visible binding for global variable ‘overlap’
#   env_join_by_serial: no visible binding for global variable ‘type’
#   env_join_by_serial: no visible binding for global variable ‘v_log’
#   env_join_by_serial: no visible binding for global variable ‘press’
#   env_join_by_serial: no visible binding for global variable ‘hum’
#   env_join_by_serial: no visible binding for global variable ‘sgmnt’
#   env_join_by_serial: no visible binding for global variable ‘f_qual_checked’
#   env_join_by_serial: no visible binding for global variable ‘f_drift_correc’
#   env_join_by_serial: no visible binding for global variable ‘f_interpolated’
#   env_ls: no visible binding for global variable ‘.’
#   env_ls: no visible binding for global variable ‘l’
#   env_ls: no visible binding for global variable ‘is_env’
#   env_ls: no visible binding for global variable ‘int’
#   env_ls: no visible binding for global variable ‘has_met’
#   env_ls: no visible binding for global variable ‘is_log’
#   env_ls: no visible binding for global variable ‘is_met’
#   env_ls: no visible binding for global variable ‘lgl’
#   env_rtc_drift: no visible binding for global variable ‘type’
#   env_rtc_drift: no visible binding for global variable ‘dev_os’
#   env_rtc_drift: no visible binding for global variable ‘v_app’
#   env_rtc_drift: no visible binding for global variable ‘v_log’
#   env_rtc_drift: no visible binding for global variable ‘is_android’
#   env_rtc_drift: no visible binding for global variable ‘is_old_app’
#   env_rtc_drift: no visible binding for global variable ‘is_just_t’
#   env_rtc_drift: no visible binding for global variable ‘is_v2.4’
#   env_rtc_drift: no visible binding for global variable ‘is_other’
#   env_rtc_drift: no visible binding for global variable ‘is_fix’
#   env_rtc_drift: no visible binding for global variable ‘tdiff’
#   env_rtc_drift: no visible binding for global variable ‘data’
#   plot_env: no visible global function definition for ‘READ_ENV’
#   plot_env: no visible binding for global variable ‘mic’
#   plot_env: no visible binding for global variable ‘pressure’
#   plot_env: no visible binding for global variable ‘id’
#   plot_env: no visible binding for global variable ‘temp’
#   plot_env: no visible binding for global variable ‘press’
#   plot_env: no visible binding for global variable ‘val’
#   plot_env: no visible binding for global variable ‘type’
#   plot_env: no visible binding for global variable ‘sh’
#   plot_env: no visible binding for global variable ‘avg’
#   plot_env: no visible binding for global variable ‘t0’
#   plot_env: no visible binding for global variable ‘t1’
#   plot_env: no visible binding for global variable ‘.data’
#   plot_env: no visible binding for global variable ‘facet_c’
#   plot_env: no visible binding for global variable ‘facet_r’
#   read_env: no visible binding for global variable ‘path_meta’
#   read_env: no visible binding for global variable ‘run2’
#   read_env: no visible binding for global variable ‘issue’
#   read_env: no visible binding for global variable ‘path_data’
#   read_env: no visible binding for global variable ‘step’
#   read_env: no visible binding for global variable ‘fn’
#   read_env: no visible binding for global variable ‘fixed’
#   read_env: no visible binding for global variable ‘data’
#   read_env: no visible binding for global variable ‘xts’
#   read_env_all: no visible binding for global variable ‘met’
#   read_env_all: no visible binding for global variable ‘rep_env’
#   read_env_all: no visible binding for global variable ‘field’
#   read_env_all: no visible binding for global variable ‘rep_other’
#   read_env_all: no visible binding for global variable ‘id’
#   read_env_all: no visible binding for global variable ‘serial’
#   read_env_all: no visible binding for global variable ‘t0’
#   read_env_all: no visible binding for global variable ‘int’
#   read_env_all: no visible binding for global variable ‘rep_no_data’
#   read_env_log: no visible binding for global variable ‘id’
#   read_env_log: no visible binding for global variable ‘time_diff_s’
#   read_env_log: no visible binding for global variable ‘long’
#   read_env_log: no visible binding for global variable ‘code’
#   read_env_log: no visible binding for global variable ‘samp_int_s’
#   read_env_log: no visible binding for global variable ‘samp_res_c’
#   read_env_log: no visible binding for global variable ‘samples’
#   read_env_log: no visible binding for global variable ‘start_time’
#   read_env_log: no visible binding for global variable ‘device’
#   read_env_log: no visible binding for global variable ‘name’
#   read_env_log: no visible binding for global variable ‘time’
#   read_env_log: no visible binding for global variable ‘action’
#   read_env_log: no visible binding for global variable ‘status’
#   read_env_log: no visible binding for global variable ‘serial’
#   read_env_log: no visible binding for global variable ‘v_log’
#   read_env_log: no visible binding for global variable ‘lat’
#   read_env_log: no visible binding for global variable ‘lon’
#   read_env_log: no visible binding for global variable ‘int’
#   read_env_log: no visible binding for global variable ‘res’
#   read_env_log: no visible binding for global variable ‘tdiff’
#   read_env_log: no visible binding for global variable ‘pass’
#   read_env_log: no visible binding for global variable ‘start’
#   read_env_log: no visible binding for global variable ‘dev’
#   read_env_metadata: no visible binding for global variable ‘field’
#   read_env_metadata: no visible binding for global variable ‘new_val’
#   read_env_metadata: no visible binding for global variable ‘fn’
#   read_env_metadata: no visible binding for global variable ‘ok’
#   read_rep_env: no visible binding for global variable ‘data’
#   read_rep_env: no visible binding for global variable ‘.’
#   read_rep_env: no visible binding for global variable ‘id’
#   read_rep_env: no visible binding for global variable ‘serial’
#   read_rep_env: no visible binding for global variable ‘t0’
#   read_rep_env: no visible binding for global variable ‘t1’
#   read_rep_env_data: no visible binding for global variable ‘time’
#   read_rep_env_data: no visible binding for global variable ‘pressure’
#   read_rep_env_data: no visible binding for global variable ‘press’
#   read_rep_env_data: no visible binding for global variable ‘humidity’
#   read_rep_other: no visible binding for global variable ‘tdiff’
#   read_rep_other: no visible global function definition for ‘tail’
#   read_rep_other: no visible binding for global variable ‘data’
#   read_rep_other: no visible binding for global variable ‘id’
#   read_rep_other: no visible binding for global variable ‘serial’
#   read_rep_other: no visible binding for global variable ‘t0’
#   t_check: no visible global function definition for ‘median’
#   append_issues: no visible binding for global variable ‘path’
#   env_example: no visible binding for global variable ‘path’
#   env_fix: no visible binding for global variable ‘path’
#   env_fix: no visible global function definition for ‘head’
#   env_fix: no visible global function definition for ‘tail’
#   env_join_by_serial: no visible binding for global variable ‘path’
#   env_ls: no visible binding for global variable ‘path’
#   plot_env: no visible global function definition for ‘READ_ENV’
#   read_env_all: no visible binding for global variable ‘path’
#   read_env_metadata: no visible binding for global variable ‘path’
#   read_rep_env: no visible binding for global variable ‘path’
#   read_rep_other: no visible binding for global variable ‘path’
#   read_rep_other: no visible global function definition for ‘tail’
#   t_check: no visible global function definition for ‘median’" %>%
#   stringr::str_split_1("\\n") %>%
#   stringr::str_subset("no visible global function definition for", negate = TRUE) %>%
#   stringr::str_split(":") %>%
#   tibble::tibble(
#     fun = purrr::map_chr(., 1),
#     val = purrr::map_chr(., 2)
#   ) %>%
#   dplyr::select(-1) %>%
#   dplyr::mutate(
#     fun = stringr::str_trim(fun) %>% stringr::str_c("## ", .),
#     val = val %>%
#       stringr::str_remove("’") %>%
#       stringr::str_split("‘") %>%
#       purrr::map_chr(2) %>%
#       stringr::str_trim()
#   ) %>%
#   dplyr::group_by(fun) %>%
#   dplyr::summarise(val = val %>% stringr::str_c(collapse = " = ") %>% stringr::str_c(" = NULL"))
# vec <- vector(mode = "character", length = nrow(x) * 2)
# vec[seq(1, length(vec), by = 2)] <- x$fun
# vec[seq(2, length(vec), by = 2)] <- x$val
# writeLines(vec, con = "/Users/ruiseabra/Desktop/vec.txt")


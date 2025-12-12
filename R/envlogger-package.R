# load_all()
# check()
# install()
# spelling::spell_check_package()

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL

# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("envlogger loaded. See ?envlogger for help.")
# }

# Tidy-eval / NSE symbols used across the package
# %>% stringr::str_split(" = ") %>% purrr::list_c() %>% stringr::str_trim() %>% stringr::str_subset("NULL", negate = TRUE) %>% unique() %>% sort() %>% dput()
utils::globalVariables(c(
  ".", "action", "avg", "bad", "chr", "code", "col_val", "corr_t0", "corr_t1", "d", "data", "dbl", "dev", "dev_os", "device", "f_bound_serial", "f_drift_correc", "f_interpolated", "f_qual_checked", "f_qual_good", "f_qual_issues", "facet_c", "facet_r", "field", "fixed", "fn", "focus", "full_days", "group", "has_met", "hum", "humidity", "i", "i1", "i2", "id", "id_original", "int", "int_lag", "int_lead", "int_t0", "int_t0_ceiling", "int_t0_floor", "int_t0_margin", "int_t1", "int_t1_ceiling", "int_t1_floor", "int_t1_margin", "is_android", "is_env", "is_fix", "is_just_t", "is_log", "is_met", "is_old_app", "is_other", "is_v2.4", "issue", "keep", "l", "lat", "lgl", "lon", "long", "lvl", "met", "mic", "n", "name", "new_val", "ok", "overlap", "overlap_mins", "pass", "path", "path_data", "path_meta", "paths", "press", "pressure", "rep_env", "rep_no_data", "rep_other", "res", "run2", "samp_int_s", "samp_res_c", "samples", "serial", "sgmnt", "sh", "start", "start_time", "status", "step", "t0", "t1", "tdiff", "temp", "time", "time_diff_s", "tmax", "tmin", "txt", "type", "v_app", "v_log", "val", "xts", "ymax", "ymin"
))


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
  # "purge_na_time" , FALSE, FALSE, "lgl" , "discard data when timestamps are NA?",
  # "purge_na_data" , FALSE, FALSE, "lgl" , "discard data when values are NA?",
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

env_cols <- c("id", "serial", "type", "v_log", "press", "hum", "data", "nrow", "min", "max", "t0", "t1")

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

plot_cols <- list(
  lvl = c(
    "l" = "#3B99B1",
    "m" = "#9FC095",
    "t" = "#E8A419",
    "s" = "#F5191C"
  ),

  exp = c(
    "c" = "#3B99B1",
    "h" = "#F5191C",
    "e" = "#E9B31F",
    "w" = "#7CBA96",
    "p" = "black"
  ),

  lvl_exp = c(
    "lc" = "#99BFEF",
    "mc" = "#5295D4",
    "tc" = "#0066A5",
    "sc" = "#00366C",
    "lh" = "#F6B00B",
    "mh" = "#EF7000",
    "th" = "#CA2700",
    "sh" = "#7D0025"
  )
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
  "pressmc01b-0405_f000_5411_01-20250131_093703.csv",
  "improper_file_name_meta.csv",
  "improper_file_name.csv",
  "issuemh04a_0434_ef00_4b29_0a-20240116_132500_meta.csv",
  "issuemh04a_0434_ef00_4b29_0a-20240116_132500.csv",
  "issuemc01a-04ad_e500_302d_06-20240525_101116_no_data.csv",
  "issuemc02a-04b4_3e00_c90f_0e-20240525_101121_low.csv",
  "issuemh01a-04d3_5e00_b818_02-20240525_095727_gap.csv",
  "issuemh02a-0449_0c00_2310_05-20240525_095804_1970.csv",
  "nozzzmc01a-0425_e500_051e_0b-20240605_063737.csv",
  "nozzzmc02a-04da_2800_d33a_0d-20240605_063925.csv",
  "nozzzmh01a-04fd_0b00_3805_0e-20240605_064147.csv",
  "nozzzmh02a-04a5_bb00_521c_0f-20240605_064246.csv",
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



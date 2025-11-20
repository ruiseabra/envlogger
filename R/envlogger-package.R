#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
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

metadata_cols_env <- template_metadata %>%
  dplyr::filter(rep_env) %>%
  dplyr::pull(field)

metadata_cols_other <- template_metadata %>%
  dplyr::filter(rep_other) %>%
  dplyr::pull(field)

metadata_cols_fix <- template_metadata %>%
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

isdir <- path <- type <- id <- press <- pressure <- serial <- serials <- data <- overlap <- t0 <- t1 <- sh <- lvl <- mic <- temp <- quantile <- approx <- val <- avg <- .data <- facet_c <- facet_r <- time <- v_app <- accuracy <- code <- start_time <- time_diff_s <- long <- device <- name <- action <- status <- v <- samp_int_s <- samp_res_c <- dev <- NULL

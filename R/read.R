#' Get paths to example files
#'
#' @description
#' `envlogger-package` comes bundled with several sample files in its inst/extdata directory. These data consist of EnvLogger files covering several years of data from two sites in northern Portugal. This function make them easy to access for use in examples.
#'
#' @param patterns character vectors, defaults to `NULL`; patterns to select one or more example files. `patterns` is vectorized, so more than one value can be supplied. If set to `NULL`, all example files are returned.
#' @param no_logs logical, defaults to `FALSE`; if `TRUE`, EnvLogger log files are omitted.
#' @param dir logical, defaults to `FALSE`; if `TRUE`, only the shore folder paths are returned.
#'
#' @return
#' The full path to one or more example files, or the full paths to all example files available.
#'
#' @export
#'
#' @examples
#' # Get the file paths of all example files
#' env_example()
#'
#' # Get the full paths to a target serial
#' env_example("04CB_CC00_1507_0C")
#'
#' # Get the full paths to any example files matching a search string
#' env_example("20250114")
#'
#' # Get only log files
#' env_example("/log_")
#'
#' # Get no log files
#' env_example(no_logs = TRUE)
#'
#' # Get only folder paths
#' env_example(dir = TRUE)
#'
#' # 'env_example()' is vectorized, meaning that multiple search strings can be used at once
#' env_example(c("ptzzy", "nozzz"))
env_example <- function(
    patterns = NULL,
    no_logs  = FALSE,
    dir      = FALSE
    ) {
  folder <- system.file("extdata", package = "envlogger")
  files  <- dir(folder, recursive = TRUE, full.names = TRUE)
  if (!is.null(patterns)) {
    files <- unlist(purrr::map(patterns, ~stringr::str_subset(files, .x)))
  }
  if (no_logs) {
    files <- stringr::str_subset(files, "/log_", negate = TRUE)
  }
  if (dir) {
    files <- dirname(dirname(files)) %>% unique()
  }
  files
}

#' Check if a path points to an EnvLogger file, logfile, corrections file or none of the three
#'
#' @description
#' Determine if the paths provided point to EnvLogger or related files.
#' This is done by checking for the presence of certain strings in the header of each file (such as "www.electricblue.eu, Portugal").
#' As a result, reports generated using the the earliest versions of EnvLogger_Viewer will not be recognized.
#' Also, if the structure of a file has been changed (e.g., by opening and saving in Excel), it may no longer be recognized as a report or logfile.
#'
#' @param paths character vectors representing the file paths to one or several files
#'
#' @return
#' Numeric vector indicating the type of file encountered: `1` if an EnvLogger file, `2` if an EnvLogger logfile, `3` if an user-produced rtc correction file, and `0` if none of the three
#'
#' @export
#'
#' @examples
#' path <- env_example(no_logs = TRUE)[1]
#' is_envlogger(path) # 1
#'
#' path <- env_example("log_")[1]
#' is_envlogger(path) # 2
is_envlogger <- function(
    paths
    ) {
  if (any(file.info(paths)$isdir)) cli::cli_abort("paths must only point to files (not folders)")

  x <- purrr::map(paths, ~.x %>%
                    readr::read_lines(n_max = 20) %>%
                    stringr::str_to_lower())

  has_eb         <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "www.electricblue.eu")))
  has_envlogger  <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "envlogger")))
  has_tap        <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "tap logger")))
  has_download   <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "data downloaded")))
  has_mission    <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "mission running")))
  has_waiting    <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "waiting to start")))
  is_corrections <- purrr::map_lgl(x, ~any(stringr::str_detect(.x, "corrections")))

  # combine
  val <- rep(0, length(x))
  val[has_eb & has_envlogger] <- 1
  val[has_tap | has_download | has_mission | has_waiting] <- 2
  val[is_corrections] <- 3

  # return
  val
}

#' Get the value of a given field in an Envlogger report header
#'
#' @param header a tibble with columns `field` and `val` and all values in lower case
#' @param field_pattern character string that must match exactly only one entry in the `field` column
#' @param force_numeric logical, defaults to `FALSE`; if `TRUE`, the value is stripped of non-numeric characters and converted to numeric.
#'
#' @return
#' The value for the target field, as string or numeric (if `force_numeric = TRUE`).
#'
#' @export
#'
#' @examples
#' header <- tibble::tibble(field = c("field1", "field2", "field3"), val = c("a", "1", "a1"))
#' env_header_val(header, "field1")
#' env_header_val(header, "field2", force_numeric = TRUE)
#' env_header_val(header, "field2", force_numeric = TRUE)
env_header_val <- function(
    header,
    field_pattern,
    force_numeric = FALSE
    ) {
  val <- header$val[header$field == field_pattern]
  if (length(val) != 1) stop("'field_pattern' must match no more and no less than 1 time")
  if (force_numeric) val <- val %>% stringr::str_remove_all("[^-0-9.]") %>% as.numeric()
  val
}

#' Read the header of an EnvLogger report
#'
#' @description
#' Given a file path, check if it points to an EnvLogger report and, if `TRUE`, read just the header
#'
#' @param path path to an EnvLogger report
#' @param check logical, defaults to `TRUE`; whether or not to check if `path` points to an EnvLogger report. Only meant to be `FALSE` when called from within `read_env_all()`, so as not to repeat the same check multiple times. **Do not set it to `FALSE` when running this function directly.**
#'#'
#' @return
#' A tibble with 1 row and 12 columns with the metadata values for most of the fields recorded in the header.
#'
#' @export
#'
#' @seealso [read_env_data()], [read_env_all()], [plot_env()]
#'
#' @examples
#' path <- env_example(no_logs = TRUE)[1]
#' read_env_header(path)
read_env_header <- function(
    path,
    check = TRUE
    ) {
  if (check) {
    not_report <- is_envlogger(path) != 1
    if (not_report) cli::cli_abort("not an EnvLogger report")
  }

  x <- readLines(path, n = 30)

  skip <- max(stringr::str_which(x, "------------------------------"))

  header <- readr::read_csv(
    path,
    n_max = skip,
    col_names = c("field", "val"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_to_lower))

  # fix fields that were named differently or were missing on earlier versions
  header$field[header$field == "time diff (sec)"] <- "time diff [logger-smartphone](sec)"
  header$field[header$field == "time diff [logger-smartphone](s)"] <- "time diff [logger-smartphone](sec)"
  if (!any(header$field == "device")) header <- tibble::add_row(header, field = "device", val = "missing")
  if (!any(header$field == "custom name")) header <- tibble::add_row(header, field = "custom name", val = "missing")
  if (is.na(env_header_val(header, "envlogger viewer version", TRUE))) header$val[header$field == "envlogger viewer version"] <- 0

  header <- tibble::tibble(
    id       = env_header_val(header, "custom name"),
    serial   = env_header_val(header, "serial number"),
    v_log    = env_header_val(header, "envlogger version"),
    v_app    = env_header_val(header, "envlogger viewer version", TRUE),
    pressure = any(grepl("pressure", header$field)),
    path     = path,
    fn       = basename(path),
    nrow     = length(x) - skip - 1,
    skip     = skip,
    lat      = env_header_val(header, "lat", TRUE),
    lon      = env_header_val(header, "long", TRUE),
    res      = env_header_val(header, "sampling resolution", TRUE),
    tdiff    = env_header_val(header, "time diff [logger-smartphone](sec)", TRUE),
    dev_type = env_header_val(header, "downloading device type"),
    dev_make = env_header_val(header, "device"),
    dev_name = if (v_app > 5) env_header_val(header, "device name") else "",
    download = env_header_val(header, "downloading mode")
  )

  # return
  header
}

#' Read the data from an EnvLogger report
#'
#' @description
#' Given a file path, check if it points to an EnvLogger file and, if `TRUE`, read just the data
#'
#' @inheritParams read_env_header
#' @param skip numeric; a value indicating how many rows to skip when reading the target EnvLogger report to skip the header (usually, the `$skip` column of the output from `read_env_header()`).
#' @param pressure logical, defaults to `FALSE`; indicates if the data being read includes a pressure field.
#' @param zero_secs logical, defaults to `FALSE`; whether to remove trailing seconds (adjustment determined based on the first timestamp).
#'
#' @return
#' A tibble with 2 columns (`t` and `temp`).
#'
#' @export
#'
#' @seealso [read_env_header()], [read_env_all()], [plot_env()]
#'
#' @examples
#' path <- env_example(no_logs = TRUE)[1]
#' read_env_header(path)
read_env_data <- function(
    path,
    skip,
    pressure  = FALSE,
    zero_secs = FALSE,
    check     = TRUE
    ) {
  if (check) {
    not_report <- is_envlogger(path) != 1
    if (not_report) cli::cli_abort("not an EnvLogger report")
  }

  data <- readr::read_csv(
    path,
    skip = skip,
    show_col_types = FALSE
  ) %>%
    tidyr::drop_na() %>%
    dplyr::rename(t = time)

  if (pressure) {
    data <- data %>%
      dplyr::rename(press = pressure) %>%
      dplyr::mutate(press = press * 0.010197)
  }

  if (zero_secs & nrow(data)) data <- dplyr::mutate(data, t = t - lubridate::second(t[1]))

  # return
  return(data)
}

#' Read an EnvLogger logfile
#'
#' @description
#' Given a file path, check if it points to an EnvLogger logfile and, if `TRUE`, read it.
#'
#' @inheritParams read_env_header
#' @param path path to an EnvLogger logfile
#' @param log_summary logical, defaults to `FALSE`; if set to `TRUE`, a summary of the serials included in the logfile is returned instead.
#'
#' @return
#' A tibble with one row for each interaction with an EnvLogger and 15 columns with relevant metadata.
#'
#' @export
#'
#' @seealso [read_env_header()], [read_env_data()], [read_env_all()], [plot_env()]
#'
#' @examples
#' path <- env_example("log_")[1]
#' read_env_log(path)
read_env_log <- function(
    path,
    log_summary = FALSE,
    check = TRUE
) {
  if (check) {
    not_report <- is_envlogger(path) != 2
    if (not_report) cli::cli_abort("not an EnvLogger logfile")
  }

  cols <- path %>%
    readr::read_lines(n_max = 1) %>%
    stringr::str_split_1(",") %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^[:alpha:]_]", "") %>%
    stringr::str_replace_all("sampling", "samp") %>%
    stringr::str_replace_all("interval", "int") %>%
    stringr::str_replace_all("resulotion", "res") %>%
    stringr::str_replace_all("resolution", "res")

  log <- path %>%
    readr::read_csv(
      skip = 1,
      col_names = cols,
      show_col_types = FALSE) %>%
    dplyr::select(-accuracy) %>%
    dplyr::mutate(
      version = version %>% stringr::str_replace_all("[[:alpha:]_]", "") %>% as.numeric(),
      code = as.character(code),
      time = stringr::str_sub(time, 1, 19) %>% as.POSIXct(),
      start_time = stringr::str_sub(start_time, 1, 19) %>% as.POSIXct()
    )

  if (!("device" %in% cols))  log <- tibble::add_column(log, device = "missing")

  log <- log %>%
    dplyr::rename(
      serial = id,
      diff   = time_diff_s,
      lon    = long,
      v      = version,
      start  = start_time,
      dev    = device,
      id     = name
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_to_lower)) %>%
    dplyr::mutate(path = path) %>%
    dplyr::relocate(time, id, serial, action, status)

  if (log_summary) {
    log <- log %>%
      dplyr::arrange(time) %>%
      dplyr::group_by(serial) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::arrange(time) %>%
      dplyr::select(time, id, serial, v, code, int = samp_int_s, res = samp_res_c, dev)
  }

  # return
  log
}


#' Read an RTC corrections file
#'
#' @description
#' Given a file path, check if it points to an user-provided, correctly formatted RTC corrections file and, if `TRUE`, read it.
#'
#' @inheritParams read_env_header
#' @param path path to an user-provided RTC corrections file
#'
#' @return
#' A tibble with RTC correction parameters.
#'
#' @export
#'
#' @seealso [read_env_header()], [read_env_data()], [read_env_all()]
#'
#' @examples
#' path <- env_example("corrr_")[1]
#' read_env_rtc(path)
read_env_rtc <- function(
    path,
    check = TRUE
) {
  if (check) {
    not_report <- is_envlogger(path) != 3
    if (not_report) cli::cli_abort("not an RTC corrections file")
  }

  rtc <- path %>%
    readr::read_csv(
      skip = 2,
      show_col_types = FALSE) %>%
    dplyr::mutate(
      site   = stringr::str_to_lower(site),
      serial = stringr::str_to_lower(serial)
      ) %>%
    tibble::add_column(path = path)

  # confirm that the corrections file is consistent:
  # --> only one date on values
  # --> date on filename == date on folder == date on values
  dates   <- unique(rtc$date)
  date_1  <- dplyr::first(dates)
  date_fn <- path %>%
    basename() %>%
    stringr::str_remove_all("[^0-9]") %>%
    as.Date(format = "%Y%m%d")

  if (length(dates) != 1 | date_1 != date_fn) cli::cli_abort(c("RTC corrections file dates aren't consistent: ", path))

  # return
  rtc
}


#' Read an EnvLogger report or logfile
#'
#' @description
#' Given a file path, check if it points to an EnvLogger report or logfile and, if `TRUE`, read it.
#'
#' @param path path to an EnvLogger report or logfile
#' @param read_data logical, defaults to `TRUE`; whether to also read the data available on EnvLogger reports or to skip that step (quicker).
#' @inheritParams read_env_header
#' @inheritParams read_env_data
#' @inheritParams read_env_log
#'
#' @seealso [read_env_header()], [read_env_data()], [read_env_log()], [plot_env()]
#'
#' @return
#' If path points to an EnvLogger report, a tibble with 1 row and several columns, among which `id` and `data`. If path points to an EnvLogger logfile, a tibble with one row for each interaction with an EnvLogger and several columns with relevant metadata.
#'
#' @export
#'
#' @examples
#' path <- env_example(no_logs = TRUE)[1]
#' read_env_all(path, zero_secs = TRUE) # an EnvLogger file
#'
#' path <- env_example("log_")[1]
#' read_env_all(path) # an EnvLogger logfile
#'
#' path <- env_example("corrr_")[1]
#' read_env_all(path) # an RTC corrections file
read_env_all <- function(
    path,
    zero_secs   = FALSE,
    read_data   = TRUE,
    log_summary = FALSE
    ) {

  env_status <- is_envlogger(path)
  if (!env_status) cli::cli_abort("not an EnvLogger report or logfile")

  if (env_status == 1) {
    header <- read_env_header(path, check = FALSE)

    if (read_data) {
      data <- read_env_data(path,
                              skip = header$skip,
                              pressure = header$pressure,
                              zero_secs = zero_secs,
                              check = FALSE)

      if (nrow(data)) {
        MIN <- min(data$temp, na.rm = TRUE)
        MAX <- max(data$temp, na.rm = TRUE)
        T0  <- min(data$t) %>% as.Date()
        T1  <- max(data$t) %>% as.Date()
      } else {
        MIN <- NA
        MAX <- NA
        T0  <- NA
        T1  <- NA
      }

      header$min <- MIN
      header$max <- MAX
      header$t0  <- T0
      header$t1  <- T1

      # tidy
      header$data <- list(data)
      header <- dplyr::relocate(header, id, serial, data, min, max, t0, t1)
    }
    out <- header
  } else if (env_status == 2) {
    out <- read_env_log(path, log_summary = log_summary, check = FALSE)
  } else {
    out <- read_env_rtc(path, check = FALSE)
  }

  # return
  out
}

#' Correct RTC drift
#'
#' @description
#' Perform linear interpolation to tidy EnvLogger data so that there's one reading every hour (00 mins, 00 secs) or other specified interval; !ALWAYS! apply before binding data from different reports, as otherwise the interpolation will generate artificial data filling any gap that may be present.
#'
#' @param dat a tibble with time (`t`) and temperature (`temp`), as the `$data` column in the output from [read_env_all()]
#' @param tdiff numeric; difference in seconds between logger RTC time (obs) and smartphone time (ref) as `(obs - ref)`; if obs is ticking quicker than ref, `tdiff` is positive.
#'
#' @seealso [read_env_all()]
#'
#' @return
#' The same tibble, but now with timestamps adjusted
#'
#' @export
#'
#' @examples
#' path  <- env_example(no_logs = TRUE)[1]
#' dat   <- read_env_all(path)$data[[1]]
#' tdiff <- read_env_all(path)$tdiff[1]
#' env_rtc_drift(dat, tdiff)
env_rtc_drift <- function(
    dat,
    tdiff
) {
  t0 <- dat$t %>% dplyr::first()
  t1 <- dat$t %>% dplyr::last()
  t1 <- t1 - tdiff

  dat$t <- seq(t0, t1, length.out = nrow(dat))

  dat
}

#' Interpolate EnvLogger data
#'
#' @description
#' Perform linear interpolation to tidy EnvLogger data so that there's one reading every hour (00 mins, 00 secs) or other specified interval; !ALWAYS! apply before binding data from different reports, as otherwise the interpolation will generate artificial data filling any gap that may be present.
#'
#' @param dat a tibble with time (`t`) and temperature (`temp`), as the `$data` column in the output from [read_env_all()]
#' @param freq_mins numeric, defaults to `60`; interpolation interval in minutes.
#' @param margin numeric, defaults to `300`; if t0 is less than `margin` seconds after the rounded hour, t0 for interpolation is set to the rounded hour; otherwise, t0 is set to the next rounded hour. The equivalent is done for t1.
#' @param stop_if_error logical, defaults to `TRUE`; when there isn't enough data to interpolate to the provided freq_mins, should the operation terminate with an error, or issue a warning and output an empty tibble?
#' @param dataset_has_pressure logical, defaults to `FALSE`; set to TRUE if `dat` includes pressure data
#'
#' @seealso [read_env_all()]
#'
#' @return
#' The same tibble, but now with readings every freq_mins
#'
#' @export
#'
#' @examples
#' path <- env_example(no_logs = TRUE)[1]
#' dat  <- read_env_all(path)$data[[1]]
#' env_interpolate(dat)
env_interpolate <- function(
    dat,
    freq_mins = 60,
    margin = 5 * 60,
    stop_if_error = TRUE,
    dataset_has_pressure = FALSE
) {
  INT <- paste(freq_mins, "mins")

  t0 <- dat$t %>% dplyr::first()
  t0_rounded <- lubridate::floor_date(t0, INT)
  t0_margin  <- t0 - t0_rounded
  units(t0_margin) <- "secs"
  t0_margin <- as.numeric(t0_margin)
  t0 <- if (t0_margin < margin) {
    t0_rounded
  } else {
    lubridate::ceiling_date(t0, INT)
  }

  t1 <- dat$t %>% dplyr::last()
  t1_rounded <- lubridate::ceiling_date(t1, INT)
  t1_margin  <- t1_rounded - t1
  units(t1_margin) <- "secs"
  t1_margin <- as.numeric(t1_margin)
  t1 <- if (t1_margin < margin) {
    t1_rounded
  } else {
    lubridate::floor_date(t1, INT)
  }

  this_dat_has_pressure <- any(colnames(dat) == "press")

  if (t0 < t1) {
    t_new <- seq.POSIXt(
      from = t0,
      to   = t1,
      by   = freq_mins * 60
    )

    temp <- stats::approx(
      x    = dat$t,
      y    = dat$temp,
      xout = t_new,
      method = "linear",
      rule = 2
    )$y

    press <- if (this_dat_has_pressure) {
      stats::approx(
        x    = dat$t,
        y    = dat$press,
        xout = t_new,
        method = "linear",
        rule = 2
      )$y
    } else {
      rep(NA, length(t_new))
    }

    x <- tibble::tibble(
      t     = t_new,
      temp  = temp,
      press = press
    )
    if (!dataset_has_pressure) x <- dplyr::select(x, -press)
    x
  } else {
    msg <- "not enough data to interpolate to the provided freq_mins"
    if (stop_if_error) {
      cli::cli_abort(msg)
    } else {
      cli::cli_alert_danger(msg)
      dplyr::filter(dat, t < "1700-01-01 00:00")
    }
  }
}

#' Join EnvLogger data corresponding to the same ID
#'
#' @description
#' Colate EnvLogger data whenever ID is shared (same and different serials)
#' !NOTE!: currently, overlaps are being handled in a very crude manner (averaging temperature for overlaping timestamps); in reality, overlaps shouldn't even happen, because data should be trimmed in the files themselves to exclude anything before and after deployment, as doing it through code is impossible (it must be done on a case by case approach); for now, overlaps are averaged out and a warning is issued.
#'
#' @param dat a tibble with a collection of outputs from running [read_env_all()] on multiple paths corresponding to EnvLogger reports, but filtered to include only data with (or without) pressure data, but not both.
#'
#' @seealso [read_env_all()]
#'
#' @return
#' The same tibble, but now with readings every freq_mins
#'
#' @export
#'
#' @examples
#' paths <- env_example(c("04E3_EE00_3852_03", "04CC_9700_010C_10"))
#' dat  <- purrr::map_dfr(paths, read_env_all)
#' env_join_id(dat)
env_join_id <- function(dat) {
  if (nrow(dat)) {
    PRESS <- unique(dat$pressure)
    if (length(PRESS) != 1) cli::cli_abort(c("'dat' can't have entries with and without pressure data", "(choose one or the other)"))

    dat <- dat %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        pressure = pressure[1],
        serials  = paste(serial, collapse = ","),
        data = data %>%
          dplyr::bind_rows() %>%
          dplyr::arrange(t) %>%
          list(),
        overlap = purrr::map_dbl(data, ~nrow(.x) - (.x$t %>% unique() %>% length)) > dplyr::n(),
        data = purrr::map(data, ~.x %>%
                            dplyr::group_by(t) %>%
                            dplyr::summarise(
                              temp  = mean(temp),
                              press = if (PRESS) mean(press) else NA
                            )),
        t0 = min(t0),
        t1 = max(t1),
        min = min(min),
        max = max(max),
        .groups = "drop"
      )
    if (!PRESS) dat <- dplyr::mutate(dat, data = purrr::map(data, ~dplyr::select(.x, -press)))
  }
  dat
}

#' Read EnvLogger data
#'
#' @description
#' Wrapper function for [read_env_all()], allowing for a simpler reading of all EnvLogger information and data available in a given directory.
#'
#' @inheritParams read_env_log
#' @inheritParams read_env_all
#' @param paths paths to EnvLogger file(s) and/or folder(s) with EnvLogger data.
#' @param recursive logical, defaults to `TRUE`; whether to search for EnvLogger files recursively or not.
#' @param bind_id logical, defaults to `TRUE`; wether to row_bind logger reports for the same id (information specific to individual report files is discarded)
#' @param correct_rtc_drift logical, defaults to `TRUE`; if `TRUE`, logger data timestamps are stretched to account for the recorded clock drift
#' @param use_rtc_correction_file logical, defaults to `TRUE`; if `TRUE`, whenever a properly-formatted rtc corrections file is found inside of a data folder, those corretions are read and applied
#' @param approx_hourly logical, defaults to `TRUE`; if `TRUE`, logger data is interpolated to rounded hours (i.e., is made perfectly hourly and trimmed at the first and last days, to ensure that included days are complete; if `bind_id = TRUE`, trimming is executed after binding)
#' @param full_days logical, defaults to `FALSE`; if `TRUE`, logger data is only kept for days in which all data points were collected.
#' @param just_rep logical, defaults to `FALSE`; if `FALSE`, a list of all reports and all logfile data is returned, while if `TRUE`, only the tibble with data from reports is returned.
#'
#' @seealso [read_env_all()], [read_env_header()], [read_env_data()], [read_env_log()], [plot_env()]
#'
#' @return
#' A list with EnvLogger information (and data) from all reports and logfiles found in the target path, divided into `$rep`and `$log` or, if `just_rep = TRUE`, a the tibble with information for all reports found.
#'
#' @export
#'
#' @examples
#' paths <- env_example("ptzzy", dir = TRUE)
#' READ_ENV(paths, log_summary = TRUE)
READ_ENV <- function(
    paths,
    zero_secs = FALSE,
    recursive = TRUE,
    read_data = TRUE,
    bind_id   = TRUE,
    correct_rtc_drift = TRUE,
    use_rtc_correction_file = TRUE,
    approx_hourly = TRUE,
    full_days     = FALSE,
    log_summary   = TRUE,
    just_rep      = FALSE
    ) {
  x <- tibble::tibble(path = paths, isdir = file.info(paths)$isdir)

  # grab paths to reports and log files
  x <- c(
    x %>%
      dplyr::filter(isdir) %>%
      dplyr::pull(path) %>%
      purrr::map(~dir(.x, full.names = TRUE, recursive = recursive)) %>%
      unlist(),
    x %>%
      dplyr::filter(!isdir) %>%
      dplyr::pull(path)
  ) %>%
    stringr::str_to_lower() %>%
    stringr::str_subset("csv")

  # check file type
  x <- tibble::tibble(
    path = x,
    type = is_envlogger(path)
  )

  # read files
  x <- list(
    rep = x %>%
      dplyr::filter(type == 1) %>%
      dplyr::pull(path) %>%
      purrr::map_dfr(~read_env_all(.x, zero_secs = zero_secs, read_data = read_data)),
    log = x %>%
      dplyr::filter(type == 2) %>%
      dplyr::pull(path) %>%
      purrr::map_dfr(~read_env_all(.x, log_summary = log_summary)),
    rtc = x %>%
      dplyr::filter(type == 3) %>%
      dplyr::pull(path) %>%
      purrr::map_dfr(~read_env_all(.x))
  )

  # discard reports without data
  if (nrow(x$rep)) x$rep <- dplyr::filter(x$rep, nrow > 1)

  if (nrow(x$rep)) {
    # if available apply RTC corrections
    # (this MUST take place BEFORE correcting clock drift)
    if (read_data) {
      if (!is.null(x$rtc)) {
        if (nrow(x$rtc)) {
          # time_offset is added to all timestamps
          # diff_offset is added to tdiff
          corr_folders <- x$rtc$path %>% dirname() %>% unique()

          for (p in seq_along(corr_folders)) {
            corr_folder <- corr_folders[p]
            corrs <- dplyr::filter(x$rtc, dirname(path) == corr_folder)

            for (r in seq_along(corrs$serial)) {
              target <- which(
                x$rep$serial == corrs$serial[r] &
                  dirname(x$rep$path) == corr_folder
              )

              if (length(target) != 1) cli::cli_abort(c("mismatch in RTC corrections file in folder:", corr_folder, "--> issue: incorrect number of matching targets"))

              if (stringr::str_detect(x$rep$id[target], corrs$site[r], negate = TRUE)) cli::cli_abort(c("mismatch in RTC corrections file in folder:", corr_folder, "--> issue: different site names"))

              # time_offset
              time_offset <- corrs$time_offset[r]
              x$rep$data[[target]] <- x$rep$data[[target]] %>%
                dplyr::mutate(t = t + time_offset)

              # diff_offset
              diff_offset <- corrs$diff_offset[r]
              x$rep$tdiff[target] <- x$rep$tdiff[target] + diff_offset
            }
          }
        }
      }
    }

    # in some (many cases), tdiff was calculated wrongly by the EnvLogger Viewer app and written into EnvLogger report files
    # the mistake involved subtracting the obs to the ref (ref - obs), instead of the opposite, which is the correct way
    # this error took place despite the description of the field in the header of EnvLogger report files still explicitly indicating that tdiff was the result of obs - ref
    # this affects report files that were produced under all of the following conditions:
    # - device running EnvLogger Viewer is an Android
    # - EnvLogger Viewer version prior or equal to 6.2
    # -> if logger only records temperature, logger version greater than 2.4, or...
    # -> if logger records additional variables, any logger version
    x$rep <- x$rep %>%
      dplyr::mutate(
        is_android = dev_type == "android",
        is_old_app = v_app <= 6.2,
        is_just_t  = stringr::str_remove_all(v_log, "[0-9.]") == "t",
        is_v2.4    = v_log %>%
          stringr::str_remove_all("[^0-9.]") %>%
          as.numeric() %>%
          "<="(2.4),
        fix = is_android & is_old_app & ((is_just_t & !is_v2.4) | !is_just_t),
        tdiff = ifelse(fix, -tdiff, tdiff)
      ) %>%
      dplyr::select(-is_android, -is_old_app, -is_just_t, -is_v2.4, -fix)


    # correct clock drift
    # drift = logger - smartphone
    # pos -> logger time was faster -> timestamps must be reduced
    # neg -> logger time was slower -> timestamps must be increased
    if (correct_rtc_drift) {
      x$rep <- dplyr::mutate(x$rep,
                             data = purrr::map2(
                               data,
                               tdiff,
                               ~env_rtc_drift(.x, .y)
                             )
      )
    }

    # interpolate hourly
    if (approx_hourly) {
      x$rep <- dplyr::mutate(
        x$rep,
        data = purrr::map(data,
                          env_interpolate,
                          stop_if_error = FALSE,
                          dataset_has_pressure = any(x$rep$pressure))
      )
    }

    # full days
    if (full_days) {
      FULL_DAYS <- x$rep$data %>%
        purrr::map(
          ~.x %>%
            dplyr::group_by(t = as.Date(t)) %>%
            dplyr::summarise(n = dplyr::n()) %>%
            dplyr::filter(n == max(n)) %>%
            dplyr::pull(t)
        )
      x$rep <- dplyr::mutate(
        x$rep,
        data = purrr::map2(data, FULL_DAYS,
                           ~dplyr::filter(.x, as.Date(t) %in% .y)
        ),
        t0 = purrr::map_chr(data, ~dplyr::first(.x$t) %>% as.character()) %>% as.Date(),
        t1 = purrr::map_chr(data, ~dplyr::last( .x$t) %>% as.character()) %>% as.Date()
      )
    }

    if (bind_id) {
      x_bind    <- dplyr::group_by(x$rep, id, pressure)
      x_bind_t  <- dplyr::filter(x_bind, !pressure) %>% env_join_id()
      x_bind_tp <- dplyr::filter(x_bind,  pressure) %>% env_join_id()

      x$rep <- x_bind_t %>%
        dplyr::bind_rows(x_bind_tp) %>%
        dplyr::select(id, serials, data, overlap, t0, t1, min, max, pressure)

      if (any(x$rep$overlap)) cli::cli_warn("some timestamps weren't unique - consider editing the report files to remove overlapping data (but do so with caution!)")
    }

    x$rep <- x$rep %>%
      dplyr::mutate(xts = purrr::map(data, ~xts::xts(dplyr::select(.x, -t), .x$t, tzone = "UTC"))) %>%
      dplyr::relocate(xts, .after = data)
  }

  if (just_rep) x$rep else x
}


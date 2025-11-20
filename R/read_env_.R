# >> ----
# paths = dplyr::filter(env_ls(env_example()), rep_env)$path; read_data = TRUE
# read_rep_env(paths, read_data)
read_rep_env <- function(
    paths,
    read_data = TRUE
) {
  str1 <- purrr::map(paths, ~readr::read_lines(.x, progress = FALSE, n_max = 30))
  skip <- purrr::map_dbl(str1,
                         ~.x %>%
                           stringr::str_which("------------------------") %>%
                           max())
  str2 <- purrr::map2(paths, skip,
                      ~readr::read_csv(
                        file = .x,
                        progress = FALSE,
                        n_max = .y,
                        col_names = c("field", "val"),
                        show_col_types = FALSE) %>%
                        fix_header(path = fs::path_file(.x)))

  # parse headers
  dat <- tibble::tibble(
    id       = env_header_val(str2, "custom name")$chr,
    serial   = env_header_val(str2, "serial number")$chr,
    type     = env_header_val(str2, "envlogger version")$txt,
    v_log    = env_header_val(str2, "envlogger version")$dbl,
    v_app    = env_header_val(str2, "envlogger viewer version")$dbl,
    press    = purrr::map_lgl(str2, ~any(stringr::str_detect(.x$field, "pressure"))),
    hum      = purrr::map_lgl(str2, ~any(stringr::str_detect(.x$field, "humidity"))),
    nrow     = env_header_val(str2, "samples")$dbl,
    skip     = skip,
    lat      = env_header_val(str2, "lat")$dbl,
    lon      = env_header_val(str2, "long")$dbl,
    res      = env_header_val(str2, "sampling resolution")$dbl,
    int      = NA, # must be computed below
    tdiff    = env_header_val(str2, "time diff [logger-smartphone](sec)")$dbl,
    dev      = env_header_val(str2, "device")$chr,
    dev_nm   = env_header_val(str2, "device name")$chr,
    dev_os   = env_header_val(str2, "downloading device type")$chr,
    mode     = env_header_val(str2, "downloading mode")$chr,
    fn       = env_header_val(str2, "file name")$chr,
    path     = fs::path_real(paths) %>% stringr::str_to_lower()
  )

  # read data after header
  if (read_data) {
    dat <- dat %>%
      dplyr::mutate(
        # get data
        data = purrr::map2(
          path,
          skip,
          read_rep_env_data
          ),
        # update nrow with real value
        # (info in header may be wrong)
        nrow = purrr::map_dbl(
          data,
          ~nrow(.x))
      ) %>%
      # separate
      split(.$nrow > 1)

    if (nrow(dat$`TRUE`)) {
      dat$`TRUE` <- dat$`TRUE` %>%
        dplyr::mutate(
          min = purrr::map_dbl(data, ~min(.x$temp, na.rm = TRUE)),
          max = purrr::map_dbl(data, ~max(.x$temp, na.rm = TRUE)),
          t0  = purrr::map_vec(data, ~dplyr::first(.x$t)),
          t1  = purrr::map_vec(data, ~dplyr::last(.x$t)),
          int = purrr::map_dbl(data,
                               ~{
                                 diffs <- diff(.x$t)
                                 units(diffs) <- "secs"
                                 diffs <- as.numeric(diffs)
                                 diffs %>%
                                   median() %>%
                                   round()
                               }
          )
        ) %>%
        dplyr::relocate(id, serial, data, min, max, t0, t1)
    }

    dat <- dplyr::bind_rows(dat)
  }

  # return
  dat %>%
    dplyr::arrange(id, serial, t0)
}


# >> ----
# path = env_example("ptzzy")$rep[[1]]; skip = 21
# read_rep_env_data(path, skip)
read_rep_env_data <- function(
    path,
    skip
) {
  data <- readr::read_csv(
    path,
    skip = skip,
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    tidyr::drop_na() %>%
    dplyr::rename(t = time)

  cols <- colnames(data)

  # detect pressure data
  if (any(stringr::str_detect(cols, "press"))) {
    data <- data %>%
      dplyr::rename(press = pressure) %>%
      dplyr::mutate(press = press * 0.010197)
  }

  # detect humidity data
  if (any(stringr::str_detect(cols, "hum"))) {
    data <- data %>%
      dplyr::rename(hum = humidity)
  }

  # if (zero_secs & nrow(data)) data <- dplyr::mutate(data, t = t - lubridate::second(t[1]))

  # return
  data
}


# >> ----
# paths = dplyr::filter(env_ls(env_example()), rep_other)$path; metadata = read_env_metadata(env_example()$metadata); read_data = TRUE
# read_rep_other(paths, metadata, read_data)
read_rep_other <- function(
    paths,
    metadata,
    read_data = TRUE
) {
  # retain relevant rows and cols
  ## rows = only those concerning non-EnvLogger files
  ## cols = discard metadata to be used in corrections
  metadata <- metadata %>%
    dplyr::filter(path %in% paths) %>%
    dplyr::select(
      path,
      dplyr::any_of(metadata_cols_env),
      dplyr::any_of(metadata_cols_other)
      )

  # make metadata complete
  ## if read_data = TRUE there are more columns needed
  data_col_types <- env_example("ptzzy")$rep[[1]] %>%
    read_rep_env(read_data = read_data) %>%
    tibble::add_row() %>%
    dplyr::slice(2)

  ## metadata for output
  meta_out <- dplyr::bind_cols(
    metadata,
    data_col_types %>%
      dplyr::select(!dplyr::any_of(colnames(metadata))) # add missing cols
  ) %>%
    dplyr::select(colnames(data_col_types)) %>% # reorder
    dplyr::mutate(
      press = FALSE,
      hum   = FALSE,
      tdiff = dplyr::if_else(is.na(tdiff), 0, tdiff),
      fn    = fs::path_file(path)
    )

  ## metadata for data read
  ### template
  template <- matrix(NA, nrow = 1, ncol = length(metadata_cols_other)) %>%
    as.data.frame() %>%
    tibble::tibble()
  colnames(template) <- metadata_cols_other
  ## merge with existing values
  meta_read <- dplyr::select(metadata, path, dplyr::any_of(metadata_cols_other))
  meta_read <- dplyr::bind_cols(
    meta_read,
    template %>%
      dplyr::select(!dplyr::any_of(colnames(meta_read))) # add missing cols
  )

  # read data
  if (read_data) {
    DATA <- list()
    for (i in seq_along(meta_read$path)) {
      meta_i <- dplyr::slice(meta_read, i)

      # read raw data
      dat <- meta_i$path %>%
        readr::read_lines(progress = FALSE) %>%
        stringr::str_to_lower()

      # transform if necessary
      ## decimal separator
      if (!is.na(meta_i$sep_dec_comma)) {
        dat <- stringr::str_replace(dat, ",", ".")
      }
      ## column separator
      dat <- stringr::str_replace(dat, ";", ",")

      # skip
      skip <- meta_i$skip
      ## guess if not supplied
      if (is.na(skip)) {
        is.data <- stringr::str_detect(dat, "[0-9]") & # has numbers
          stringr::str_detect(dat, "[bcdefghijklnoqrstuvwxyz]", negate = TRUE) & # doesn't have letters (except for amp)
          stringr::str_detect(dat, "[/-]") & # has characters related to datetime
          stringr::str_detect(dat, "[,.]") # has separators

        skip <- is.data %>% cumsum() %>% stringr::str_which("1") %>% dplyr::first() %>% magrittr::subtract(1)
        if (is.na(skip)) skip <- 0
      }
      dat <- tail(dat, -skip)

      # split columns
      dat <- dat %>%
        stringr::str_split(",") %>%
        purrr::map(~c(.x[[1]], .x[[length(.x)]])) # sometimes the temperature unit is stored between the timestamp and the temperature value, and by selecting only the first and the last elements we ensure that we have only two columns, hopefully the ones we want/need

      # interpret t and temp
      format <- if (!is.na(meta_i$time_format)) {
        meta_i$time_format
      } else {
        "%Y-%m-%d %H:%M"
      }
      dat <- tibble::tibble(
        t    = purrr::map_vec(dat, 1),
        temp = purrr::map_chr(dat, 2) %>% as.numeric()
      ) %>%
        dplyr::mutate(
          t = lubridate::as_datetime(
            t,
            format =format
          )
        ) %>%
        dplyr::arrange(t)

      # store
      DATA[[i]] <- dat
    }

    # add to tibble and compute associated stats
    meta_out <- meta_out %>%
      dplyr::mutate(
        data  = DATA,
        nrow  = purrr::map_dbl(data, ~nrow(.x)),
        min   = purrr::map_dbl(data, ~min(.x$temp, na.rm = TRUE)),
        max   = purrr::map_dbl(data, ~max(.x$temp, na.rm = TRUE)),
        t0    = purrr::map_vec(data, ~dplyr::first(.x$t)),
        t1    = purrr::map_vec(data, ~dplyr::last(.x$t)),
        int   = purrr::map_dbl(data,
                             ~{
                               diffs <- diff(.x$t)
                               units(diffs) <- "secs"
                               diffs <- as.numeric(diffs)
                               diffs %>%
                                 median() %>%
                                 round()
                             }
        )
      )
  }

  # return
  meta_out %>%
    dplyr::arrange(id, serial, t0)
}


# >> ----
# paths = env_example(c("ptzzy", "ptzzw"))$log; log_summary = TRUE
# read_env_log(paths, log_summary)
read_env_log <- function(
    paths,
    log_summary = FALSE
) {
  msg_warn <- c()

  # dput(cols[[1]])
  COLS <- c("time", "action", "id", "version", "name", "status", "code", "samp_int_s", "samp_res_c", "samples", "time_diff_s", "start_time", "lat", "long", "accuracy", "device")

  # find the colnames in each logfile
  cols <- paths %>%
    readr::read_lines(n_max = 1, progress = FALSE) %>%
    purrr::map(~.x %>%
                 stringr::str_split_1(",") %>%
                 stringr::str_trim() %>%
                 stringr::str_to_lower() %>%
                 stringr::str_replace_all(" ", "_") %>%
                 stringr::str_remove_all("[^[:alpha:]_]") %>%
                 stringr::str_replace_all("sampling", "samp") %>%
                 stringr::str_replace_all("interval", "int") %>%
                 stringr::str_replace_all("resulotion", "res") %>%
                 stringr::str_replace_all("resolution", "res")
    )

  # read all info in each logfile
  logs <- purrr::map2(paths, cols,
                     ~.x %>%
                       readr::read_csv(
                         skip = 1,
                         col_names = .y,
                         show_col_types = FALSE) %>%
                       dplyr::mutate(
                         version    = version %>%
                           as.character() %>%
                           readr::parse_number(),
                         code       = code %>%
                           as.character() %>%
                           readr::parse_character(),
                         time       = lubridate::ymd_hms(time, tz = "UTC"),
                         start_time = lubridate::ymd_hms(start_time, tz = "UTC"),
                         device     = if(any(.y == "device")) device else "missing"
                       )
  )

  # colnames not in COLS must be dropped
  unknown_cols <- cols %>%
    purrr::map_lgl(~all(.x %in% COLS)) %>%
    magrittr::not()

  if (any(unknown_cols)) {
    msg_warn <- c() %>%
      bullets("!", "dropped unsupported columns for the following logfiles: ") %>%
      bullets(" ", fs::path_file(paths[unknown_cols]), col = "blue")
    logs <- purrr::map(logs, ~dplyr::select(.x, dplyr::any_of(COLS)))
  }

  # merge all logs
  ## note that bind_rows ensures that columns missing from one logfile and not the other are retained (values are set to NA)
  logs <- dplyr::bind_rows(logs)

  # if any colnames are still missing, they must be generated and populated with NA
  missing_cols <- COLS[!COLS %in% colnames(logs)]
  if (length(missing_cols)) {
    for (m in missing_cols) logs[[m]] <- NA
  }

  # rename cols and tidy
  logs <- logs %>%
    dplyr::rename(
      serial = id,
      tdiff  = time_diff_s,
      lon    = long,
      v_log  = version,
      pass   = code,
      int    = samp_int_s,
      res    = samp_res_c,
      nrow   = samples,
      start  = start_time,
      dev    = device,
      id     = name
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_to_lower)) %>%
    dplyr::relocate(time, action, status, id, serial, v_log, nrow, lat, lon, int, res, tdiff, pass, start)

  if (log_summary) {
    logs <- logs %>%
      dplyr::arrange(time) %>%
      dplyr::group_by(serial) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::arrange(time) %>%
      dplyr::select(time, id, serial, v_log, int, res, pass, dev) %>%
      dplyr::ungroup()
  }

  # return
  list(logs = logs, msg_warn = msg_warn)
}


# >> ----
# paths = env_example()$metadata
# read_env_metadata(paths)
read_env_metadata <- function(
    paths
) {
  # read metadata details data
  met <- paths %>%
    purrr::map(~.x %>%
                     readr::read_csv(
                       skip = 4,
                       progress = FALSE,
                       show_col_types = FALSE,
                       col_types = readr::cols(.default = "c")
                      ) %>%
                     dplyr::select(field, new_val) %>%
                     tibble::add_column(path = .x)
    ) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        stringr::str_to_lower
      )
    ) %>%
    tidyr::drop_na() %>%
    # pivot
    tidyr::pivot_wider(
      names_from  = field,
      values_from = new_val
    ) %>%
    # update col_types
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("tz_") | dplyr::starts_with("purge_temp_"),
        as.numeric
      ),
      dplyr::across(
        dplyr::starts_with("trim_") | dplyr::starts_with("purge_time_"),
        ~lubridate::as_datetime(.x)
      )
    )

  # read corresponding file names
  original <- paths %>%
    purrr::map(
      ~tibble::tibble(
        path = .x,
        fn = .x %>%
          readr::read_lines(n_max = 1, progress = FALSE) %>%
          stringr::str_split(",----,") %>%
          dplyr::first() %>%
          dplyr::last()
      )
    ) %>%
    purrr::list_rbind()

  # confirm that the metadata files are consistent
  # i.e., that there's always a corresponding data file in the same folder
  met <- met %>%
   dplyr::left_join(original, by = "path") %>%
    dplyr::mutate(
      path = fs::path(path %>% fs::path_dir(), fn) %>%
        stringr::str_to_lower() %>%
        fs::path_tidy(),
      ok   = fs::file_exists(path)
    )

  bad <- dplyr::filter(met, !ok)

  if (nrow(bad)) {
    msg <- c() %>%
      bullets("x", "found inconsistencies!") %>%
      bullets("i", "metadata files must match files stored in the same folder") %>%
      bullets(" ", bad$path, col = "blue")
    cli::cli_abort(msg)
  }

  # return
  met <- met %>%
    dplyr::filter(ok) %>%
    dplyr::select(-ok, -fn)
  suppressMessages(readr::type_convert(met))
}


# >> ----
# paths = env_example()$rep %>% env_ls() %>% dplyr::filter(rep_env) %>% dplyr::pull(path); df = read_rep_env(paths, read_data = TRUE)
# enforce_col_types(df)
enforce_col_types <- function(
    df
) {
  has_data <- "data" %in% colnames(df)

  cols <- env_example("ptzzy")$rep[[1]] %>%
    read_rep_env(read_data = has_data) %>%
    tibble::add_row() %>%
    dplyr::slice(2)

  # ensure colnames identity and order
  df <- dplyr::select(df, dplyr::all_of(colnames(cols)))
  same_cols <- identical(colnames(cols), colnames(df))
  if (!same_cols) cli::cli_abort(c("x" = "colnames not as expected"))

  # enforce col_types
  col_types_list <- tibble::tibble(
    col  = colnames(cols),
    type = purrr::map_chr(cols, pillar::type_sum)
  ) %>%
    dplyr::left_join(col_types, by = "type")

  for (COL in colnames(cols)) {
    df[[COL]] <- suppressWarnings(dplyr::filter(col_types_list, col == COL)$fun[[1]](df[[COL]]))
  }

  df
}


# >> ----
# paths <- env_example(); avoid_pattern = NULL; read_data = TRUE; log_summary = TRUE; apply_fixes = TRUE
# read_env_all(paths, avoid_pattern, read_data, apply_fixes, log_summary)
read_env_all <- function(
    paths,
    avoid_pattern = NULL,
    read_data     = TRUE,
    apply_fixes   = TRUE,
    log_summary   = FALSE
) {
  msg_bullets <- msg_warn <- list()
  file_paths <- env_ls(
    paths,
    avoid_pattern = avoid_pattern,
    list_unsupported = TRUE)

  # read files
  out <- list()

  ## logfiles
  logs <- file_paths %>%
    dplyr::filter(log) %>%
    dplyr::pull(path)
  if (length(logs)) {
    logs <- read_env_log(logs, log_summary = log_summary)
    msg_warn$log <- logs$msg_warn
    out$log      <- logs$logs
  }

  ## metadata details files
  metas <- file_paths %>%
    dplyr::filter(met) %>%
    dplyr::pull(path)
  if (length(metas)) out$metadata <- read_env_metadata(metas)

  ## reports
  reps <- list()

  ### (EnvLogger)
  reps_env <- file_paths %>%
    dplyr::filter(rep_env) %>%
    dplyr::pull(path)

  if (length(reps_env)) {
    # read
    reps_env <- read_rep_env(reps_env, read_data = read_data)

    if (apply_fixes & !is.null(out$metadata)) {
      cols <- template_metadata %>%
        dplyr::filter(rep_env) %>%
        dplyr::pull(field)

      # check for available metadata updates that need to be executed at this moment (before assessing quality or joining data)
      for (f in seq_along(out$metadata$path)) {
        meta <- out$metadata %>%
          dplyr::slice(f) %>%
          # select only columns relevent for EnvLogger reports (at this stage, only updates to info matter, not corrections)
          dplyr::select(path, dplyr::any_of(cols)) %>%
          # drop columns without new_vals
          dplyr::select(dplyr::where(~!is.na(.)))
        pos <- which(reps_env$path == meta$path)
        # if a line of metadata corresponds to 1 file in env_rep, and if there are relevant fields to update, do so one-by-one
        if (ncol(meta) > 1 & length(pos) == 1) {
          meta$path <- NULL
          for (col in colnames(meta)) {
            reps_env[[col]][[pos]] <- meta[[col]]
          }
        }
      }
    }
    reps$env <- enforce_col_types(reps_env)
  }

  ### (non-EnvLogger)
  reps_other <- file_paths %>%
    dplyr::filter(rep_other) %>%
    dplyr::pull(path)

  if (length(reps_other) & !is.null(out$metadata)) {
    # read
    reps$other <- read_rep_other(
      reps_other,
      metadata = out$metadata,
      read_data = read_data) %>%
      enforce_col_types()
  }

  if (length(reps)) {
    # merge reps
    reps <- reps %>%
      purrr::list_rbind() %>%
      dplyr::arrange(id, serial, t0) %>%

      # add flags
      tibble::add_column(
        f_fixs_checked = FALSE,
        f_fixs_applied = FALSE,
        f_qual_checked = FALSE,
        f_qual_good    = FALSE,
        f_qual_issues  = "",
        f_drift_correc = FALSE,
        f_interpolated = FALSE,
        f_bound_serial = FALSE,
        f_bound_id     = FALSE
      )

    # signal reps with no data
    reps_no_data <- reps %>% dplyr::filter(nrow <= 1) %>% dplyr::pull(path)
    file_paths$rep_no_data <- file_paths$path %in% reps_no_data

    reps <- reps %>% dplyr::filter(nrow > 1)
    out$report <- reps
  }

  # msg_bullets
  msg <- c()

  # folders/files found
  n_folders <- file_paths$path %>% fs::path_dir() %>% unique() %>% length()
  n_files   <- nrow(file_paths)
  msg <- bullets(msg, "i", glue::glue("found {n_files} file{ifelse(n_files > 1, 's', '')} in {n_folders} folder{ifelse(n_folders > 1, 's', '')}"))

  # files read, with data
  n_read <- file_paths %>% dplyr::filter(int != 0, !rep_no_data) %>% nrow()
  n_reps <- file_paths %>% dplyr::filter(rep, int != 0, !rep_no_data) %>% nrow()
  n_logs <- file_paths %>% dplyr::filter(log, int != 0) %>% nrow()
  n_mets <- file_paths %>% dplyr::filter(met, int != 0) %>% nrow()

  vec <- stringr::str_c(
    cli::col_black(cli::col_green("files read (n = {n_read})")),
    " [",
    stringr::str_c(c(
      (if (n_reps) "{n_reps} reports"),
      (if (n_logs) "{n_logs} logs"),
      (if (n_mets) "{n_mets} metadata")),
      collapse = ", "
    ),
    "]"
  )

  msg <- bullets(msg, "v", glue::glue(vec))

  msg_bullets$read <- msg

  ### issues ----
  # unsupported
  issues <- file_paths %>%
    dplyr::filter(int == 0) %>%
    append_issues(
    step  = "unsupported",
    issue = "unsupported"
  )

  # valid reps without data
  issues <- file_paths %>%
    dplyr::filter(rep, int != 0, rep_no_data) %>%
    append_issues(
      step  = "unsupported",
      issue = "no data",
      df    = issues
    )

  # return
  c(
    out,
    list(issues = issues, msg_bullets = msg_bullets, msg_warn = msg_warn)
  )
}

# >> ----
# paths = env_example(); dat = read_env_all(paths, read_data = TRUE)
# env_fix(dat)
env_fix <- function(
    dat
) {
  splits <- 0
  rep <- dat$report

  if (!is.null(dat$metadata)) {
    # check if there's work to do
    FIX <- dplyr::select(dat$metadata, path, dplyr::any_of(metadata_cols_fix))
    if (ncol(FIX) > 1) {
      # discard rows without work to do
      keep <- FIX %>%
        dplyr::select(-path) %>%
        apply(1, function(x) !all(is.na(x)))
      FIX <- FIX[keep, ]
      # add columns for all fix parameters (default value is NA)
      missing_cols <- setdiff(metadata_cols_fix, colnames(FIX))
      if (length(missing_cols)) {
        FIX2 <- matrix(
          NA,
          ncol = length(missing_cols),
          nrow = nrow(FIX)) %>%
          as.data.frame()
        colnames(FIX2) <- missing_cols

        FIX <- dplyr::bind_cols(FIX, FIX2)
      }

      # apply fixes
      for (i in seq_along(FIX$path)) {
        fix <- dplyr::slice(FIX, i)
        r   <- which(rep$path == fix$path)
        if (length(r) == 1) {

          anything_done <- FALSE

          ## OFFSETS
          # offset_time
          if (!is.na(fix$offset_time)) {
            rep_data       <- rep$data[[r]]
            rep_data$t     <- rep_data$t + fix$offset_time
            rep$data[[r]]  <- rep_data
            rep$t0[[r]]    <- dplyr::first(rep_data$t)
            rep$t1[[r]]    <- dplyr::last( rep_data$t)
            anything_done  <- TRUE
          }

          # offset_diff
          if (!is.na(fix$offset_diff)) {
            rep$tdiff[[r]] <- rep$tdiff[[r]] + fix$offset_diff
            anything_done  <- TRUE
          }

          ## DISCARD ENTRIES WITH TEMP BEYOND THRESHOLD
          # purge_temp_min
          if (!is.na(fix$purge_temp_min)) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              temp >= fix$purge_temp_min)
            anything_done <- TRUE
          }

          # purge_temp_max
          if (!is.na(fix$purge_temp_max)) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              temp <= fix$purge_temp_max)
            anything_done <- TRUE
          }

          ## DISCARD ENTRIES WITH TIME BEYOND THRESHOLD
          # purge_time_bef
          if (!is.na(fix$purge_time_bef)) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              t >= fix$purge_time_bef)
            anything_done <- TRUE
          }

          # purge_time_aft
          if (!is.na(fix$purge_time_aft)) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              t <= fix$purge_time_aft)
            anything_done <- TRUE
          }

          ## DISCARD ENTRIES WITH TIME == NA
          # purge_na_time
          if (!is.na(fix$purge_na_time) & fix$purge_na_time) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              !is.na(t))
            anything_done <- TRUE
          }

          ## DISCARD ENTRIES WITH DATA == NA
          # purge_na_data
          if (!is.na(fix$purge_na_data) & fix$purge_na_data) {
            rep$data[[r]] <- dplyr::filter(
              rep$data[[r]],
              !is.na(temp))
            anything_done <- TRUE
          }

          ## SPLIT DATA WITH GAPS
          # split_time_gap
          split_yes <- !is.na(fix$split_time_gap) & fix$split_time_gap
          if (split_yes | anything_done) {
            data  <- rep$data[[r]]
            # time diffs in secs
            diffs <- diff(data$t)
            units(diffs) <- "secs"
            # compare with sampling interval
            gap <- c(FALSE, diffs != rep$int[[r]])
            has_gap <- gap %>%
              head(-1) %>%
              tail(-1) %>%
              any()
            # if one or more gaps are found, split
            if (has_gap) {
              # split data based on the gaps found
              split_data <- split(data, cumsum(gap))
              split_data <- unname(split_data)
              # duplicate the entry in rep (or triplicate, etc)
              old_rep <- dplyr::slice(rep, r)
              old_rep <- dplyr::slice(old_rep, rep(1, length(split_data)))
              # update with the new data (each new entry gets one of the splits)
              old_rep$data <- split_data
              # update tdiff
              ## nrow is needed for the computation of new tdiffs
              ## other stats updated later for all entries
              new_rep <- old_rep %>%
                dplyr::mutate(
                  nrow  = purrr::map_dbl(data, ~nrow(.x)),
                  tdiff = purrr::map2_dbl(
                    tdiff,
                    nrow,
                    ~round((.x / rep$nrow[[r]]) * .y)
                  )
                )
              # replace rep entry
              rep <- rep %>%
                dplyr::slice(-r) %>% # remove the previous entry with gaps
                dplyr::bind_rows(new_rep) # add new entries

              splits <- splits + length(split_data) - 1
            }
            anything_done <- TRUE
          }
        }

        ## FLICK f_fixs_applied
        rep$f_fixs_applied[[r]] <- anything_done
      }
      # update stats
      rep <- rep %>%
        dplyr::mutate(
          nrow = purrr::map_dbl(data, ~nrow(.x)),
          t0   = purrr::map_vec(data, ~dplyr::first(.x$t)),
          t1   = purrr::map_vec(data, ~dplyr::last( .x$t)),
          min  = purrr::map_dbl(data, ~min(.x$temp)),
          max  = purrr::map_dbl(data, ~max(.x$temp))
        ) %>%
        dplyr::arrange(id, serial, t0)
    }
  }

  # messages
  if (any(rep$f_fixs_applied)) {
    msg <- bullets(c(), "v", glue::glue("fix details found and applied (n = {sum(rep$f_fixs_applied) - splits})"), col = "green")
  } else {
    msg <- bullets(c(), "v", "no fix details found", col = "green")
  }
  dat$msg_bullets$fix <- msg

  # return
  rep$f_fixs_checked = TRUE
  dat$report <- rep
  dat
}


# >> ----
# dat = read_env_all(env_example()); lims = list(min = -30, max = +70, t0 = "2000-01-01", t1 = Sys.Date()); buffer_secs = 10
# env_check_qual(dat, lims, buffer_secs)
env_check_qual <- function(
    dat,
    lims = list(
      min = -30,
      max = +70,
      t0 = "2000-01-01",
      t1 = Sys.Date()),
    buffer_secs = 10
) {
  lims$t0 <- lubridate::as_datetime(lims$t0)
  lims$t1 <- lubridate::as_datetime(lims$t1)
  rep <- dat$report

  # assess quality
  rep$f_qual_issues <- rep %>%
      dplyr::mutate(
        ## min & max within thresholds
        bad_min = dplyr::if_else(min < lims$min, "min", " "),
        bad_max = dplyr::if_else(max > lims$max, "max", " "),

        ## t0 & t1 within range (T1 default = today)
        bad_t0 = dplyr::if_else(t0 < lims$t0, "t0", " "),
        bad_t1 = dplyr::if_else(t1 > lims$t1, "t1", " "),

        ## all t within range
        tmin = purrr::map_vec(data, ~min(.x$t)),
        tmax = purrr::map_vec(data, ~max(.x$t)),
        bad_tmin = dplyr::if_else(tmin < lims$t0, "tmin", " "),
        bad_tmax = dplyr::if_else(tmax > lims$t1, "tmax", " "),

        ## timestamps and data without NAs
        bad_time_na = dplyr::if_else(purrr::map_lgl(data, ~.x$t %>% is.na() %>% any()), "time_na", " "),
        bad_data_na = dplyr::if_else(purrr::map_lgl(data, ~.x %>% dplyr::select(-t) %>% is.na() %>% any()), "data_na", " "),

        ## timestamps are unidirectional, constant and without gaps
        bad_time_gap = dplyr::if_else(
          !purrr::map_lgl(
            data,
            t_check,
            buffer_secs = buffer_secs),
          "time_gap",
          " ")
      ) %>%

      # aggregate
      dplyr::rowwise() %>%
      dplyr::summarise(
        f_qual_issues = dplyr::c_across(tidyselect::starts_with("bad_")) %>%
                         stringr::str_c(collapse = ",") %>%
                         stringr::str_remove_all(", ") %>%
                         stringr::str_trim() %>%
                         stringr::str_remove("^,")
                         ) %>%
    dplyr::pull(f_qual_issues)

  rep <- rep %>%
    dplyr::mutate(
      f_qual_checked = TRUE,
      f_qual_good    = f_qual_issues == "")

  # if there are fixes needed, gather relevant metadata to address them
  rep_fix <- dplyr::filter(rep, !f_qual_good)
  if (nrow(rep_fix)) {
    new_vals <- list()
    for (i in seq_along(rep_fix$path)) {
      f <- rep_fix$f_qual_issues[i] %>% stringr::str_split_1(",")
      tmp <- list()
      if (any(f == "min"))      tmp$purge_temp_min = lims$min
      if (any(f == "max"))      tmp$purge_temp_min = lims$max
      if (any(f == "t0"))       tmp$purge_time_bef = lims$t0
      if (any(f == "tmin"))     tmp$purge_time_bef = lims$t0
      if (any(f == "t1"))       tmp$purge_time_aft = lims$t1
      if (any(f == "tmax"))     tmp$purge_time_aft = lims$t1
      if (any(f == "time_na"))  tmp$purge_na_time  = TRUE
      if (any(f == "data_na"))  tmp$purge_na_data  = TRUE
      if (any(f == "time_gap")) tmp$split_time_gap = TRUE
      new_vals[[i]] <- purrr::map(tmp, as.character)
    }

    dat$issues <- append_issues(
      paths    = rep_fix,
      step     = "quality",
      issue    = rep_fix$f_qual_issues,
      new_vals = new_vals,
      df       = dat$issues
    )
  }

  ## if some (or all) files are ok...
  if (any(rep$f_qual_good)) {
    if (all(rep$f_qual_good)) {
      msg <- bullets(c(), "v", "quality checks passed by all reports", col = "green")
    } else {
      msg <- bullets(c(), "i", glue::glue("quality checks failed by some reports (n = {sum(!rep$f_qual_good)}/{nrow(rep)})"), col = "yellow")
    }
  } else {
    msg <- bullets(c(), "x", "quality checks failed by all reports!", col = "red")
  }
  dat$msg_bullets$qual <- msg

  # return
  dat$report <- rep %>%
    dplyr::filter(f_qual_good) %>%
    dplyr::select(-f_qual_issues, -f_qual_good)
  dat
}


# >> ----
# dat <- read_env_all(env_example())
# env_rtc_drift(dat)
# dat$rep$data[[24]]; env_rtc_drift(dat)$rep$data[[24]];
# dat$rep$data[[24]] %>% tail(); env_rtc_drift(dat)$rep$data[[24]] %>% tail()
env_rtc_drift <- function(
    dat
) {
  dat$report <- dat$report %>%
    dplyr::mutate(
      is_other   = !type %in% envlogger_types,
      is_android = dev_os == "android",
      is_old_app = v_app <= 6.2,
      is_just_t  = type == "t",
      is_v2.4    = v_log <= 2.4,
      is_fix     = is_android &
        is_old_app &
        ((is_just_t & !is_v2.4) | !is_just_t),
      is_fix     = dplyr::if_else(is_other, FALSE, is_fix),

      tdiff = dplyr::if_else(is_fix, -tdiff, tdiff)
    ) %>%
    dplyr::select(!dplyr::starts_with("is_"))

  # correct drift
  dat$report <- dat$report %>%
    dplyr::mutate(
      data = purrr::map2(
        data, tdiff,
        ~{
          t0 <- .x$t %>% dplyr::first()
          t1 <- .x$t %>% dplyr::last()
          t1 <- t1 - .y
          .x$t <- seq(t0, t1, length.out = nrow(.x))
          .x
        }
      ),
      nrow = purrr::map_dbl(data, ~nrow(.x)),
      t0   = purrr::map_vec(data, ~dplyr::first(.x$t)),
      t1   = purrr::map_vec(data, ~dplyr::last( .x$t)),
      f_drift_correc = TRUE
    )

  # messages
  dat$msg_bullets$drift <- bullets(c(), "v", "RTC clock drifts corrected", col = "green")

  # return
  dat
}


# >> ----
# used by env_interpolate()
# --- #
# df = read_env_all(env_example("humid")$rep[[1]])$rep; corr_t0 = lubridate::as_datetime(df$t0); corr_t1 = lubridate::as_datetime(df$t1); df = df$data[[1]]; new_interval = 120
# env_interpolate_df(df, new_interval, corr_t0, corr_t1)
env_interpolate_df <- function(
    df,
    new_interval,
    corr_t0,
    corr_t1
) {
  # new timestamps
  tnew <- seq(corr_t0, corr_t1, by = new_interval * 60)

  # preserve data colnames
  cols <- colnames(df)[-1]

  # interpolate each data field
  dnew <- purrr::map(cols, ~{
    vals <- dplyr::pull(df, dplyr::all_of(.x))
    vals <- stats::approx(
      x    = df$t,
      y    = vals,
      xout = tnew,
      method = "linear",
      rule = 2
    )$y
  })

  # re-establish data colnames
  names(dnew) <- cols

  # return
  dplyr::bind_cols(t = tnew, dplyr::bind_cols(dnew))
}


# >> ----
# dat = read_env_all(env_example()); new_interval = 60; new_interval_margin = 5 * 60
# env_interpolate(dat, new_interval, new_interval_margin)
env_interpolate <- function(
    dat,
    new_interval = 60,
    new_interval_margin = 5 * 60
) {
  INT <- stringr::str_c(new_interval, " mins")

  # prepare
  int <- dat$report %>%
    dplyr::mutate(
      # t0
      # find seq start point
      # - before first dat timestamp if difference to previous rounded hour is less than interpolate_margin
      # this is a mean to prevent 13:01 to lead to a t0 of 14:00
      int_t0 = purrr::map_vec(data, ~dplyr::first(.x$t)),
      int_t0_floor   = lubridate::floor_date(int_t0, INT),
      int_t0_ceiling = lubridate::ceiling_date(int_t0, INT),
      int_t0_margin  = lubridate::interval(int_t0_floor, int_t0) %/% lubridate::seconds(1),
      corr_t0 = dplyr::if_else(
        int_t0_margin < new_interval_margin,
        int_t0_floor,
        int_t0_ceiling),

      # t1
      # find seq end point
      # - after first dat timestamp if difference to next rounded hour is less than interpolate_margin
      int_t1 = purrr::map_vec(data, ~dplyr::last(.x$t)),
      int_t1_floor   = lubridate::floor_date(int_t1, INT),
      int_t1_ceiling = lubridate::ceiling_date(int_t1, INT),
      int_t1_margin  = lubridate::interval(int_t1, int_t1_ceiling) %/% lubridate::seconds(1),
      corr_t1 = dplyr::if_else(
        int_t1_margin < new_interval_margin,
        int_t1_ceiling,
        int_t1_floor),

      # ok to interpolate?
      corr_ok = corr_t0 < corr_t1

    ) %>%
    dplyr::select(!dplyr::starts_with("int_")) %>%
    split(.$corr_ok)

  ## if issues were found...
  if (!is.null(int$`FALSE`)) {
    issues <- append_issues(
      paths = int$`FALSE`,
      step  = "interpolation",
      issue = "interpolation",
      df    = dat$issues
    )
  }

  # interpolate
  dat$report <- int$`TRUE` %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(
          df = data,
          corr_t0 = corr_t0,
          corr_t1 = corr_t1
        ),
        function(df, corr_t0, corr_t1) env_interpolate_df(df, new_interval, corr_t0, corr_t1)
      ),
      nrow = purrr::map_dbl(data, ~nrow(.x)),
      t0   = purrr::map_vec(data, ~dplyr::first(.x$t)),
      t1   = purrr::map_vec(data, ~dplyr::last( .x$t)),
      f_interpolated = TRUE
    ) %>%
    dplyr::select(!dplyr::starts_with("corr_"))

  # messages
  dat$msg_bullets$interp <- bullets(c(), "v", glue::glue("data interpolated to every {new_interval} mins"), col = "green")

  # return
  dat
}


# >> ----
# paths = env_example(); dat <- read_env(paths = paths, show_progress = FALSE, show_warnings = FALSE, correct_rtc_drift = TRUE, new_interval = 60, join_serials = FALSE, join_ids = FALSE, keep_flags = TRUE); overlap_max_mins = 60 * 2
# env_join_by_serial(dat, overlap_max_mins)
env_join_by_serial <- function(
    dat,
    overlap_max_mins
) {
  rep <- dat$report %>%
    dplyr::arrange(id, serial, t0, t1) %>%

    # check for when data has just been downloaded (no mission restart)
    # in such cases there are two or more files, and one of them contains
    # all data (the others must be discarded)
    dplyr::mutate(
      .by = c(id, serial),
      n = dplyr::n(),
      # data$t interval for the current row
      int = purrr::map_vec(
        data,
        ~lubridate::interval(dplyr::first(.x$t), dplyr::last(.x$t))
      ),
      # data$t interval for the next row (if available)
      int_lead = dplyr::lead(int, n = 1),
      # is int1 fully within int2?
      inside = purrr::map2_lgl(
        int,
        int_lead,
        ~lubridate::"%within%"(.x, .y)
      ) %>%
        tidyr::replace_na(FALSE)
    ) %>%
    split(.$inside)

  # if present, save information about files to be dropped
  flag <- list()
  if (!is.null(rep$`TRUE`)) flag$inside <- rep$`TRUE`

  # and continue with only the valid files
  rep <- rep$`FALSE`

  # check if there's partial overlap between consecutive files
  rep <- rep %>%
    dplyr::mutate(
      .by = c(id, serial),
      # data$t interval for the previous row (if available)
      # must be computed here, only after discarding full overlaps
      int_lag = dplyr::lag(int, n = 1)
    ) %>%
    dplyr::mutate(
      # find overlaps
      intersect = purrr::map2_vec(
        int,
        int_lag,
        ~lubridate::intersect(.x, .y)
        ),

      # overlap durations
      overlap_mins = purrr::map_dbl(
        intersect,
        ~.x %>%
          lubridate::as.duration() %>%
          as.numeric("minutes") %>%
          tidyr::replace_na(0) %>%
          round(0)
      ),

      # is the overlap excessive (if yes, it will be flagged)
      overlap = overlap_mins > overlap_max_mins
    )

  # trim overlapping data
  rep <- rep %>%
    dplyr::arrange(id, serial, t0, t1) %>%
    dplyr::mutate(
      .by  = c(id, serial),
      data = purrr::map2(
        data,
        intersect,
        ~if (!is.na(.y)) {
          dplyr::filter(.x, !lubridate::"%within%"(t, .y))
        } else {
          .x
        }),
      t0 = purrr::map_vec(data, ~dplyr::first(.x$t)),

      # pts trimmed
      trimmed = nrow - purrr::map_dbl(data, ~nrow(.x))
    )

  # flag excessive overlaps for review
  # the user is informed of excessive overlaps, but joining continues with youngest overlapping data being discarded
  if (any(rep$overlap)) flag$overlap <- dplyr::filter(rep, overlap)

  # join by serial
  n_before_join <- nrow(rep)

  rep <- rep %>%
    dplyr::group_by(id, serial, type, v_log, press, hum) %>%
    dplyr::mutate(
      sgmnt = letters[1:dplyr::n()],
      data  = purrr::map2(data, sgmnt, ~tibble::add_column(.x, sgmnt = .y))
    ) %>%
    dplyr::summarise(
      # pre-trimmed data can now just be bound together without any overlap
      data = dplyr::bind_rows(data) %>% dplyr::arrange(t) %>% list(),

      # update stats
      nrow = nrow(data[[1]]),
      min  = min(data[[1]]$temp),
      max  = max(data[[1]]$temp),
      t0   = dplyr::first(data[[1]]$t),
      t1   = dplyr::last( data[[1]]$t),
      path_1st = dplyr::first(path),

      # update flags
      f_qual_checked = all(f_qual_checked),
      f_drift_correc = all(f_drift_correc),
      f_interpolated = all(f_interpolated),
      f_bound_serial = TRUE,
      f_bound_id     = FALSE,

      .groups = "drop"
    )

  # messages
  ## if issues were found...
  ### warn ----
  if (length(flag)) {
    # warn about files inside (i.e., just downloaded, full overlaps)
    if (!is.null(flag$inside)) {
      dat$issues <- append_issues(
        paths  = flag$inside,
        step   = "join_serial",
        issue  = "full_overlap",
        df     = dat$issues
      )
    }

    # warn about excessive partial overlaps (i.e., longer than overlap_max_mins)
    if (!is.null(flag$overlap)) {
      dat$issues <- append_issues(
        paths    = flag$overlap,
        step     = "join_serial",
        issue    = "partial_overlap",
        new_vals = purrr::map(
          flag$overlap$t0,
          ~list(purge_time_bef = as.character(.x))),
        df       = dat$issues
      )
    }
  }

  ## report about the joining process
  if (n_before_join > nrow(rep)) {
    msg <- bullets(c(), "v", glue::glue("reports joined by serial (n = {n_before_join} -> {nrow(rep)})"), col = "green")
  } else {
    msg <- bullets(c(), "v", "no reports to join by serial", col = "green")
  }
  dat$msg_bullets$join_serial <- msg

  # return
  dat$report <- rep
  dat
}


# >> ----
# paths = env_example(); dat <- read_env(paths = paths, show_progress = FALSE, show_warnings = FALSE, correct_rtc_drift = TRUE, new_interval = 60, join_serials = TRUE, join_ids = FALSE, keep_flags = TRUE); join_ids_trim_d1 = TRUE
# env_join_by_id(dat, join_ids_trim_d1)
env_join_by_id <- function(
    dat,
    join_ids_trim_d1
) {
  # if join_ids_trim_d1 is TRUE, we must first find the serials that will be joined and determine which will need to have their day of joining trimmed

  ## continue here; commit after completing
  ## afterwards, must make sure that when custom name is absent, serial is set as id
  ## then, make sure summarise_env keeps id (id = site, id = site_group)

  # first trim each serial_i+1 to the end of serial_i
  rep <- dat$report %>%
    dplyr::arrange(id, serial, t0, t1) %>%

    # determine available intervals
    dplyr::mutate(
      # computations done per id
      .by = id,

      # data$t interval for the current row
      int = purrr::map2_vec(
        t0,
        t1,
        ~lubridate::interval(.x, .y)
      ),

      # data$t interval for the previous row (if available)
      int_lag = dplyr::lag(int, n = 1),

      # find overlaps
      intersect = purrr::map2_vec(
        int,
        int_lag,
        ~lubridate::intersect(.x, .y)
      ),

      # determine overlap duration
      overlap_mins = purrr::map_dbl(
        intersect,
        ~.x %>%
          lubridate::as.duration() %>%
          as.numeric("minutes") %>%
          tidyr::replace_na(0) %>%
          round(0)
      ),

      # trim overlapping data
      data = purrr::map2(
        data,
        intersect,
        ~if (!is.na(.y)) {
          dplyr::filter(.x, !lubridate::"%within%"(t, .y))
        } else {
          .x
        }),

      # pts trimmed
      trimmed = nrow - purrr::map_dbl(data, ~nrow(.x)),

      # update t0
      t0 = purrr::map_vec(data, ~dplyr::first(.x$t))
    )

  # save overlap lengths for review
  trimmed <- rep

  # second, if join_ids_trim_d1 = TRUE, trim the first day of serial_i+1
  if (join_ids_trim_d1) {
    rep <- rep %>%
      dplyr::arrange(id, serial, t0, t1) %>%

      # add indexes to facilitate rejoining tibbles later on
      tibble::rownames_to_column("i") %>%

      # determine available intervals
      dplyr::mutate(
        # determine which entries are beyond serial #1
        .by = id,
        group = 1:dplyr::n(),
      )

    firsts <- dplyr::filter(rep, group == 1)
    others <- dplyr::filter(rep, group != 1)

    if (nrow(others)) {
      others <- others %>%
        dplyr::mutate(
          # trim day 1 of data
          data = purrr::map(
            data,
            ~dplyr::filter(.x, lubridate::as_date(t) != lubridate::as_date(t)[1])
          )
        )
    }

    rep <- firsts %>%
      dplyr::bind_rows(others) %>%
      dplyr::arrange(as.numeric(i)) %>%
      dplyr::select(-i, -group)


  # join by id
  n_before_join <- nrow(rep)
  rep <- rep %>%
    dplyr::group_by(id, press, hum) %>%
    dplyr::mutate(
      sgmnt = 1:dplyr::n(),
      data  = purrr::map2(
        data,
        sgmnt,
        ~dplyr::mutate(.x, sgmnt = stringr::str_c(.y, sgmnt)))
    ) %>%
    dplyr::summarise(
      # collapse certain fields to retain information of the range of values
      serial = serial %>%
        unique() %>%
        stringr::str_c(collapse = ",") %>%
        stringr::str_remove("^,"),
      type   = type %>%
        unique() %>%
        stringr::str_c(collapse = ",") %>%
        stringr::str_remove("^,"),
      v_log  = v_log %>%
        unique() %>%
        stringr::str_c(collapse = ",") %>%
        stringr::str_remove("^,"),

      # pre-trimmed data can now just be bound together without any overlap
      data = data %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(t) %>%
        # dplyr::mutate(sgmnt = sgmnt %>% factor() %>% as.numeric()) %>%
        list(),

      # update stats
      nrow = nrow(data[[1]]),
      min  = min(data[[1]]$temp),
      max  = max(data[[1]]$temp),
      t0   = dplyr::first(data[[1]]$t),
      t1   = dplyr::last( data[[1]]$t),

      # update flags
      f_qual_checked = all(f_qual_checked),
      f_drift_correc = all(f_drift_correc),
      f_interpolated = all(f_interpolated),
      f_bound_serial = all(f_bound_serial),
      f_bound_id     = TRUE,

      .groups = "drop"
    ) %>%
    dplyr::relocate(press, hum, .after = v_log)

  ### warn ----
  # messages
  ## if issues were found...
  overlaps_id <- dplyr::filter(trimmed, trimmed > 0)
  if (nrow(overlaps_id)) {
    # flag excessive overlaps for review
    # the user is informed of excessive overlaps, but joining continues with youngest overlapping data being discarded
    # gather relevant metadata to address them

    dat$issues <- append_issues(
      paths    = overlaps_id$path_1st,
      step     = "join_id",
      issue    = "partial_overlap",
      new_vals = purrr::map(
        overlaps_id$t0,
        ~list(purge_time_bef = as.character(.x))),
      df       = dat$issues
    )

  }

  ## report about the joining process
  if (n_before_join > nrow(rep)) {
    msg <- bullets(c(), "v", glue::glue("reports joined by id ... (n = {n_before_join} -> {nrow(rep)})"), col = "green")
  } else {
    msg <- bullets(c(), "v", "no reports to join by id", col = "green")
  }
  dat$msg_bullets$join_id <- msg

  # return
  dat$report <- rep
  dat
}


# >> ----
# paths = env_example(); dat <- read_env(paths = paths, show_progress = FALSE, show_warnings = FALSE, correct_rtc_drift = TRUE, new_interval = 60, join_serials = TRUE, join_ids = TRUE, keep_flags = TRUE)
# env_full_days(dat)
env_full_days <- function(
  dat
) {
  dat$report <- dat$report %>%
    dplyr::mutate(
      # identify full days
      ## this code may lead to an incomplete day being retained if there's only one (incomplete) day of data
      ## but this is so unlikely and meaningless that it isn't worth coding for
      full_days = purrr::map(
        data,
        ~.x %>%
          dplyr::group_by(t = lubridate::floor_date(t, "days")) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::filter(n == max(n)) %>%
          dplyr::pull(t)
      ),

      # filter out incomplete days
      data = purrr::map2(
        data,
        full_days,
        ~dplyr::filter(
          .x,
          lubridate::floor_date(t, "days") %in% .y
        )
      ),

      # update stats
      nrow = purrr::map_dbl(data, ~nrow(.x)),
      min  = purrr::map_dbl(data, ~min(.x$temp)),
      max  = purrr::map_dbl(data, ~max(.x$temp)),
      t0   = purrr::map_vec(data, ~dplyr::first(.x$t)),
      t1   = purrr::map_vec(data, ~dplyr::last( .x$t))
    ) %>%
    dplyr::select(-full_days)

  # messages
  dat$msg_bullets$full <- bullets(c(), "v", "data filtered to only keep full days", col = "green")

  # return
  dat
}

# >> ----
# paths = env_example(); dat <- read_env(paths = paths, show_progress = FALSE, show_warnings = FALSE, correct_rtc_drift = TRUE, new_interval = 60, join_serials = TRUE, join_ids = TRUE, keep_flags = TRUE)
# x <- env_generate_fixes(dat)
# fs::file_delete(x$files_created)
env_generate_fixes <- function(
    dat
) {
  fixes   <- dat$issues %>% dplyr::filter(path_meta != "")
  n_files <- nrow(fixes)

  # if there are metadata files to be generated, do so
  new_files <- c()
  if (n_files) {
    new_files <- create_metadata_file(fixes, update = TRUE)
  }

  # messages
  if (!n_files) {
    # if nothing was done, let user know that there was nothing to do
    msg <- bullets(c(), "v", "no metadata files to generate", col = "green")
  } else {
    # otherwise, report the number of files generated
    msg <- bullets(c(), "v", glue::glue("new metadata files generated automatically (n = {n_files})"), col = "green")
  }
  dat$msg_bullets$meta <- msg

  # return
  dat$files_created <- new_files
  dat
}

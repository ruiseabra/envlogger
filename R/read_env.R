# >> ----
read_env_work <- function(
    paths,
    avoid_pattern,
    read_data,
    apply_fixes,
    log_summary,
    show_progress,
    qual_lims,
    correct_rtc_drift,
    new_interval,
    new_interval_margin,
    join_serials,
    join_ids ,
    overlap_max_mins,
    full_days,
    auto_generate_fixes
) {
  # [1] read files ----
  dat <- run_section(
    use_dat       = FALSE,
    fun           = read_env_all,
    section_nm    = "reading files",
    show_progress = show_progress,
    paths         = paths,
    avoid_pattern = avoid_pattern,
    read_data     = read_data,
    apply_fixes   = apply_fixes,
    log_summary   = log_summary
  )

  if (!is.null(dat$report)) {

    # [2] apply fixes ----
    if (apply_fixes) {
      dat <- run_section(
        dat           = dat,
        fun           = env_fix,
        section_nm    = "applying fixes (if available)",
        show_progress = show_progress
      )
    }

    # [3] quality checks ----
    dat <- run_section(
      dat           = dat,
      fun           = env_check_qual,
      section_nm    = "performing quality checks",
      show_progress = show_progress,
      lims          = qual_lims
    )

    # [4] correct clock drift ----
    if (correct_rtc_drift) {
      dat <- run_section(
        dat           = dat,
        fun           = env_rtc_drift,
        section_nm    = "correcting clock drift",
        show_progress = show_progress
      )
    }

    # [5] interpolate to round time ----
    if (!is.null(new_interval)) {
      dat <- run_section(
        dat           = dat,
        fun           = env_interpolate,
        section_nm    = "interpolate to round time",
        show_progress = show_progress,
        new_interval  = new_interval
      )
    }

    # [6] join by serial ----
    if (join_serials) {
      dat <- run_section(
        dat              = dat,
        fun              = env_join_by_serial,
        section_nm       = "joining by serial",
        show_progress    = show_progress,
        overlap_max_mins = overlap_max_mins
      )
    }

    # [7] join by id ----
    if (join_ids) {
      dat <- run_section(
        dat           = dat,
        fun           = env_join_by_id,
        section_nm    = "joining by ids",
        show_progress = show_progress
      )
    }

    # [8] filter full days ----
    if (full_days) {
      dat <- run_section(
        dat           = dat,
        fun           = env_full_days,
        section_nm    = "filtering full days",
        show_progress = show_progress
      )
    }

    # [9] generate metadata ----
    if (auto_generate_fixes) {
      dat <- run_section(
        dat           = dat,
        fun           = env_generate_fixes,
        section_nm    = "automatically generating metadata files",
        show_progress = show_progress
      )
    }
  }

  # return
  dat
}


# >> ----
#' Read EnvLogger data
#'
#' @description
#' Reads and organizes EnvLogger data and associated information. The following steps are executed sequentially:
#'
#' * `step 1` -- read files
#' * `step 2` -- apply fixes
#' * `step 3` -- check quality
#' * `step 4` -- correct RTC drift
#' * `step 5` -- interpolate to round time
#' * `step 6` -- join by serial
#' * `step 7` -- join by id
#' * `step 8` -- keep only full days
#' * `step 9` -- auto-generate metadata files
#'
#' @inheritParams env_ls
#' @param log_summary Should logfile entries be provided in full (`FALSE`) or should they be made more compact by only including the last entry for each serial?
#' @param show_progress Should progress messages be printed?
#' @param show_warnings If issues are detected, should final warning messages be printed?
#' @param read_data If set to `FALSE`, only the header information is gathered from the data reports. This is much quicker, and can be used to assess structure and issues from a large database before importing all data.
#' @param apply_fixes Whether to implement the fixes found in the metadata files.
#' @param qual_lims A list with the elements `min`, `max`, `t0`, `t1` (names are strict). These values will be used to identify quality issues in each data report (values outside the windows provided are flagged as issues). `min` and `max` correspond to the low and high temperature thresholds. `t0` and `t1` correspond to the time window accepted for the timestamps. Default values are `min = -30`, `max = +70`, `t0 = "2000-01-01"` and `t1 = Sys.Date()`, i.e., today.
#' @param correct_rtc_drift Whether to expand or compress timestamps to account for recorded RTC drift.
#' @param new_interval Desired sampling rate in minutes. If left `NULL` no interpolation takes place and data remains raw. If provided, data is interpolated to every `new_interval`-minutes. This is important for analyses that require data from all loggers to be at the same time points (every hour, every half-hour). Interpolation is aware of eventual gaps in the data and does not generate artificial data where none exists.
#' @param join_serials Whether to join logger reports relating to the same serial numbers.
#' @param join_ids If set to `TRUE`, `join_serials` is forced to `TRUE` as well. Whether to join logger reports relating to the same ids (information specific to individual report files is discarded).
#' @param overlap_max_mins Maximum allowed overlap length in minutes. When data from two files overlaps by less than the stipulated number of minutes, the situation is normal and handled silently. Otherwise, an issue is reported.
#' @param keep_flags Whether to retain columns flagging data processing steps, which are normally only used internally by [read_env()] and discarded before printing the output. Only relevant for debugging errors.
#' @param full_days Should logger data only be kept for days in which all data points were collected? (e.g., if sampling rate is 60 minutes, full days must have 24 data points).
#' @param auto_generate_fixes If issues are identified for which a simple fix can be automatically determined, should the corresponding metadata files be automatically created?
#' @param auto_implement_fixes If set to `TRUE`, `auto_generate_fixes` is forced to `TRUE` as well. After automatically generating metadata files to address issues identified, should data be re-imported again to immediately implement those fixes?
#'
#' @section RTC drift correction:
#' The correction implemented is linear and assumes that RTC drift accumulates at the same rate throughout the deployment. This assumption comes from the fact that all EnvLoggers feature temperature-compensated RTCs, which should result in drift being independent of temperature and only be affected by deployment length. EnvLoggers T2.4 accumulate about 10 to 15 minutes of RTC drift over a one-year deployment, which may be important to correct. EnvLoggers T7.3 and beyond feature a much more precise RTC and accumulate only 1 to 2 minutes of drift every year.
#' The correction applied is based on `tdiff`, a numeric value in seconds that represents the difference between logger RTC time (`obs`; observed) and smartphone time (`ref`; refference) as `(obs - ref)`. This means that if the logger RTC (`obs`) ticks quicker than `ref`, `tdiff` becomes positive.
#'
#' * drift = logger - smartphone
#' * pos -> logger time was faster -> timestamps must be compressed
#' * neg -> logger time was slower -> timestamps must be expanded
#'
#' @section Interpolation to round times:
#' Perform linear interpolation to tidy EnvLogger data so that there's one reading every hour (00 mins, 00 secs) or other specified interval; !ALWAYS! applied before joining data from different reports, as otherwise the interpolation would generate artificial data filling any gap that may be present.
#'
#'
#' @seealso [plot_env()]
#'
#' @return
#' A list with EnvLogger information (and data) from all reports, logfiles and metadata found in the target paths, divided into `$log`, `$metadata`, and `$report`. Additionally, if issues are found, the returned list may also include `$issues`, `$files_with_issues`, and `$files_created`.
#'
#' @export
#'
#' @examples
#' paths <- env_example("ptzzy", just_dir = TRUE)
#' read_env(paths, log_summary = TRUE)
# --- #
# paths <- "~/Dropbox/RS/bio/datasets/temp_loggers/reports_tidy/_finalized_1.0/ptcax"
# load_all(); paths <- env_example(); avoid_pattern = "env_archive"; log_summary = TRUE; show_progress = TRUE; show_warnings = TRUE; read_data = TRUE; apply_fixes = TRUE; qual_lims = list(min = -30, max = +70, t0 = "2000-01-01", t1 = Sys.Date()); correct_rtc_drift = TRUE; new_interval = 60; new_interval_margin = 5 * 60; join_serials = TRUE; join_ids = TRUE; overlap_max_mins = 60; keep_flags = FALSE; full_days = TRUE; auto_generate_fixes = TRUE; auto_implement_fixes = TRUE
# x <- read_env(env_example(), avoid_pattern, log_summary, show_progress, show_warnings, read_data, apply_fixes, qual_lims, correct_rtc_drift, new_interval, new_interval_margin, join_serials, join_ids, overlap_max_mins, keep_flags, full_days, auto_generate_fixes, auto_implement_fixes)
# fs::file_delete(x$new_metadata$path_meta)
# fs::file_delete(c("/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemc02a-04b4_3e00_c90f_0e-20240525_101121_low_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemh01a-04d3_5e00_b818_02-20240525_095727_gap_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemh02a-0449_0c00_2310_05-20240525_095804_1970_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/ptzzy/2025-01-14/ptzzymc01a-04ce_ce00_4114_06-20250114_111522_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/ptzzy/2025-01-14/ptzzymc02a-14cb_8f00_e758_05-20250114_111020_meta.csv"))
# fs::file_delete(c("/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemc02a-04b4_3e00_c90f_0e-20240525_101121_low_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemh01a-04d3_5e00_b818_02-20240525_095727_gap_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/issue/2024-05-25/issuemh02a-0449_0c00_2310_05-20240525_095804_1970_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/ptzzy/2025-01-14/ptzzymc01a-04ce_ce00_4114_06-20250114_111522_meta.csv", "/users/ruiseabra/dropbox/rs/bio/code2share/envlogger/inst/extdata/ptzzy/2025-01-14/ptzzymc02a-14cb_8f00_e758_05-20250114_111020_meta.csv")); load_all(); x <- read_env(env_example(), avoid_pattern, log_summary, show_progress, show_warnings, read_data, apply_fixes, qual_lims, correct_rtc_drift, new_interval, new_interval_margin, join_serials, join_ids, overlap_max_mins, keep_flags, full_days, auto_generate_fixes, auto_implement_fixes)
read_env <- function(
    paths,
    avoid_pattern        = NULL,
    log_summary          = FALSE,
    show_progress        = TRUE,
    show_warnings        = TRUE,
    read_data            = TRUE,
    apply_fixes          = TRUE,
    qual_lims            = list(min = -30, max = +70, t0 = "2000-01-01", t1 = Sys.Date()),
    correct_rtc_drift    = FALSE,
    new_interval         = NULL,
    new_interval_margin  = 5 * 60,
    join_serials         = TRUE,
    join_ids             = TRUE,
    overlap_max_mins     = 60 * 3,
    keep_flags           = FALSE,
    full_days            = FALSE,
    auto_generate_fixes  = FALSE,
    auto_implement_fixes = FALSE
) {
  ok <- is.list(qual_lims)
  if (ok) ok <- all(names(qual_lims) %in% c("min", "max", "t0", "t1"))
  if (!ok) cli::cli_abort(c("x" = "the arg 'qual_lims' must be a list that includes the following elements: min, max, t0, t1 (names are strict)"))

  if (join_ids) join_serials <- TRUE
  if (auto_implement_fixes) auto_generate_fixes <- TRUE

  # RUN 1 ----
  dat <- read_env_work(
    paths               = paths,
    avoid_pattern       = avoid_pattern,
    read_data           = read_data,
    apply_fixes         = apply_fixes,
    log_summary         = log_summary,
    show_progress       = show_progress,
    qual_lims           = qual_lims,
    correct_rtc_drift   = correct_rtc_drift,
    new_interval        = new_interval,
    new_interval_margin = new_interval_margin,
    join_serials        = join_serials,
    join_ids            = join_ids,
    overlap_max_mins    = overlap_max_mins,
    full_days           = full_days,
    auto_generate_fixes = auto_generate_fixes
  )

  # RUN 2 ----
  # if (auto_implement_fixes & !is.null(dat$issues)) {
  if (auto_implement_fixes & any(dat$issues$path_meta != "")) {
      issues <- dat$issues %>%
      dplyr::mutate(run2 = FALSE)

    if (show_progress) {
      msg <- glue::glue("importing data again to automatically implement {nrow(dplyr::filter(issues, path_meta != ''))} new fixes")
      div <- rep("=", nchar(msg)) %>% stringr::str_c(collapse = "")
      c() %>%
        bullets(" ", "") %>%
        bullets(" ", div) %>%
        bullets(" ", msg) %>%
        bullets(" ", div) %>%
        bullets(" ", "") %>%
        cli::cli_bullets()
    }


    # update paths
    paths$metadata <- c(
      paths$metadata,
      issues %>%
        dplyr::filter(path_meta != "") %>%
        dplyr::pull(path_meta)
      )

    dat <- read_env_work(
      paths               = paths,
      avoid_pattern       = avoid_pattern,
      read_data           = read_data,
      apply_fixes         = apply_fixes,
      log_summary         = log_summary,
      show_progress       = show_progress,
      qual_lims           = qual_lims,
      correct_rtc_drift   = correct_rtc_drift,
      new_interval        = new_interval,
      new_interval_margin = new_interval_margin,
      join_serials        = join_serials,
      join_ids            = join_ids,
      overlap_max_mins    = overlap_max_mins,
      full_days           = full_days,
      auto_generate_fixes = auto_generate_fixes
    )

    # keep info about metadata files created during RUN 1
    dat$issues <- if (!is.null(dat$issues)) {
      dplyr::bind_rows(
        issues %>% dplyr::filter(path_meta != ""),
        dat$issues)
    } else {
      issues
    }
  }

  ## [!] warnings ----
  msg <- c()

  # report files issues
  if (nrow(dat$issues)) {
    # tidy dat$issues for clear output
    dat$issues <- dat$issues %>%
        dplyr::mutate(
          fixed = path_meta != "" & !run2,
          issue = stringr::str_replace_all(issue, "_", " ")
        ) %>%
        dplyr::select(-run2)

    # simplified version of issues for printing messages
    issues <- dat$issues %>%
      dplyr::mutate(
        fn    = fs::path_file(path_data),
      ) %>%
      dplyr::select(
        step,
        fn,
        issue,
        fixed
      )

    # padding width
    fn_width <- issues$fn %>%
      stringr::str_length() %>%
      max() %>%
      magrittr::add(3)

    # message header
    msg <- msg %>%
      bullets("i", glue::glue("issues detected (total n = {nrow(issues)})")) %>%
      bullets("i", glue::glue("⚠️ type ", cli::col_br_magenta("?create_metadata_file()"), " to learn more about the issues listed below")) %>%
      bullets("i", glue::glue("️💾 access paths to files with unresolved issues using ", cli::col_br_magenta("$files_with_issues"))) %>%
      bullets("i", glue::glue("️💾 access paths to newly created metadata files using ", cli::col_br_magenta("$files_created"))) %>%
      bullets(" ", "")

    # unresolved issues
    bad <- dplyr::filter(issues, !fixed)
    if (nrow(bad)) {
      msg <- bullets(msg, "x", glue::glue("issues that remain unresolved (total n = {nrow(bad)})"), col = "red")

      # additional messages depending on presence of certain issue types
      if (any(bad$step == "unsupported")) {
        msg <- bullets(msg, ">", "⛔️ unsupported/no data files have not be imported")
      }
      if (any(bad$step == "quality")) {
        msg <- bullets(msg, ">", "🚧 files with quality issues have not be imported (issues must be resolved first)")
      }
      if (any(bad$issue == "full overlap")) {
        msg <- bullets(msg, ">", "🙈 files that fully overlap other files have been ignored")
      }
      if (any(bad$issue == "partial overlap")) {
        msg <- bullets(msg, ">", glue::glue("🔍 partial overlaps were resolved automatically, ", cli::style_underline("but consider doublechecking")))
      }

      # list files
      msg <- c(msg, path_issues_bullets(bad$fn, bad$issue, fn_width)) %>% bullets(" ", "")
    }

    # fixed issues
    ok <- dplyr::filter(issues, fixed)
    if (nrow(ok)) {
      msg <- bullets(msg, "v", glue::glue("issues that have resolved automatically (total n = {nrow(ok)})"), col = "green")

      # list files
      msg <- c(msg, path_issues_bullets(ok$fn, ok$issue, fn_width)) %>% bullets(" ", "")
    }
  }

  # log file warnings (not very solid, will need revision at some point)
  if (length(dat$msg_warn)) msg <- c(msg, dat$msg_warn)

  # print warnings
  if (show_warnings & length(msg)) cli::cli_warn(msg)

  ## [.] tidy reports ----
  if (!is.null(dat$report)) {
    ## discard flags
    if (!keep_flags) dat$report <- dplyr::select(dat$report, !dplyr::starts_with("f_"))
    ## add xts
    dat$report <- dat$report %>%
      dplyr::mutate(
        xts = purrr::map(
          data,
          ~xts::xts(
            dplyr::select(.x, -t),
            .x$t,
            tzone = "UTC")
        )) %>%
      dplyr::relocate(xts, .after = data)
  }

  ## [.] return ----
  dat$issues$run2 <- NULL
  dat$files_with_issues <- dplyr::filter(dat$issues, !fixed)$path_data
  dat$files_created     <- dplyr::filter(dat$issues, fixed)$path_meta

  nms <- intersect(
    names(dat),
    c("report", "log", "metadata", "issues", "files_with_issues", "files_created")
  )
  dat[nms]
}


### TO DOs ----
## update examples

## work on parse_id(dat, cctbon = FALSE, fields = (list(list(pos = 1:5, nm = "site", vals = "all"), list(pos = 6, nm = "lvl", vals = c("s", "t", "m", "l", "p")))))
## parse_id_field_chr, parse_id_field_dbl, parse_id_field_lgl, parse_id_field_fct
## work on add_solar(daylight y/n, sun height, nearest to sunrise, nearest to sunset, time after sunrise, time before sunset, time to solar max)
## work on add_tide, get_fes_tides
## work on summarise_id, summarize_id
## work on plot_env

# – parse_id_dbl(id, colname, start, end)
# – parse_id_fct(id, colname, start, end, levels)
# – parse_id_cctbon(id)
# – colnames append id_
# – remove data from first day when joining by id
# – tides

# marés viagens

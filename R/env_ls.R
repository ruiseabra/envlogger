# >>>> ----
#' List EnvLogger files
#'
#' @description
#' Search `paths` for EnvLogger files (EnvLogger reports, logfiles and metadata files).
#'
#' @param paths A character vector of one or more paths. May include folders, which are searched recursively.
#' @param avoid_pattern A character vector of one or more patterns. File paths matching to `avoid_pattern` are dropped.
#' @param list_unsupported Should unsupported files be included in the output?
#'
#' @return
#' A tibble with nine columns, including the file paths (`path`), two columns to indicate the type of files encountered, and six columns to facilitate quick filtering of specific file categories.
#'
#' In more detail, the two columns that indicate the type of files encountered are `$int` (numeric) and `$chr` (character), and their values are as follows:
#' * `1` and `report`      if an EnvLogger report or a valid report with a different format and an associated metadata file
#' * `2` and `log`         if an EnvLogger logfile
#' * `3` and `metadata`    if an EnvLogger metadata file
#' * `0` and `unsupported` if none of the three
#'
#' The remaining columns are logical vectors for quick filtering, namely:
#' * `rep`       - path points to a valid report of any kind?
#' * `rep_env`   - path points to an EnvLogger report?
#' * `rep_other` - path points to a report with a different format?
#' * `has_met`   - path points to a report with an associated metadata file?
#' * `log`       - path points to an EnvLogger logfile?
#' * `met`       - path points to an EnvLogger metadata file?
#'
#' @section Details:
#' `env_ls()` determines if the paths provided point to EnvLogger reports, logfiles, metadata files or none of the three.
#' This is done by checking for the presence of certain strings in the header of each file (such as "www.electricblue.eu, Portugal").
#' As a result, reports generated using the the earliest versions of the EnvLogger_Viewer app may not be recognized.
#' Also, if the structure of a file has been changed (e.g., by opening and saving in Excel), it may no longer be recognized.
#'
#' @section Non-EnvLogger data reports:
#' Otherwise unsupported files that are accompanied by an associated metadata file are assumed to be valid report files created by devices outside of the EnvLogger ecosystem, and will therefore be listed as "valid reports" (but this is no guarantee that they will be successfully read by downstream functions).
#'
#' @seealso [read_env()], [metadata_create_file()], [env_example()]
#' @export
#'
#' @examples
#' paths <- env_example()
#' env_ls(paths)
# --- #
# paths <- env_example(); avoid_pattern = NULL; list_unsupported = TRUE
# env_ls(paths, avoid_pattern, list_unsupported)
env_ls <- function(
    paths,
    avoid_pattern = NULL,
    list_unsupported = FALSE
) {
  # support for env_example lists
  nms <- c("unsupported", "metadata", "log", "report")
  if (is.list(paths)) if (all(names(paths) %in% nms)) paths <- purrr::list_c(paths)

  # search within directories provided
  file_paths <- tibble::tibble(
    path  = fs::path_abs(paths),
    isdir = file.info(path)$isdir
  ) %>%
    split(.$isdir)

  # files are already accessible
  if (!is.null(file_paths$`FALSE`)) file_paths$`FALSE` <- file_paths$`FALSE`$path
  # folders must be recursively searched for files
  if (!is.null(file_paths$`TRUE`))  file_paths$`TRUE`  <- file_paths$`TRUE`$path %>% fs::dir_ls(recurse = TRUE, type = "file")

  # merge
  file_paths <- file_paths %>%
    purrr::list_c() %>%
    unique() %>%
    stringr::str_to_lower()

  # ...if any paths available
  if (length(file_paths)) {
    # drop files matching avoid_pattern
    file_paths <- avoid_pattern %>%
      c("env_archive") %>%
      purrr::map(~stringr::str_subset(file_paths, .x, negate = TRUE)) %>%
      purrr::reduce(intersect)

    # check file types and status
    file_paths <- tibble::tibble(
      path = file_paths,

      # detect files for which there's an associated metadata file
      ## when that is the case for unsupported files, they are in turn assumed to be valid reports that do not conform to the envlogger format, but that must be imported anyway through parsing
      has_met = file_paths %>%
        fs::path_ext_remove() %>%
        stringr::str_c("_meta.csv") %>%
        magrittr::is_in(file_paths)
    ) %>%

      dplyr::mutate(
        # read header
        l = purrr::map_chr(path, ~.x %>%
                             readr::read_lines(
                               n_max = 20,
                               progress = FALSE) %>%
                             stringr::str_to_lower() %>%
                             stringr::str_flatten_comma()),

        # find env rep strings
        is_env = purrr::map_lgl(l, ~ c(
          "www.electricblue.eu",
          "envlogger"
        ) %>%
          stringr::str_detect(.x, .) %>%
          any()),

        # find log strings
        is_log = purrr::map_lgl(l, ~ c(
          "tap logger",
          "data downloaded",
          "mission running",
          "waiting to start"
        ) %>%
          stringr::str_detect(.x, .) %>%
          any()),

        # find metadata strings
        is_met = purrr::map_lgl(l, ~ c(
          "metadata"
        ) %>%
          stringr::str_detect(.x, .) %>%
          any()),

        # compute numeric representation
        int = 0,
        int = dplyr::if_else(is_env, 1, int),
        int = dplyr::if_else(!is_env & has_met, 2, int),
        int = dplyr::if_else(is_log, 3, int),
        int = dplyr::if_else(is_met, 4, int),

        # compute string representation
        chr = dplyr::case_when(
          int == 0 ~ "unsupported",
          int == 1 ~ "report",
          int == 2 ~ "report",
          int == 3 ~ "log",
          int == 4 ~ "metadata"
        ),

        # logical is TRUE if corresponds to any valid Envlogger file
        lgl = int != 0,

        # additional columns to facilitate subsetting
        # csv  = fs::path_ext(path) == "csv",
        rep       = int == 1 | int == 2,
        rep_env   = int == 1,
        rep_other = int == 2,
        log = int == 3,
        met = int == 4,

        int = dplyr::if_else(int > 1, int - 1, int)
      ) %>%
      dplyr::select(-l, -dplyr::starts_with("is_"))
  } else {
    # no valid files found
    cli::cli_abort(glue::glue("'paths' doesn't point to ", cli::col_red("ANY"), " file"))
  }

  # tidy
  file_paths <- file_paths %>%
    dplyr::relocate(has_met, .after = "rep_other") %>%
    dplyr::arrange(int, path)

  # stop/warn if none of the paths point to EnvLogger files
  msg <- c(
    "x" = stringr::str_c("'paths' doesn't point to ", cli::col_red("ANY"), " EnvLogger file or folder(s) containing EnvLogger files"),
    "i" = "supported files: EnvLogger reports, logfiles and metadata files"
  )

  if (all(!file_paths$lgl)) {
    if (list_unsupported) cli::cli_warn(msg) else cli::cli_abort(msg)
  }

  # return
  if (!list_unsupported) file_paths <- dplyr::filter(file_paths, lgl)
  dplyr::select(file_paths, -lgl)
}


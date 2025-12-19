# >>>> ----
#' Get paths to example files
#'
#' @description
#' The [envlogger-package] comes bundled with several sample files in its inst/extdata directory. `env_example()` provides a quick shortcut to access the paths to these sample files.
#'
#' @param pattern Patterns to select one or more example files. `pattern` is vectorized, so all values supplied are used. If `NULL` (default) all example files are returned.
#' @param just_dir If `TRUE` only folder paths are returned.
#' @param delete_new_metadata_files When executing the examples provided for some functions in the [envlogger-package], new metadata files may be created, which in turn may limit the scope of other examples. If set to `TRUE`, any metadata file that isn't part of the original example files bundle is deleted, allowing for a clean slate.
#'
#' @return
#' A named list with the full path to one or more example files, grouped by EnvLogger file type. If `just_dir = TRUE`, a character vector with folder paths.
#'
#' @section Details:
#' The sample files provided consist of EnvLogger reports, logfiles and metadata files concerning several years of data from a few rocky shores in northern Portugal and Norway.
#' The content in some of the files has been modified to introduce issues that exemplify some of the package's quality control features.
#' Files include:
#' * "normal" data, just temperature - `nozzz`, `ptzzw`, and `ptzzy`
#' * "normal" data, just temperature, different format - `notenv`
#' * "normal" data, temperature and pressure - `press`
#' * "normal" data, temperature and humidity - `humid`
#' * "abnormal" data, with several quality issues - `issue`
#'
#' @seealso [read_env()], [metadata_create_file()], [env_ls()]
#' @export
#'
#' @examples
#' # Get the file paths of all example files
#' env_example()
#'
#' # Get the file paths to a target serial
#' env_example("04CB_CC00_1507_0C")
#'
#' # Get the file paths to any example files matching a search string
#' env_example("20250114")
#'
#' # Get only logfiles
#' env_example()$log
#'
#' # Get only folder paths
#' env_example(just_dir = TRUE)
#'
#' # 'env_example()' is vectorized, meaning that multiple search strings can be used at once
#' env_example(c("ptzzy", "nozzz"))
# --- #
# pattern  = "."; just_dir = FALSE
# env_example(pattern, just_dir)
env_example <- function(
    pattern  = NULL,
    just_dir = FALSE,
    delete_new_metadata_files = FALSE
) {
  if (is.null(pattern)) pattern <- "."
  if (any(pattern == "")) pattern[pattern == ""] <- "."

  folder <- system.file("extdata", package = "envlogger")
  files  <- folder %>%
    env_ls(list_unsupported = TRUE) %>%
    dplyr::filter(
      purrr::map_lgl(path, ~.x %>%
                       stringr::str_detect(stringr::str_to_lower(pattern)) %>%
                       any())
    )

  if(delete_new_metadata_files) {
    to_drop <- tibble::tibble(
      path = files %>%
        dplyr::filter(met) %>%
        dplyr::pull(path)
    ) %>%
      dplyr::mutate(
        fn = fs::path_file(path),
        keep = purrr::map_lgl(
          fn,
          ~stringr::str_detect(example_files_to_keep, .x) %>% any()
        )
      ) %>%
      dplyr::filter(!keep)

    if (nrow(to_drop)) fs::file_delete(to_drop$path)

    # fetch file list again
    files  <- folder %>%
      env_ls(list_unsupported = TRUE) %>%
      dplyr::filter(
        purrr::map_lgl(path, ~.x %>%
                         stringr::str_detect(stringr::str_to_lower(pattern)) %>%
                         any())
      )
  }

  if (just_dir) {
    files$path %>% fs::path_dir() %>% unique()
  } else {
    split(files$path, files$chr)
  }
}

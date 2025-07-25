#' Decompose CCTBON ids into their elements and append to the original data
#'
#' @description
#' Extract country, shore, microhabitat level, exposure and replicate number from standard CCTBON ids and convert to level and exposure to factors with properly ordered factor levels.
#'
#' @param env_data a tibble generated by `READ_ENV(..., just_rep = TRUE)` or with equivalent format with a column named `$id`, containing logger names structured according to the CCTBON scheme.
#' @param new_sh string, defaults to `NULL`; new shore name to replace any instance of shore name that doesn't comply with the standard 5-chars (nowadays always 5, but previously just 4)
#'
#' @seealso [read_env_all()], [READ_ENV()]
#'
#' @return
#' A tibble with `$sh`, `$lvl`, `$exp`, `$mic` and `$rep`, each containing the parsed values for each element of the CCTBON id.
#'
#' @section CCTBON IDs:
#' EnvLogger custom IDs can be up to 10 characters long. To follow the CCTBON network's scheme, IDs must be structured strictly as follows:
#' - 2 char for country code
#'    - must follow standard 2-letter country codes
#'    - e.g., "pt" for Portugal
#' - 3 char for site
#'    - e.g., "ang" for Angeiras
#'    - any combination is accepted (including with numbers)
#'    - HOWEVER, the first 5 chars (country + site, i.e., the full site name) must be unique
#' - 1 char for shore height; strictly, one of the following:
#'    - "s" for supratidal
#'    - "t" for top-shore
#'    - "m" for mid-shore
#'    - "l" for low-shore
#'    - "p" for tide-pool
#' - 1 char for solar exposure; strictly, one of the following:
#'    - "h" for hot
#'    - "c" for cold
#'    - "w" for west
#'    - "e" for east
#'    - "i" for in "o" for out (associated with tide-pools)
#' - 2 digits for replicate number
#'     - e.g., "01"
#' - 1 char for sub-replicate
#'     - e.g., "a"
#'     - should be used sequentially in alphabetical order
#'
#' For example, ID "ptminmh02a" refers to the 2nd logger deployed in the Portuguese shore of Mindelo, at the mid-shore level, with maximum exposure to the sun.
#'
#' @export
#'
#' @examples
#' paths <- env_example("ptzzy", dir = TRUE)
#' x <- READ_ENV(paths)
#' cctbon_id(x)
cctbon_id <- function(env_data, new_sh = NULL) {
  if (class(env_data)[1] == class(list())) env_data <- env_data$rep
  id  <- env_data$id
  sh  <- stringr::str_sub(id, end = -6)
  mic <- stringr::str_sub(id, start = -5)
  if (!is.null(new_sh)) sh <- ifelse(nchar(sh) != 5, new_sh, sh)
  id <- paste0(sh, mic)
  env_data$id <- id

  lvls <- c("l", "m", "t", "s")
  exps <- c("c", "e", "h", "w", "p")

  country <- stringr::str_sub(id, 1, 2)

  lvl  <- stringr::str_sub(id, 5 + 1, 5 + 1)
  lvl  <- factor(
    lvl,
    levels = c(
      lvls,
      setdiff(lvl %>% unique(), lvls)
    ))

  exp  <- stringr::str_sub(id, 5 + 2, 5 + 2)
  exp  <- factor(
    exp,
    levels = c(
      exps,
      setdiff(exp %>% unique(), exps)
    ))

  rep <- stringr::str_sub(id, 5 + 3, 5 + 4)

  mic   <- paste0(lvl, exp)
  micro <- paste0(mic, rep)

  cctbon <- tibble::tibble(
    # country = country,
    sh    = sh,
    micro = micro,
    lvl   = lvl,
    exp   = exp,
    mic   = mic,
    rep   = rep
  )

  dplyr::bind_cols(cctbon, env_data)
}

#' Group CCTBON data by microhabitat and summarise it
#'
#' @description
#' Take CCTBON data and summarise daily and by microhabitat (i.e., discard logger by logger granularity and return min > avg > max stats for each day)
#'
#' @param cctbon_data a tibble generated by `READ_ENV(..., just_rep = TRUE)`, with logger names structured according to the CCTBON scheme.
#' @param by_micro logical, defaults to `FALSE`; whether data should be grouped at the microhabitat level.
#' @param by_shore logical, defaults to `FALSE`; whether data should be grouped at the shore level (ignored if `by_micro = TRUE`).
#' @param by_day logical, defaults to `TRUE`; whether data should be grouped daily (applied before `roll_days`).
#' @param roll_days integer, defaults to `0`; number of days over which to group summarised data (rolling mean); if `0`, data is returned without temporal summarisation.
#'
#' @export
#' @examples
#' paths <- env_example(c("ptzzw", "nozzz"), dir = TRUE)
#' x <- READ_ENV(paths)
#' x <- cctbon_id(x)
#' cctbon_summarise(x)
cctbon_summarise <- function(
    cctbon_data,
    by_micro  = FALSE,
    by_shore  = FALSE,
    by_day    = TRUE,
    roll_days = 0
) {
  roll_days <- floor(roll_days)

  cctbon_data <- tidyr::unnest(cctbon_data, cols = "data")

  if (!by_micro & !by_shore & !by_day) {
    cctbon_data
  } else {
    if (by_day) cctbon_data <- dplyr::mutate(cctbon_data, t = as.Date(t))

    x <- dplyr::group_by(cctbon_data, t)

    if (by_shore) x <- dplyr::group_by(x, sh, id = sh, .add = TRUE)
    if (by_micro) x <- dplyr::group_by(x, sh, lvl, exp, mic, id = paste(sh, mic, sep = "_"), .add = TRUE)
    if (!by_shore & !by_micro) x <- dplyr::group_by(x, sh, lvl, exp, mic, id, .add = TRUE)

    x <- x %>%
      dplyr::summarise(
        lvl = lvl %>% unique() %>% paste(collapse = ","),
        exp = exp %>% unique() %>% paste(collapse = ","),
        mic = mic %>% unique() %>% paste(collapse = ","),
        min = min(            temp,       na.rm = TRUE),
        q10 = stats::quantile(temp, 0.10, na.rm = TRUE),
        q25 = stats::quantile(temp, 0.25, na.rm = TRUE),
        avg = mean(           temp,       na.rm = TRUE),
        q50 = stats::quantile(temp, 0.50, na.rm = TRUE),
        q75 = stats::quantile(temp, 0.75, na.rm = TRUE),
        q90 = stats::quantile(temp, 0.90, na.rm = TRUE),
        max = max(            temp,       na.rm = TRUE),
        .groups = "drop"
      )

    if (roll_days) {
      k_per_day <- x %>%
        dplyr::arrange(t) %>%
        dplyr::pull(t) %>%
        unique() %>%
        diff()
      units(k_per_day) <- "mins"
      k_per_day <- k_per_day %>% as.numeric() %>% min()
      k_per_day <- 24 * 60 / k_per_day
      x <- x %>%
        dplyr::group_by(sh, lvl, exp, mic, id) %>%
        dplyr::mutate(dplyr::across(c("min", "q10", "q25", "avg", "q50", "q75", "q90", "max"), ~zoo::rollmean(.x, k = k_per_day * roll_days, align = "center", fill = NA)))
    }
    dplyr::ungroup(x)
  }
}

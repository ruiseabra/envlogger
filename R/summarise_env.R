# >>>> ----
#' Group EnvLogger data by id elements and summarise it
#'
#' @description
#' Takes an EnvLogger tibble resulting from a call to [read_env()], groups data by grouping variables and summarises it. In the simplest case, grouping is only done along time (`by_day`). However, if logger ids have been broken into id elements with [parse_id()] or [parse_id_cctbon()], grouping can also be done across id elements. For example, if data follows the CCTBON naming scheme, data can be grouped by `site`, `lvl` (shore height level) or `exp` (exposure to solar radiation). This function exists mainly to condition data for [plot_env()], but can also be useful by itself.
#'
#' Note that `summarise_env` can be called on the output of a previous call to `summarise_env`, allowing, for example, for first computing daily maximums, and then on a second call, to request an average of those daily maximums across all loggers sharing the same microhabitat.
#'
#' @param env The output of a call to [read_env()] (either directly, of the `$report` element). Additionally, a call to [parse_id()] or [add_info()] ensures more grouping options. In alternative, the output of a call to `summarise_env`.
#' @param var The name of a single column over which the function(s) in `fun_list` will be applied. Defaults to `temp`. Can also be set to `hum` or `press` if wanting to summarise using humidity or pressure data, respectively. Can also be any other name, as long as it exists as colnames and codes for a data type that will result in a single numeric value when processed by all functions provided in `fun_list`.
#' @param fun_list Named list with one or more functions to be applied to `var`. Each function must return a single numeric value. The names of the columns derived from applying the functions provided are taken from the names of `fun_list`. If `fun` has no names, generic names are generated. If not provided, the following stats are computed: min, q10 (10th quantile), q25, average, q50, q75, q90 and max.
#' @param by_day Whether to summarise values daily or at the native sampling interval.
#' @param by_site Whether to group by `site`.
#' @param by_group Whether to group by `group`.
#' @param site The name of the column that codes for site. If a vector with more than one element, elements are combined to form a new column by pasting values together. Only relevant if `by_site = TRUE`. Can be omitted if data follows the CCTBON naming scheme.
#' @param group One or more column names to combine as a grouping factor. If a vector with more than one element, elements are combined to form a new column by pasting values together. Only relevant if `by_group = TRUE`. Can be omitted if data follows the CCTBON naming scheme (in which case `lvl` and `exp` are used).
#' @param same_days Should the output only include data for days for which all distinct grouping ids (site + group) have data?
#' @param roll_days Compute a rolling mean over the x days before each data point. If `0` (default), no rolling mean is applied.
#' @param by_day_lim Maximum number (in thousands) of collective rows accepted when `by_day = FALSE`. If the aggregate number of rows exceeds this value, `by_day` will be automatically set to `TRUE`. This is to prevent an unintentional call requesting the computation of summary stats over datasets that are too large, a situation that may lead to R stalling.
#'
#' @return
#' A tibble with unnested data containing all the combinations of `id` and `t` (time), as well as `site` and `group` if `by_site = TRUE` and `by_group` = TRUE, respectively, along with as many data summary columns as the number of functions provided using `fun_list`.
#'
#' @export
#'
#' @seealso [read_env()], [parse_id()], [add_info()], [plot_env()]
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' env <- parse_id_cctbon(env)
#'
#' # daily max for each logger
#' daily_max <- summarise_env(
#'    env,
#'    var = "temp",
#'    fun_list = list(max = max),
#'    by_day = TRUE
#'    )
#' daily_max
#'
#' # daily minimum, average and maximum daily_max for each site
#' summarise_env(
#'    daily_max,
#'    var = "max",
#'    fun_list = list(
#'        dmax_min = min,
#'        dmax_avg = mean,
#'        dmax_max = max,
#'        sd = sd
#'        ),
#'    by_day = TRUE,
#'    by_site = TRUE,
#'    same_days = TRUE
#'    )
# --- #
# env <- read_env(env_example(c("ptzzy", "ptzzw")), new_interval = 60, show_progress = FALSE, show_warnings = FALSE) %>% parse_id_cctbon(); fun_list = NULL; by_site = TRUE; by_group = TRUE; by_day = TRUE; roll_days = 2; site = "site"; group = c("lvl", "exp"); var = "temp"; same_days = FALSE
# summarise_env(env, by_day = by_day, by_site = by_site, by_group = by_group, roll_days = roll_days)
summarise_env <- function(
    env,
    var        = "temp",
    fun_list   = NULL,
    by_day     = TRUE,
    by_site    = FALSE,
    by_group   = FALSE,
    site       = "site",
    group      = c("lvl", "exp"),
    same_days  = FALSE,
    roll_days  = 0,
    by_day_lim = 50
) {
  discard_cols <- setdiff(c("serial", "temp", "hum", "press", "nrow", "min", "max", "t0", "t1", "v_log", "type"), var)

  x <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # check if var exists in the data columns
  data_cols <- if ("data" %in% colnames(x)) {
    x %>%
      dplyr::pull(data) %>%
      purrr::map(colnames) %>%
      purrr::list_c() %>%
      unique()
  } else {
    colnames(x)
  }
  has_data_cols <- data_cols %>%
    magrittr::equals(var) %>%
    any()
  if (!has_data_cols) cli::cli_abort(c(
    "x" = glue::glue("couldn't find ", cli::col_magenta("var"), " ({var})")
  ))

  # adjust params
  by_day <- by_day | roll_days
  if (is.null(fun_list)) fun_list <- summarise_funs
  if (is.null(names(fun_list))) names(fun_list) <- stringr::str_c("fun", 1:length(fun_list))

  # rearrange data
  if ("data" %in% colnames(x)) x <- x %>%
    dplyr::select(!dplyr::any_of(c(discard_cols, "hum", "press"))) %>%
    tidyr::unnest(cols = data)

  x <- x %>%
    dplyr::relocate(sgmnt, .before = t) %>%
    dplyr::select(!dplyr::any_of(discard_cols)) %>%
    dplyr::rename(var = dplyr::matches(var))

  ## don't allow by_day be TRUE if the amount of data available to process is too large
  if (!by_day) {
    nrow <- x %>%
      nrow() %>%
      magrittr::divide_by(1000) %>%
      round()
    if (nrow > by_day_lim & (by_site | by_group)) {
      by_day <- TRUE
      cli::cli_warn(c(
        "!" = glue::glue("the aggregate number of rows ({nrow}K) exceeds ", cli::col_magenta("by_day_lim"), " ({by_day_lim}K)"),
        ">" = glue::glue(cli::col_magenta("by_day"), " was therefore switched to ", cli::col_red("TRUE")),
        "i" = glue::glue("set ", cli::col_magenta("by_day_lim"), " to {nrow + 1} or more to bypass this automated change")
      ))
    }
  }

  # if grouping by 'site', make sure colname(s) exists
  if (by_site) {
    if (!all(site %in% colnames(x))) cli::cli_abort(c("x" = "failed to group by 'site'", "i" = "the value(s) provided couldn't be found in any colnames"))

    # collapse columns provided for 'site' and make into a factor
    x$site <- x %>%
      dplyr::select(dplyr::all_of(site)) %>%
      apply(1, function (x) stringr::str_c(x, collapse = "")) %>%
      factor()
  }

  # if grouping by 'group', make sure colname(s) exists
  if (by_group) {
    if (!all(group %in% colnames(x))) cli::cli_abort(c("x" = "failed to group by 'group'", "i" = "the value(s) provided couldn't be found in any colnames"))

    # collapse columns provided for 'group'
    x$group <- x %>%
      dplyr::select(dplyr::all_of(group)) %>%
      apply(1, function (x) stringr::str_c(x, collapse = ""))

    # make into a factor
    levels_group <- x$group %>% unique() %>% sort()
    x <- dplyr::mutate(x, group = factor(group, levels = levels_group))
  }

  new_cols <- setdiff(colnames(x), c(env_cols, "var", "t"))

  # begin grouping
  if (!by_group & !by_site & !by_day) {
    # return
    x
  } else {
    # 1st, group by day (done by flooring all dates to day)
    if (by_day) x <- dplyr::mutate(x, t = lubridate::floor_date(t, unit = "day"))

    # if by_day was FALSE, the following grouping does nothing, otherwise, it introduces the grouping by day
    # nevertheless, this line mustn't be placed inside of the 'if' statement, because it must always be run, so that the grouping is initiated
    x <- dplyr::group_by(x, t)

    # 2nd, add group by site
    if (by_site) x <- dplyr::group_by(x, id = site, site, .add = TRUE)

    # finally, add group by group
    if (!by_site & by_group) x <- dplyr::group_by(x, id = group, group, .add = TRUE)
    if (by_site  & by_group) x <- dplyr::group_by(x, id = stringr::str_c(site, "_", group), group, .add = TRUE)

    # if not grouping by site or by group, retain all new_cols
    if (!by_site & !by_group) x <- dplyr::group_by(x, id, dplyr::across(dplyr::all_of(new_cols)), .add = TRUE)

    x <- x %>%
      dplyr::summarise(
        purrr::map(fun_list, ~.x(var)) %>% tibble::as_tibble(),
        .groups = "drop"
      )

    if (roll_days) {
      stats <- names(fun_list)
      new_cols <- setdiff(colnames(x), c("t", stats))

      k_per_day <- x %>%
        dplyr::arrange(t) %>%
        dplyr::pull(t) %>%
        unique() %>%
        diff()
      units(k_per_day) <- "mins"
      k_per_day <- k_per_day %>% as.numeric() %>% min()
      k_per_day <- 24 * 60 / k_per_day

      x <- x %>%
        dplyr::group_by(
          dplyr::across(dplyr::all_of(new_cols))
        ) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(stats),
            ~slider::slide_dbl(
              .x,
              .f = mean,
              .before = k_per_day * roll_days,
              .complete = TRUE) %>%
              tidyr::replace_na(NA)
          )
        ) %>%
        tidyr::drop_na()
    }

    # filter for days shared between all ids
    x <- x %>%
      dplyr::ungroup() %>%
      dplyr::relocate(t, .before = names(fun_list)[1])

    if (same_days) {
      n_ids  <- x$id %>% unique() %>% length()
      shared_days <- x %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          d = t %>% lubridate::as_date() %>% unique() %>% list()
        ) %>%
        tidyr::unnest(d) %>%
        dplyr::group_by(d) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::filter(n == n_ids) %>%
        dplyr::pull(d)

      if (length(shared_days)) {
        x <- x %>%
          dplyr::filter(
            lubridate::as_date(t) %in% shared_days
          )
      } else {
        cli::cli_warn(c(
          "!" = glue::glue("failed to enforce ", cli::col_magenta("same_days")),
          "i" = "there are no days shared by all id groups",
          ">" = "output includes all available entries"
        ))
      }
    }

    # x %>%
    #   # dplyr::select(!dplyr::all_of(names(fun_list))) %>%
    #   tidyr::complete(!!!rlang::syms(c("id", "site", "group", "t")))

    # return
    x
  }
}

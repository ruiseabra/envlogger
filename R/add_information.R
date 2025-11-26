# >>>> ----
#' Parse structured logger ids
#'
#' @description
#' If the logger ids used follow a stable naming scheme, [parse_id()] can be used to generate new columns that hold each naming segment. [cctbon_id()] provides a quick shortcut for handling data that follows the CCTBON id scheme.
#'
#' @param env The output of a call to [read_env()] (either directly, of the `$report` element).
#' @param div A sequence of numbers defining the length of each id element. Can be provided as a number (1111123445) or as a string ("1111123445"). Numbers must be consecutive and same numbers must be contiguous. Same numbers define the position and width of the desired element in the ids.
#' @param fields A list providing the names and data types of each id elements. `fields` must have as many elements as those outlined in `div`. Certain downstream functions such as [add_info()] expect a column named `site` coding location name. Check the `Fields` section below for more information on the structure of `fields`.
#' @param standard In alternative to setting `div` and `fields`, provide the name of one of the supported naming schemes and the other args will be set automatically to the expected values. Current schemes supported are: 'cctbon'. Check the details sections below for more information.
#'
#' @return
#' A tibble with EnvLogger report data, augmented with new columns resulting from the parsing of data report ids.
#'
#' @section standard = CCTBON:
#' If the logger ids follow scheme used by the CCTBON network (<https://www.coastalwarming.com/cctbon>), simply use [cctbon_id()] instead.
#'
#' To conform with the CCTBON scheme, ids must:
#' * always be 10 chr long
#' * always contain the following elements in the exact order as outlined next:
#' * 5 chr (#1-5) for site name (must be unique in the network)
#' * 1 chr (#6) for shore level (l, m or t = low, mid or top intertidal, s = supratidal)
#' * 1 chr (#7) for solar exposure (c = cold, h = hot)
#' * 2 chr (#8-9) for microhabitat replicate number (01, 02, 03, ...)
#' * 1 chr (#10) for logger position replicate number (a, b, c, ...)
#'
#' @section Fields:
#' We can use the CCTBON scheme to better illustrate the structure of `fields`. In that case:
#' * `div = "1111123445"`
#' * `fields = list(site = "fct", lvl  = c("l","m","t","s"), exp  = c("c","h"), rep1 = "num", rep2 = "chr")`
#'
#' This will result in chars #1-5 being assigned to column `site`, char #6 to column `lvl`, char #7 to column `exp`, and so on.
#' Moreover, the data types are interpreted as follows:
#' * `fct` results in a factor with levels set to the unique values available
#' * `chr` results in a character vector (as.character)
#' * `num` results in a numeric vector (as.numeric)
#' * when the type provided has a length greater than one (as is the case for `lvl` and `exp` in the CCTBON scheme), results in a factor with levels set to the values provided as type
#'
#' As long as (a) the number of fields provided matches the number of different numbers supplied in `div`, (b) `fields` follows the structure `field name = data type`, (c) data types are one of 'chr', 'num', 'fct', or c('a', 'b'), and (d) data type conversions are possible without generating `NA`s, any file scheme can be parsed.
#'
#' @export
#'
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' # If ids follow a support standard
#' parse_id(env, standard = "cctbon")
#' cctbon_id(env) # even shorter
#'
#' # If using a different scheme
#' parse_id(
#'    env,
#'    div = "1111123445",
#'    fields = list(
#'        site = "fct",
#'        lvl  = c("l","m","t","s"),
#'        exp  = c("c","h"),
#'        rep1 = "num",
#'        rep2 = "chr")
#'        )
# --- #
# env <- read_env(env_example(c("ptzzy", "ptzzw")), new_interval = 60, show_progress = FALSE, show_warnings = FALSE); div = fields = NULL; standard = "cctbon"
# parse_id(env, standard = standard)
# cctbon_id(env)
parse_id <- function(
    env,
    div      = NULL,
    fields   = NULL,
    standard = NULL
) {
  rep <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # ids must have same length
  id_nchar <- rep$id %>% nchar() %>% unique()
  if (length(id_nchar) != 1) cli::cli_abort(c("x" = "ids must all have the same length"))

  # adjust to available standards
  if (!is.null(standard)) {
    done <- FALSE

    standard <- stringr::str_to_lower(standard)
    if (standard %in% names(parse_id_standards)) {
      div    <- parse_id_standards[[standard]]$div
      fields <- parse_id_standards[[standard]]$fields
      done   <- TRUE
    }

    if (!done) cli::cli_abort(c(
      "x" = "couldn't recognize the standard requested",
      "i" = glue::glue("standards available: ", stringr::str_c(names(parse_id_standards), collapse = ", "))
    ))
  }

  # either div + field or a valid standard must be provided
  if (is.null(div) | is.null(fields)) cli::cli_abort(c(
    "x" = "either provide 'div' and 'fields' or select a 'standard'",
    "i" = glue::glue("standards available: ", stringr::str_c(names(parse_id_standards), collapse = ", "))
  ))

  # slice ids into the prescribed fields
  N <- length(rep$id)
  ids_fields <- tibble::tibble(
    val  = rep$id %>%
      stringr::str_split("") %>%
      purrr::list_c(),
    i1 = rep$id %>%
      seq_along() %>%
      rep(each = nchar(div)) %>%
      formatC(width = nchar(N), flag = "0"),
    i2 = div %>%
      stringr::str_split_1("") %>%
      rep.int(times = N),
    i  = stringr::str_c(i1, "_", i2)
  ) %>%
    dplyr::group_by(i, i1, i2) %>%
    dplyr::summarise(val = stringr::str_c(val, collapse = ""), .groups = "drop") %>%
    tibble::add_column(field = rep.int(names(fields), times = N)) %>%
    dplyr::arrange(i1, i2) %>%
    dplyr::select(i = i1, field, val) %>%
    tidyr::pivot_wider(
      names_from = "field",
      values_from = "val"
    )

  # convert to the desired data types
  for (f in names(fields)) {
    type <- fields[[f]]
    vals <- ids_fields[[f]]
    if (length(type) > 1) type <- "fct2"
    if (type == "chr")  ids_fields[[f]] <- as.character(vals)
    if (type == "num")  ids_fields[[f]] <- suppressWarnings(as.numeric(vals))
    if (type == "fct")  ids_fields[[f]] <- suppressWarnings(factor(vals, levels = unique(vals) %>% sort()))
    if (type == "fct2") ids_fields[[f]] <- suppressWarnings(factor(vals, levels = fields[[f]]))
  }

  # detect failed conversions
  fails <- ids_fields %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~any(is.na(.x))
      )
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "field",
      values_to = "bad"
    ) %>%
    dplyr::filter(bad)
    if (nrow(fails)) cli::cli_abort(c(
      "x" = glue::glue("failed in converting: ", stringr::str_c(fails$field, collapse = ", ")),
      "i" = "confirm the parsing details provided or check id integrity"
    ))

  # bind new columns
  rep <- dplyr::bind_cols(
    rep,
    ids_fields %>% dplyr::select(-i)
  ) %>%
    dplyr::relocate(
      dplyr::all_of(names(fields)),
      .after = "serial"
    )

  # return
  rep
}

# >> ----
#' @rdname parse_id
#' @export
cctbon_id <- function(
    env
) {
  parse_id(
    env,
    standard = "cctbon"
  )
}

# >>>> ----
#' Add latlon coordinates or other info to an EnvLogger tibble
#'
#' @description
#' Joins latlon or other relevant information to an EnvLogger tibble resulting from a call to [read_env()]. Information is joined by location name, so, unless the logger ids used originally code for location name only, a call to [parse_id()] will be required to generate a column that specifically codes for location name. Those same location names have to be used here.
#'
#' @param env The output of a call to [read_env()] (either directly, of the `$report` element). Likely, a call to [parse_id()] will also be required to isolate the location name component from the logger ids.
#' @param info A data.frame (or tibble) providing location names, latlon coordinates, and any additional information to be joined to `env`. The column coding for location names must be strictly named `site`, and must be present. Columns coding for latitude and longitude, if included, must be strictly named `lat` and `lon`, not `long`, `longitude` or `x,y`. All additional columns are retained and joined (any column name matching to colnames in `env` will receive the suffix `x_`).
#'
#' @return
#' The same structure provided as input (`env`), augmented with new columns resulting from the joining of the information provided.
#'
#' @section Why use [add_info()] to set latlon:
#' Since EnvLogger data reports include latitude and longitude, it is reasonalbe to wonder why the [envlogger-package] doesn't collect latlon info from that source. In fact, the latitude and longitude provided in EnvLogger reports is there to potentially facilitate troubleshooting, in case the location of origin of a data report is uncertain due to failure to set meaningfull custom logger ids and similar issues. This is because there are many scenarious that can lead to the latlon data stored in the data report files being inaccurate or absent. For example, the smartphone used to download the data has just been turned on, a GPS fix may not yet have been available when the data was downloaded, or may be highly innacurate. The same happens if the loggers have being deployed in a location where GPS coverage is low or absent (underwater, inside of a cave, in an urban setting, etc.). On top of that, depending on the accuracy of the devices used during each download session (and whether there's only one device being used or multiple), potentially resulting in significant differences between the coordinates recorded on each file retrieved on the same day and from the same site.
#'
#' With all this in mind, sourcing latlon from the data report files would be too unreliable. On top of that, users typically maintain a registry of which loggers have been deployed and where, and that is a much more trustworthy and stable source of information for site geographic coordinates. It is in this context that [add_info()] is provided, as a way to streamline the merging of the logger data imported with the curated location information.
#'
#'
#' @export
#'
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' env <- cctbon_id(env)
#'
#' add_info(
#'    env,
#'    info = data.frame(
#'        var1 = 1:2,
#'        var3 = 1:2,
#'        site = c("ptzzy", "ptzzw"),
#'        var2 = 1:2,
#'        lat = c(36, 40),
#'        lon = -10,
#'        var4 = 1:2,
#'        exp = 1:2)
#'    )
# --- #
# env <- read_env(env_example(c("ptzzy", "ptzzw")), new_interval = 60, show_progress = FALSE, show_warnings = FALSE) %>% cctbon_id(); info = data.frame(var1 = 1:2, var3 = 1:2, site = c("ptzzy", "ptzzw"), var2 = 1:2, lat = c(36, 40), lon = -10, var4 = 1:2, exp = 1:2)
# add_info(env, info)
add_info <- function(
  env,
  info
) {
  rep <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # assess 'info' structure
  # is a data.frame
  if (!is.data.frame(info) & !tibble::is_tibble(info)) cli::cli_abort(c("x" = "'info' must be a data.frame"))
  cols <- colnames(info)
  # has site
  if ((!"site" %in% cols)) cli::cli_abort(c("x" = "couldn't find any column named 'site'"))
  # has lat and lon
  if ((!"lat" %in% cols) | (!"lon" %in% cols)) cli::cli_warn(c("!" = "couldn't find any columns named 'lat' and/or 'lon'"))

  # append "x_" suffix to all other columns that match a colname that exists in rep
  cols_mandatory <- c("site", "lat", "lon")
  other_matching <- intersect(
    colnames(rep),
    cols
  ) %>%
    setdiff(cols_mandatory)

  if (length(other_matching)) {
    pos <- which(colnames(info) %in% other_matching)
    colnames(info)[pos] <- stringr::str_c("x_", colnames(info)[pos])
  }

  # reorder columns --> site, lat, lon, all other in the order provided
  other_cols <- setdiff(colnames(info), cols_mandatory)
  info <- dplyr::relocate(info, dplyr::all_of(c(cols_mandatory, other_cols)))

  # join to rep
  rep <- rep %>%
    dplyr::left_join(info, by = "site") %>%
    dplyr::relocate(lat, lon, .before = type)

  # return
  rep
}

# >>>> ----
#' Group EnvLogger data by id elements and summarise it
#'
#' @description
#' Takes an EnvLogger tibble resulting from a call to [read_env()], groups data by id elements and summarises it. In the simplest case, grouping is only done along time (`by_day`). However, if logger ids have been broken into id elements with [parse_id()] or [cctbon_id()], grouping can also be done across id elements. For example, if data follows the CCTBON naming scheme, data can be grouped by `site`, `lvl` or `exp`. This function exists mainly to condition data for [plot_env()], but can also be useful in itself.
#'
#' @param env The output of a call to [read_env()] (either directly, of the `$report` element). Additionally, a subsequent call to [parse_id()] ensures more grouping options.
#' @param fun_list Named list with one or more functions to be applied to temperatures. Each function must return a single numeric value. The names of the columns derived from applying the functions provided are taken from the names of `fun_list`. If `fun` has no names, generic names are generated.
#' @param site The name of the column that codes for site. If a vector with more than one element, elements are combined to form a new column by pasting values together. Only relevant if `by_site = TRUE`. Can be omitted if data follows the CCTBON naming scheme.
#' @param group One or more column names to combine as a grouping factor. If a vector with more than one element, elements are combined to form a new column by pasting values together. Only relevant if `by_group = TRUE`. Can be omitted if data follows the CCTBON naming scheme.
#' @param by_day Whether to summarise values daily or at the native sampling interval.
#' @param by_site Whether to group by `site`.
#' @param by_group Whether to group by `group`.
#' @param roll_days Compute a rolling mean over the x days before each data point. If `0` (default), no rolling mean is applied.
#'
#' @return
#' The same structure provided as input (`env`), augmented with new columns resulting from the joining of the information provided.
#'
#'
#' @export
#'
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' env <- cctbon_id(env)
#'
#' add_info(
#'    env,
#'    info = data.frame(
#'        var1 = 1:2,
#'        var3 = 1:2,
#'        site = c("ptzzy", "ptzzw"),
#'        var2 = 1:2,
#'        lat = c(36, 40),
#'        lon = -10,
#'        var4 = 1:2,
#'        exp = 1:2)
#'    )
# --- #
# env <- read_env(env_example(c("ptzzy", "ptzzw")), new_interval = 60, show_progress = FALSE, show_warnings = FALSE) %>% cctbon_id(); fun_list = NULL; by_site = TRUE; by_group = TRUE; by_day = TRUE; roll_days = 2; site = "site"; group = c("lvl", "exp")
# summarise_env(env, by_day = by_day, by_site = by_site, by_group = by_group, roll_days = roll_days)
summarise_env <- function(
  env,
  fun_list  = NULL,
  site      = "site",
  group     = c("lvl", "exp"),
  by_day    = TRUE,
  by_site   = FALSE,
  by_group  = FALSE,
  roll_days = 0
) {
  by_day <- by_day | roll_days
  if (is.null(fun_list)) fun_list <- summarise_funs
  if (is.null(names(fun_list))) names(fun_list) <- stringr::str_c("fun", 1:length(fun_list))
  rep <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # if grouping by 'site', make sure colname(s) exists
  if (by_site) {
    if (!all(site %in% colnames(rep))) cli::cli_abort(c("x" = "failed to group by 'site'", "i" = "the value(s) provided couldn't be found in any colnames"))

    # collapse columns provided for 'site' and make into a factor
    rep$site <- rep %>%
      dplyr::select(dplyr::all_of(site)) %>%
      apply(1, function (x) stringr::str_c(x, collapse = "")) %>%
      factor()
  }

  # if grouping by 'group', make sure colname(s) exists
  if (by_group) {
    if (!all(group %in% colnames(rep))) cli::cli_abort(c("x" = "failed to group by 'group'", "i" = "the value(s) provided couldn't be found in any colnames"))

    # collapse columns provided for 'group'
    rep$group <- rep %>%
      dplyr::select(dplyr::all_of(group)) %>%
      apply(1, function (x) stringr::str_c(x, collapse = ""))

    # make into a factor
    levels_group <- rep$group %>% unique() %>% sort()
    rep <- dplyr::mutate(rep, group = factor(group, levels = levels_group))
  }

  new_cols <- setdiff(colnames(rep), env_cols)

  # begin grouping
  x <- tidyr::unnest(rep, cols = data)
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
    if (by_site) x <- dplyr::group_by(x, site, .add = TRUE)

    # finally, add group by group
    if (by_group) x <- dplyr::group_by(x, group, .add = TRUE)

    # if not grouping by site or by group, retain all new_cols
    if (!by_site & !by_group) x <- dplyr::group_by(x, id, serial, dplyr::across(dplyr::all_of(new_cols)), .add = TRUE)

    x <- x %>%
      dplyr::summarise(
        purrr::map(fun_list, ~.x(temp)) %>% tibble::as_tibble(),
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

    # return
    x <- dplyr::ungroup(x)
    x
  }
}

# Drone videos from Svalbard 2025 aboard the NG Endurance (Rui Seabra); submitted as 1080p due to file size, but all originals are 4K,60fps

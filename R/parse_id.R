# >>>> ----
#' Parse structured logger ids
#'
#' @description
#' If the logger ids used follow a stable naming scheme, [parse_id()] can be used to generate new columns that hold each naming segment. [parse_id_cctbon()] provides a quick shortcut for handling data that follows the CCTBON id scheme.
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
#' If the logger ids follow scheme used by the CCTBON network (<https://www.coastalwarming.com/cctbon>), simply use [parse_id_cctbon()] instead.
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
#' @seealso [read_env()], [add_info()], [summarise_env()]
#' @export
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' # If ids follow a supported id namind standard
#' parse_id(env, standard = "cctbon")
#' parse_id_cctbon(env) # even shorter for cctbon
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
# parse_id_cctbon(env)
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
parse_id_cctbon <- function(
    env
) {
  parse_id(
    env,
    standard = "cctbon"
  )
}


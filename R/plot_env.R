#' Plot EnvLogger data
#'
#' @description
#' Takes an EnvLogger tibble resulting from a call to [read_env()] and produces a plot based on the `ggplot2` package or the `dygraphs` package. Use `dygraphs` for an interactive plot, which is most useful when the focus is on inspecting data across different time periods. Use `ggplot2` for more options and customization, such as adding ribbons and faceting.
#'
#' @inheritParams summarise_env
#' @param var The name of the single column containing the variable to be used for plotting. Defaults to `temp`. Can also be set to `hum` or `press` if wanting to visualize humidity or pressure data, respectively. Can also be any other name, as long as it points to a column holding numeric data.
#' @param col_by The name of the single column containing the variable to be used for coloring each time series. Defaults to `sgmnt`, which results in the different segments of each id being shown with alternating colors (the data from each original data file corresponds to a segment, which remain identifiable even after joining by serial and id during import with [read_env()]). Coloring by segment facilitates the inspection of whether or not data has been joined together correctly). Other typical values for `col_by` include `site`, `lvl` and `exp`.
#' @param cols A named vector with the colors to be used. Defaults to `NULL`, in which case colors are determined automatically. Names must match the categories present in the column being used for coloring (define using `col_by`).
#' @param tz A number of hours to be added to timestamps (pos or neg). Please note that this doesn't provide true timezone support, as all timestamps are shifted by the same amount throughout the entire set of data.
#' @param same_days Should the plot only include data for days for which all distinct ids have data?
#' @param gg_ribbon (requires `dy = FALSE`) Whether to add a ribbon. If `TRUE`, `env` must include two columns strictly named `$ymin` and `$ymax`.
#' @param gg_ribbon_alpha Set the transparency of ribbons (`0` = fully transparent, `1` = fully opaque).
#' @param gg_facet_row,gg_facet_col (requires `dy = FALSE`) The name of the single column containing the variable to be used for distributing time series across different panels.
#' @param gg_cold_to_hot (requires `dy = FALSE`) Whether to color time series according to their mean `var` value (blue for lower values, red for higher values).
#' @param dy Whether to plot using `dygraphs`. Defaults to `FALSE`, resulting in a plot using `ggplot2`.
#' @param dy_focus (requires `dy = TRUE`) Either the names or the indexes pointing to one or more elements of the column `id`. Only ids referenced in `focus` will show in color, all other ids will be shown in black. If set to `all`, all ids are shown in color.
#' @param dy_pointSize (requires `dy = TRUE`) The size of points. Defaults to `NULL`, which cases for points not to be drawn. If provided, points are drawn to the size specified.
#'
#' @return
#' A ggplot or a dygraph.
#'
#' @seealso [read_env()], [parse_id()], [add_info()], [summarise_env()]
#'
#' @export
#'
#' @examples
#' ## DYGRAPHS
#' # interactivelly inspect segment joins with dygraphs
#' env <- read_env(
#'    env_example("ptzzy"),
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#' env <- parse_id_cctbon(env)
#'
#' # note how different segments of the same id are colored differently
#' # (segments = data from individual EnvLogger data report files)
#' # zoom to 2024-01-10 to 15 to see the the transition from ...
#' # ... segment 1 to segment 2 for id ptzzymc01a (from green to purple)
#' # use dy_focus to evidence a specific id (all other ids are plotted in black)
#' plot_env(env, dy = TRUE, dy_focus = "ptzzymc01a", dy_pointSize = 2)
#'
#' ## GGPLOT2
#' # more flexible plots with ggplot2
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw", "nozzz")), # 2 sites in Portugal and 1 in Norway
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' env <- parse_id_cctbon(env)
#'
#' # plot summarised data to make better sense of large datasets
#' # and use ribbons to depict ranges
#' # daily minimum, average and maximum for each site
#' daily_stats <- summarise_env(
#'    env,
#'    var = "temp",
#'    fun_list = list(
#'        ymin = min,
#'        avg = mean,
#'        ymax = max
#'        ),
#'    by_day = TRUE,
#'    by_site = TRUE
#'    )
#' plot_env(daily_stats, var = "avg", gg_ribbon = TRUE, col_by = "site")
#'
#' # use same_days to restrict plot to the common data range
#' # provide a custom set of colors
#' plot_env(
#'    daily_stats,
#'    var = "avg",
#'    gg_ribbon = TRUE,
#'    col_by = "site",
#'    same_days = TRUE,
#'    cols = c(nozzz = "blue", ptzzy = "black", ptzzw = "red"))
# -- #
# gg #
# env <- read_env(env_example(c("ptzzy", "ptzzw", "nozzz")), show_warnings = FALSE, show_progress = FALSE, new_interval = 60) %>% parse_id_cctbon() %>% summarise_env(by_site = TRUE, by_group = TRUE, fun_list = list(ymin = min, temp = mean, ymax = max), same_days = TRUE); env$site <- factor(as.character(env$site), levels = c("ptzzw", "ptzzy", "nozzz")); var = "temp"; col_by = "site"; cols = NULL; tz = 0; gg_ribbon = TRUE; gg_ribbon_alpha = 0.3; gg_facet_row = "site"; gg_facet_col = NULL; gg_cold_to_hot = TRUE; dy = FALSE; dy_focus = "all"; dy_pointSize = NULL
#
# dy #
# env <- read_env(env_example("ptzzy"), show_warnings = FALSE, show_progress = FALSE) %>% parse_id_cctbon(); var = "temp"; col_by = NULL; cols = NULL; tz = 0; gg_ribbon = FALSE; gg_ribbon_alpha = 0.3; gg_facet_row = NULL; gg_facet_col = NULL; gg_cold_to_hot = TRUE; dy = TRUE; dy_focus = "all"; dy_pointSize = 2
#
# plot_env(env, var = var, col_by = col_by, cols = cols, tz = tz, gg_ribbon = gg_ribbon, gg_ribbon_alpha = gg_ribbon_alpha, gg_facet_row = gg_facet_row, gg_facet_col = gg_facet_col, gg_cold_to_hot = gg_cold_to_hot, dy = dy, dy_focus = dy_focus, dy_pointSize = dy_pointSize)
plot_env <- function(
    env,
    var       = "temp",
    col_by    = NULL,
    cols      = NULL,
    tz        = 0,
    same_days = FALSE,
    ## gg
    gg_ribbon       = FALSE,
    gg_ribbon_alpha = 0.3,
    gg_facet_row    = NULL,
    gg_facet_col    = NULL,
    gg_cold_to_hot  = TRUE,
    ## dy
    dy           = FALSE,
    dy_focus     = "all",
    dy_pointSize = NULL
) {
  # force UTC so that xts doesn't throw warnings about TZ
  old_TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_TZ), add = TRUE)

  # load data (support env as a list or as a tibble)
  x <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # make sure that x is unnested
  if ("data" %in% colnames(x)) x <- x %>%
    dplyr::select(!dplyr::any_of(c("press", "hum"))) %>%
    tidyr::unnest(data)

  # sort by id and time
  x <- x %>% dplyr::arrange(id, t)

  # filter for days shared between all ids
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

  # adjust/check params
  ## dy ## enforce focus as character
  if (dy & is.numeric(dy_focus)) dy_focus <- unique(x$id)[[dy_focus]]
  ## var and col_by must point to valid colnames
  if (!var %in% colnames(x)) cli::cli_abort(c("x" = glue::glue(cli::col_magenta("var = '{var}'"), " not found")))
  if (is.null(col_by)) {
    col_by <- if (dy) "sgmnt" else "id"
  } else {
    if (!col_by %in% colnames(x)) cli::cli_abort(c("x" = glue::glue(cli::col_magenta("col_by = '{col_by}'"), " not found")))
  }
  ## if cols not provided and col_by = lvl, exp or lvl_exp
  if (is.null(cols) & col_by %in% names(plot_cols)) cols <- plot_cols[[col_by]]

  # gg ## gather relevant columns - facets
  if (!dy & !is.null(gg_facet_col)) x$facet_c <- dplyr::pull(x, gg_facet_col)
  if (!dy & !is.null(gg_facet_row)) x$facet_r <- dplyr::pull(x, gg_facet_row)

  # gg ## gather relevant columns - ribbon
  if (!dy & gg_ribbon) {
    # columns ymin and ymax must be present
    if (any(!c("ymin", "ymax") %in% colnames(x))) cli::cli_abort(c("x" = glue::glue("when ", cli::col_magenta(" = TRUE"), " columns ", cli::col_magenta("ymin"), " and ", cli::col_magenta("ymax"), " must be present")))
    x$ymin  <- dplyr::pull(x, ymin)
    x$ymax  <- dplyr::pull(x, ymax)
  }

  # dy ## if var is targeting hum or press, make sure temp is also plotted
  ## temp
  if (dy & var == "hum") {
    x <- dplyr::bind_rows(
      x %>% dplyr::mutate(id = stringr::str_c(id, "_h")),
      x %>% dplyr::mutate(hum = temp, id = stringr::str_c(id, "_t"))
    )
    if (dy_focus != "all") dy_focus <- c(
      stringr::str_c(dy_focus, "_h"),
      stringr::str_c(dy_focus, "_t"))
  }
  ## hum
  if (dy & var == "press") {
    x <- dplyr::bind_rows(
      x %>% dplyr::mutate(id = stringr::str_c(id, "_p")),
      x %>% dplyr::mutate(press = temp, id = stringr::str_c(id, "_t"))
    )
    if (dy_focus != "all") dy_focus <- c(
      stringr::str_c(dy_focus, "_p"),
      stringr::str_c(dy_focus, "_t"))
  }

  # tidy x (remove unwanted columns, rename target var)
  x <- dplyr::bind_cols(
    x %>%
      dplyr::select(
        id, dplyr::any_of(c("sgmnt", "facet_c", "facet_r", "ymin", "ymax")), t
      ),
    x %>%
      dplyr::select(
        var = dplyr::matches(var),
        col = dplyr::matches(col_by)
      )
  )

  # offset by tz
  x <- dplyr::mutate(x, t = t + tz * 3600)

  if (dy) {
    ## dygraphs ##
    # rearrange data
    ## if col_by = sgmnt, convert to 1,2,1,2
    x <- x %>%
      dplyr::arrange(id, t) %>%
      dplyr::mutate(
        col = if (col_by == "sgmnt") {
          col %>%
            factor() %>%
            as.numeric() %>%
            magrittr::mod(2) %>%
            magrittr::subtract(1, .) %>%
            magrittr::add(1) %>%
            as.character()
        } else {
          col
        },
        id_original = id,
        id = if (col_by == "id") id else stringr::str_c(id, "_", col)
      )

    if (any(stringr::str_count(x$id, "_") > 1)) {
      # when ids end up like "ptzzy_mh_mh", strip one of _mh
      id_elements     <- x$id %>% stringr::str_split("_")
      n_elements      <- purrr::map_dbl(id_elements, length) %>% max()
      id_last_minus_0 <- purrr::map_chr(id_elements, n_elements - 0)
      id_last_minus_1 <- purrr::map_chr(id_elements, n_elements - 1)
      # last and last-1 are identical, so remove one of the repeated instances
      if (identical(id_last_minus_0, id_last_minus_1)) {
        x <- x %>%
          dplyr::mutate(
            id = stringr::str_remove(id, stringr::str_c("_", id_last_minus_0))
          )
      }
    }

    # define colors
    default_col <- "grey80"
    cols_xts <- x %>%
      dplyr::select(id, id_original, col) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        focus = if (any(dy_focus == "all")) TRUE else (id_original %in% dy_focus)
      )

    if (is.null(cols)) {
      cols <- if (col_by == "sgmnt") {
        c("1" = "green", "2" = "purple")
      } else {
        col_focus <- cols_xts %>% dplyr::filter(dy_focus)
        tmp <- grDevices::hcl.colors(n = sum(col_focus$focus), palette = "Zissou 1")
        names(tmp) <- col_focus$col
        tmp
      }
    }

    cols_xts <- cols_xts %>%
      dplyr::mutate(
        col_val = cols[col] %>% tidyr::replace_na(default_col),
        col_val = dplyr::if_else(focus, col_val, default_col),
        col_val = unname(col_val),
        stroke  = dplyr::if_else(col_val == default_col, 0.5, 2)
      )

    if (all(cols_xts$focus)) cols_xts$stroke <- 1

    # convert to xts
    x <- x %>%
      dplyr::select(id, t, var) %>%
      tidyr::pivot_wider(
        names_from  = id,
        values_from = var
      )
    x <- xts::xts(
      x = dplyr::select(x, -t),
      order.by = dplyr::pull(x, t) + tz * 3600,
      tzone = "UTC"
    )

    # build plot
    p_dy <- dygraphs::dygraph(x) %>%
      dygraphs::dyRangeSelector()

    n_focus <- cols_xts %>% dplyr::filter(!focus)
    if (nrow(n_focus)) {
      p_dy <- p_dy %>%
        dygraphs::dyGroup(
          name  = n_focus$id,
          color = n_focus$col_val,
          strokeWidth = n_focus$stroke
        )
    }

    y_focus <- cols_xts %>% dplyr::filter(focus)
    if (nrow(y_focus)) {
      p_dy <- p_dy %>%
        dygraphs::dyGroup(
          name  = y_focus$id,
          color = y_focus$col_val,
          strokeWidth = y_focus$stroke
        )
    }

    p_dy <- p_dy %>%
      dygraphs::dyOptions(
        connectSeparatedPoints = TRUE,
        drawPoints = !is.null(dy_pointSize),
        pointSize = dy_pointSize,
        stepPlot = FALSE
      ) %>%
      dygraphs::dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.5,
        hideOnMouseOut = FALSE
      ) %>%
      dygraphs::dyLegend(
        show = "always",
        labelsSeparateLines = TRUE,
        hideOnMouseOut = FALSE
      )

    # plot
    p_dy

  } else {
    ## ggplot2 ##

    # ensure that colder groups get bluer colors when cold_to_hot = TRUE
    if (gg_cold_to_hot) {
      ORDER <- x %>%
        dplyr::group_by(col) %>%
        dplyr::summarise(avg = mean(var)) %>%
        dplyr::arrange(avg) # from coldest to hottest
      x$col <- factor(as.character(x$col), levels = ORDER$col)
    }

    # if not specified, get the right colors
    if (is.null(cols)) {
      cols_n  <- dplyr::n_distinct(x$col)
      cols_nm <- unique(sort(x$col))

      cols <- if (cols_n == 1) {
        ## black if there's only one color category
        tmp <- "black"
        names(tmp) <- cols_nm
        tmp
      } else if (col_by %in% names(plot_cols)) {
        ## if col_by = lvl, exp or lvl_exp
        plot_cols[[col_by]]
      } else {
        ## otherwise, cols will be a color ramp
        tmp <- grDevices::hcl.colors(n = cols_n, palette = "Zissou 1")
        names(tmp) <- cols_nm
        tmp
      }
    }

    # build plot
    p_gg <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x     = t,
        y     = var,
        group = id,
        col   = col
      )) +
      ggplot2::xlab("") +
      ggplot2::ylab("")

    # add ribbon
    if (gg_ribbon) p_gg <- p_gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = t,
          ymin = ymin,
          ymax = ymax,
          group = id,
          fill = col
        ),
        col = NA,
        alpha = gg_ribbon_alpha)

    p_gg <- p_gg + ggplot2::geom_line()

    # apply facets
    if (!is.null(gg_facet_col))
      p_gg <- p_gg + ggplot2::facet_grid(cols = ggplot2::vars(facet_c))
    if (!is.null(gg_facet_row))
      p_gg <- p_gg + ggplot2::facet_grid(rows = ggplot2::vars(facet_r))
    if (!is.null(gg_facet_col) & !is.null(gg_facet_row))
      p_gg <- p_gg + ggplot2::facet_grid(rows = ggplot2::vars(facet_r), cols = ggplot2::vars(facet_c))

    # apply scales and colors
    p_gg <- p_gg +
      ggplot2::guides(color = ggplot2::guide_legend(col_by), fill = "none") +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::scale_fill_manual( values = cols)

    p_gg <- p_gg + ggplot2::theme_bw()

    # plot
    p_gg
  }
}

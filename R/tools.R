# >> ----
# t1 = read_env_all(env_example("ptzzw")$rep[[1]], read_data = TRUE)$rep$data[[1]]$t; t2 = read_env_all(env_example("gap")$rep[[1]], read_data = TRUE)$rep$data[[1]]$t; buffer_secs = 10
# t_check(t1, buffer_secs); t_check(t2, buffer_secs)
t_check <- function(t, buffer_secs = 10) {
  if (tibble::is_tibble(t)) t <- t$t
  d <- diff(t)
  units(d) <- "secs"

  d <- as.numeric(d)
  d_med <- median(d)
  d_var <- abs(d_med - d)

  negs <- which(d < 0)
  gaps <- which(d_var > buffer_secs)
  if (length(negs)) { # not unidirectional?
    stringr::str_c("time_gap neg @ ", t[negs[[1]]] %>% as.character())
  } else if (length(gaps)) { # not constant and without gaps?
    stringr::str_c("time_gap @ ", t[gaps[[1]]] %>% as.character())
  } else { # ok!
    " "
  }
}


# >> ----
# append_issues(paths = "path", step = "step", issue = "issue", new_vals = list(id = "id"))
append_issues <- function(
    paths = NULL,
    step,
    issue,
    new_vals = NULL,
    df       = NULL
) {
  if (is.null(df)) {
    df <- tibble::tibble(
      step      = "delete",
      path_data = "delete",
      path_meta = "delete",
      issue     = "delete",
      new_vals  = list(list(delete = "delete")),
      run2      = TRUE,
      fixed     = FALSE
    ) %>%
      dplyr::slice(-1)
  }

  if (tibble::is_tibble(paths)) paths <- paths %>% dplyr::pull(path)
  if (length(paths)) {
    df <- dplyr::bind_rows(
      df,
      tibble::tibble(
        step      = step,
        path_data = paths,
        path_meta = if (is.null(new_vals)) "" else metadata_create_fn(paths),
        issue     = issue,
        new_vals  = new_vals,
        run2      = TRUE,
        fixed     = FALSE
      )
    )
  }
  df
}


# >> ----
# msg = c("v" = stringr::str_c(cli::col_green("bef"), "ore")); bullet = "x"; vec = stringr::str_c("af", cli::col_red("ter")); col = "yellow"
# bullets(msg, bullet, vec, col) %>% cli::cli_bullets()
bullets <- function(msg, bullet = " ", vec, col = NULL) {
  COLS <- c("red", "blue", "yellow", "green", "white", "black", "cyan", "grey", "magenta", "sliver", "white", "none")
  if (!is.null(col)) if (col %in% COLS) vec <- eval(parse(text = stringr::str_c("cli::col_", col)))(vec)
  names(vec) <- rep_len(bullet, length(vec))
  msg <- c(msg, vec)
  msg
}

# >> ----
# paths = env_example("issue")$rep; paths = stringr::str_remove(paths, fs::path_common(paths)); issues = as.character(seq_along(paths)^2); path_width = max(stringr::str_length(paths)) + 3
# bullets_path_issues(paths, issues, path_width) %>% cli::cli_bullets()
bullets_path_issues <- function(paths, issues, fn_width) {
  paths  <- paths %>%
    stringr::str_c(" ") %>%
    cli::col_blue() %>%
    stringr::str_pad(
      width = fn_width + 10,
      side = "right",
      pad = ".",
      use_width = FALSE)

  issues <- purrr::map_vec(issues, ~stringr::str_c(cli::col_red(stringr::str_split_1(.x, ",")), collapse = ","))

  stringr::str_c(paths, " (", issues, ")")
}


# >> ----
# dat = NULL; fun = function(x) {list(rep = mean(x), msg_bullets = "ok")}; section_nm = "test"; use_dat = FALSE; x = 1:10
# run_section(dat = dat, use_dat = use_dat, fun = fun, section_nm = section_nm, x = x)
run_section <- function(
    dat = NULL,
    use_dat = TRUE,
    fun,
    section_nm = "",
    show_progress = TRUE,
    ...
) {
  dat <- if (use_dat) fun(dat, ...) else fun(...)
  if (show_progress) dat$msg_bullets %>% dplyr::last() %>% cli::cli_bullets()
  dat
}



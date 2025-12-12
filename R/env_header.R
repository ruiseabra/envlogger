# >> ----
# header <- env_example("improper_file_name")$rep[[1]]; header <- env_example("ptzzw")$rep[[1]]; header <- header %>% readr::read_csv(progress = FALSE, n_max = 21, col_names = c("field", "val"), show_col_types = FALSE); path = "test"
# fix_header(header, path)
fix_header <- function(header, path) {
  header <- header %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_to_lower))

  # fix fields that were named differently in earlier versions
  header$field[header$field == "time diff (sec)"] <- "time diff [logger-smartphone](sec)"
  header$field[header$field == "time diff [logger-smartphone](s)"] <- "time diff [logger-smartphone](sec)"

  # fix fields that were missing in earlier versions
  f <- "custom name"
  if (!any(header$field == f)) header <- tibble::add_row(header, field = f, val = "")
  f <- "file name"
  if (!any(header$field == f)) header <- tibble::add_row(header, field = f, val = path)

  fields <- c("device", "device name")
  for (f in fields) {
    if (!any(header$field == f))
      header <- tibble::add_row(header, field = f, val = "missing")
  }

  # if curstom name is empty, use serial instead
  nm <- env_header_val(header, "custom name")$chr %>% stringr::str_trim()
  if (is.na(nm) | nm == "") {
    header$val[header$field == "custom name"] <- env_header_val(header, "serial number")$chr

  }

  # fix values that were formatted improperly in earlier versions
  if (is.na(env_header_val(header, "envlogger viewer version")$dbl)) {
    header$val[header$field == "envlogger viewer version"] <- 0
  }

  # return
  header
}


# >> ----
# headers <- env_example("ptzzw")$rep[[1]] %>% readr::read_csv(progress = FALSE, n_max = 21, col_names = c("field", "val"), show_col_types = FALSE); field_pattern = "device"
# env_header_val(headers, field_pattern)
env_header_val <- function(
    headers,
    field_pattern
) {
  if (tibble::is_tibble(headers)) headers <- list(headers)
  vals <- tibble::tibble(
    chr = headers %>%
      purrr::map_chr(~.x %>%
                       dplyr::filter(field == field_pattern) %>%
                       dplyr::pull(val)),
    l   = purrr::map_dbl(chr, length),
    dbl = purrr::map_dbl(chr, ~suppressWarnings(readr::parse_number(.x))),
    txt = purrr::map_chr(chr, ~suppressWarnings(stringr::str_remove_all(.x, "[-0-9.]"))) %>% stringr::str_trim()
  )
  if (any(vals$l != 1)) cli::cli_abort(c("x" = "'field_pattern' must match no more and no less than 1 time"))
  # return
  dplyr::select(vals, chr, dbl, txt)
}

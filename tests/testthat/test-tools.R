### t_check

test_that("t_check() accepts env tibble and env$t", {
  # tibble with EnvLogger data
  env <- env_read(env_example("ptzzw")$rep[[1]], read_data = TRUE)$rep$data[[1]]

  # test
  expect_no_error(t_check(env))
  expect_no_error(t_check(env$t))
})

test_that("t_check() returns expected value when timestamps are ok", {
  # tibble with EnvLogger data
  env <- env_read(env_example("ptzzw")$rep[[1]], read_data = TRUE)$rep$data[[1]]

  # test
  expect_identical(t_check(env), " ")
})

test_that("t_check() returns expected issue elements when timestamps are not ok", {
  # tibble with EnvLogger data
  env <- env_read(env_example("_gap")$rep[[1]], read_data = TRUE)$rep$data[[1]]

  # test
  expect_identical(t_check(env), "time_gap @ 2023-05-04 03:00:00")
})

test_that("t_check() only identifies gaps shorter than buffer_secs", {
  # tibble with EnvLogger data
  env <- env_read(env_example("_gap")$rep[[1]], read_data = TRUE)$rep$data[[1]]

  # test
  expect_identical(t_check(env, buffer_secs = 3600 * 24 * 120), "time_gap @ 2023-05-04 03:00:00")
  expect_identical(t_check(env, buffer_secs = 3600 * 24 * 180), " ")
})


### append_issues

test_that("append_issues() returns tibble", {
  # tibble with issue details
  out <- append_issues(
    paths = "path",
    step  = "step",
    issue = "issue"
    )

  # test
  expect_type(out, "list")
  expect_true(tibble::is_tibble(out))
})

test_that("append_issues() returns tibble with expected names and col types", {
  # dput(colnames(append_issues(paths = "path", step = "step", issue = "delete")))
  cols <- c("step", "path_data", "path_meta", "issue", "new_vals", "run2", "fixed")
  # dput(out %>% dplyr::summarise(dplyr::across(dplyr::everything(), typeof)) %>% as.character())
  col_types <- c("character", "character", "character", "character", "list", "logical", "logical")
  # append_issues() output
  out <- append_issues(
    paths = "path",
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"))

  # test colnames
  expect_named(out, cols)
  # test col types
  expect_equal(
    out %>% dplyr::summarise(dplyr::across(dplyr::everything(), typeof)) %>% as.character(),
    col_types
  )
})

test_that("append_issues() returns empty tibble if no paths provided", {
  # append_issues() output
  out <- append_issues(
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"))

  # test
  expect_type(out, "list")
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 0)
})

test_that("append_issues() returns tibble with same nrow as length(paths)", {
  # several paths
  paths <- letters[1:3]
  # append_issues() output
  out <- append_issues(
    paths = paths,
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"))

  # test
  expect_equal(nrow(out), length(paths))
})

test_that("append_issues() outputs appended tibble if df is supplied", {
  # several paths
  paths1 <- rep("a", 3)
  paths2 <- rep("b", 4)
  # first tibble
  out1 <- append_issues(
    paths = paths1,
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"))
  # appended tibble
  out2 <- append_issues(
    paths = paths2,
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"),
    df    = out1)

  # test
  expect_equal(nrow(out2), nrow(out1) + length(paths2))
  expect_equal(out2$path_data, c(paths1, paths2))
})

test_that("append_issues() correctly generates metadata file paths", {
  # several paths
  paths <- letters[1:3]
  # corresponding metadata paths
  meta_paths <- metadata_create_fn(paths)
  # append_issues() output
  out <- append_issues(
    paths = paths,
    step  = "step",
    issue = "issue",
    new_vals = list(id = "id"))

  # test
  expect_equal(out$path_meta, meta_paths)
})


### bullets

test_that("bullets() handles lack of param values", {
  # test
  expect_no_error(bullets())
  expect_equal(bullets(), c(" " = " "))
})

test_that("bullets() accepts valid colors", {
  # assign red color
  out1 <- bullets(col = "red") %>% unname()
  # generate cli red prefix-suffix
  out2 <- cli::col_red(" ") %>% as.character()

  # test
  expect_equal(out1, out2)
})

test_that("bullets() ignores invalid colors", {
  # test
  expect_no_error(bullets(col = "invalid_color"))
  expect_equal(bullets(), c(" " = " "))
})

test_that("bullets() sets names correctly", {
  # test
  expect_equal(bullets(bullet = "x"), c("x" = " "))
})

test_that("bullets() concatenates multiple messages", {
  # concatenate two messages with bullets
  out1 <- bullets(vec = "1", bullet = "x") %>%
    bullets(vec = "2", bullet = ">") %>%
    bullets(vec = "3", bullet = "v")
  # generate expected output (dput(out1))
  out2 <- c(x = "1", `>` = "2", v = "3")

  # test
  expect_equal(out1, out2)
})

test_that("bullets() doesn't accept vec with length gt 1", {
  # test
  expect_error(bullets(vec = c("a", "b")))
})


### bullets_path_issues
#
# bullets_path_issues("a", "b", fn_width = 15) %>% cli::cli_bullets()
# \033[34m\033[39m
# 1234567890123456789
# a ........ (b)12345
# a ............. (b)
# # >> ----
# # paths = env_example("issue")$rep; paths = stringr::str_remove(paths, fs::path_common(paths)); issues = as.character(seq_along(paths)^2); path_width = max(stringr::str_length(paths)) + 3
# # bullets_path_issues(paths, issues, path_width) %>% cli::cli_bullets()
# bullets_path_issues <- function(paths, issues, fn_width) {
#   nchar_blue <- cli::col_blue("") %>%
#     as.character() %>%
#     stringr::str_replace_all("\\", "X")
#     nchar()
#
#   paths  <- paths %>%
#     stringr::str_c(" ") %>%
#     cli::col_blue() %>%
#     stringr::str_pad(
#       width = fn_width + 10,
#       side = "right",
#       pad = ".",
#       use_width = FALSE)
#
#   issues <- purrr::map_vec(issues, ~stringr::str_c(cli::col_red(stringr::str_split_1(.x, ",")), collapse = ","))
#
#   stringr::str_c(paths, " (", issues, ")")
# }
#

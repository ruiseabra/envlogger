test_that("env_ls() fails with invalid paths", {
  # doesn't point to any file
  paths1 <- NULL
  # length 0
  paths2 <- c()
  # points to non-existing path
  paths3 <- ""

  # test
  expect_error(env_ls()) # paths must be provided
  expect_error(env_ls(paths1))
  expect_error(env_ls(paths2))
  expect_error(env_ls(paths3))
})

test_that("env_ls() accepts directories and paths with length > 1", {
  # points to one valid directory
  paths1 <- env_example(just_dir = TRUE)[1]
  # points to several valid directories
  paths2 <- env_example(just_dir = TRUE)

  # test
  expect_no_error(env_ls(paths1))
  expect_no_error(env_ls(paths2))
})

test_that("env_ls() accepts env_example() lists", {
  # unchanged env_example() list
  paths1 <- env_example()
  # env_example() list with one list element name changed
  paths2 <- paths1
  names(paths2)[1] <- "aaa"

  #test
  expect_no_error(env_ls(paths1))
  expect_error(env_ls(paths2))
})

test_that("env_ls() finds files inside folders recursively", {
  # common path for all env_example() files
  path <- env_example(just_dir = TRUE, delete_new_metadata_files = TRUE) %>% fs::path_common()
  # files listed recursively using fs::dir_ls()
  paths1 <- path %>%
    fs::dir_ls(recurse = TRUE, type = "file") %>%
    sort() %>%
    as.character() %>%
    stringr::str_to_lower() # needed to match env_ls() output
  # files listed using env_ls()
  paths2 <- path %>%
    env_ls(list_unsupported = TRUE) %>%
    dplyr::pull(path) %>%
    sort()

  # test
  expect_identical(paths1, paths2)
})

test_that("env_ls() avoids multiple patterns", {
  # patterns to avoid
  patterns <- c("humid", "press")
  # files listed avoiding target patterns
  paths <- env_ls(paths = env_example(), avoid_pattern = patterns)$path

  # test
  expect_no_match(paths, patterns[1])
  expect_no_match(paths, patterns[2])
})

test_that("env_ls() lists unsupported files", {
  # with unsupported
  paths1 <- env_ls(paths = env_example(), list_unsupported = TRUE )$path
  # without unsupported
  unsupported <- env_example()$unsupported
  paths2 <- env_ls(paths = env_example(), list_unsupported = FALSE)$path

  # test
  expect_equal(paths1, c(unsupported, paths2))
})

test_that("env_ls() outputs tibble", {
  # using env_example() list
  out1 <- env_ls(env_example())
  # using directories
  out2 <- env_ls(env_example(just_dir = TRUE))
  # using file paths
  out3 <- env_ls(env_example()$report)

  # test
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true(tibble::is_tibble(out3))
})

test_that("env_ls() outputs paths in lowercase", {
  # common path for all env_example() files
  path <- env_example(just_dir = TRUE, delete_new_metadata_files = TRUE) %>% fs::path_common()
  # use fs::dir_ls() to retain uppercase characters
  paths1 <- path %>%
    fs::dir_ls(recurse = TRUE, type = "file") %>%
    sort() %>%
    as.character()
  # use env_ls()
  paths2 <- path %>%
    env_ls(list_unsupported = TRUE) %>%
    dplyr::pull(path) %>%
    sort()

  # confirm that paths1 contains uppercase characters
  expect_true(stringr::str_detect(paths1, "A") %>% any())

  # test
  expect_equal(stringr::str_to_lower(paths1), paths2)
})

test_that("env_ls() outputs expected colnames, col types and col values", {
  # dput(colnames(env_ls(env_example())))
  cols <- c("path", "int", "chr", "rep", "rep_env", "rep_other", "has_met", "log", "met")
  # env_ls() output
  out <- env_ls(env_example(delete_new_metadata_files = TRUE), list_unsupported = TRUE)

  # test colnames
  expect_named(out, cols)
  # test col types
  expect_type(out$int, "double")
  expect_type(out$chr, "character")
  out %>%
    dplyr::select(-path, -int, -chr) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), typeof)) %>%
    as.character() %>%
    magrittr::equals("logical") %>%
    all() %>%
    expect_true()
  # test col values
  expect_in(out$int, 0:3)
  expect_in(out$chr, c("report", "log", "metadata", "unsupported"))
})


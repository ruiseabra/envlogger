test_that("env_example() handles NULL pattern", {
  # test
  expect_no_error(env_example())
  expect_no_error(env_example(pattern = NULL))
  expect_no_error(env_example(pattern = c()))
  expect_no_error(env_example(pattern = ""))
})

test_that("env_example() outputs list in all but one instance", {
  # test
  expect_type(env_example(), "list")
  expect_type(env_example(pattern = ""), "list")
  expect_type(env_example(delete_new_metadata_files = TRUE), "list")
})

test_that("env_example() outputs character vector when just_dir = TRUE", {
  # test
  expect_type(env_example(just_dir = TRUE), "character") # vector of paths to directories
})

test_that("env_example() accepts multiple patterns", {
  # patterns to check
  patterns <- c("humid", "press")
  # just with patterns
  paths1 <- env_example(patterns) %>% purrr::list_c()
  # all available
  paths2 <- env_example() %>% purrr::list_c()

  # confirm that paths1 is a smaller subset
  expect_gt(length(paths2), length(paths1))

  # test
  expect_true(
    all(
      stringr::str_detect(paths1, patterns[1]) |
      stringr::str_detect(paths1, patterns[2])
      )
  )
})

test_that("env_example() can return just directories", {
  # using env_example()
  paths1 <- env_example(just_dir = TRUE) %>% sort()
  # using fs::path_dir()
  paths2 <- env_example() %>%
    purrr::list_c() %>%
    fs::path_dir() %>%
    stringr::str_to_lower() %>%
    unique() %>%
    sort()

  # test
  expect_equal(paths1, paths2)
})

test_that("env_example() can cleanup files generated in examples folders", {
  # create new metadata file in example folders
  new_file <- metadata_create_file(env_example("humidsc01a")$report, update = TRUE)
  # with the newly generated file
  paths1 <- env_example() %>% purrr::list_c()
  # after cleanup
  paths2 <- env_example(delete_new_metadata_files = TRUE) %>% purrr::list_c()

  # test
  expect_true( new_file %in% paths1)
  expect_false(new_file %in% paths2)
})

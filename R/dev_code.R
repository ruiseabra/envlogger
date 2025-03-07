if (FALSE) {
  ### declare files to be ignored during build
  use_build_ignore("dev_code.R")
  # use_build_ignore("data-raw")
  # use_git_ignore("dev_code.R")

  ### declare intent to use GITHUB
  use_git()

  ### set package licence
  use_mit_license()

  ### declare use of RMARKDOWN for help files
  use_roxygen_md()

  ### generate a readme file for GITHUB
  use_readme_rmd()

  ### update the readme
  devtools::build_readme()

  ### import specific functions
  use_import_from(package = "magrittr", fun = "%>%")

  ### declare dependencies
  use_package("cli")
  use_package("dplyr")
  use_package("ggplot2")
  use_package("lubridate")
  use_package("magrittr")
  use_package("purrr")
  use_package("readr")
  use_package("stringr")
  use_package("tibble")
  use_package("tidyr")
  use_package("zoo")
  use_package("xts")
  use_package("dygraphs")

  ### create and edit a new R file
  use_r("xxx")

  ### GENERATE EXAMPLE DATA
  # run code in "data-raw/envlogger.R"
}

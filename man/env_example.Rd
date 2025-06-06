% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{env_example}
\alias{env_example}
\title{Get paths to example files}
\usage{
env_example(patterns = NULL, no_logs = FALSE, dir = FALSE)
}
\arguments{
\item{patterns}{character vectors, defaults to \code{NULL}; patterns to select one or more example files. \code{patterns} is vectorized, so more than one value can be supplied. If set to \code{NULL}, all example files are returned.}

\item{no_logs}{logical, defaults to \code{FALSE}; if \code{TRUE}, EnvLogger log files are omitted.}

\item{dir}{logical, defaults to \code{FALSE}; if \code{TRUE}, only the shore folder paths are returned.}
}
\value{
The full path to one or more example files, or the full paths to all example files available.
}
\description{
\code{envlogger-package} comes bundled with several sample files in its inst/extdata directory. These data consist of EnvLogger files covering several years of data from two sites in northern Portugal. This function make them easy to access for use in examples.
}
\examples{
# Get the file paths of all example files
env_example()

# Get the full paths to a target serial
env_example("04CB_CC00_1507_0C")

# Get the full paths to any example files matching a search string
env_example("20250114")

# Get only log files
env_example("/log_")

# Get no log files
env_example(no_logs = TRUE)

# Get only folder paths
env_example(dir = TRUE)

# 'env_example()' is vectorized, meaning that multiple search strings can be used at once
env_example(c("ptzzy", "nozzz"))
}

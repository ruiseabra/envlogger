% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{env_join_id}
\alias{env_join_id}
\title{Join EnvLogger data corresponding to the same ID}
\usage{
env_join_id(dat)
}
\arguments{
\item{dat}{a tibble with a collection of outputs from running \code{\link[=read_env_all]{read_env_all()}} on multiple paths corresponding to EnvLogger reports, but filtered to include only data with (or without) pressure data, but not both.}
}
\value{
The same tibble, but now with readings every freq_mins
}
\description{
Colate EnvLogger data whenever ID is shared (same and different serials)
!NOTE!: currently, overlaps are being handled in a very crude manner (averaging temperature for overlaping timestamps); in reality, overlaps shouldn't even happen, because data should be trimmed in the files themselves to exclude anything before and after deployment, as doing it through code is impossible (it must be done on a case by case approach); for now, overlaps are averaged out and a warning is issued.
}
\examples{
paths <- env_example(c("04E3_EE00_3852_03", "04CC_9700_010C_10"))
dat  <- purrr::map_dfr(paths, read_env_all)
env_join_id(dat)
}
\seealso{
\code{\link[=read_env_all]{read_env_all()}}
}

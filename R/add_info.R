# >>>> ----
#' Add latlon coordinates or other info to an EnvLogger tibble
#'
#' @description
#' Joins latlon or other relevant information to an EnvLogger tibble resulting from a call to [read_env()]. Information is joined by location name, so, unless the logger ids used originally code for location name only, a call to [parse_id()] will be required to generate a column that specifically codes for location name. Those same location names have to be used here.
#'
#' @param env The output of a call to [read_env()] (either directly, of the `$report` element). Likely, a call to [parse_id()] will also be required to isolate the location name component from the logger ids.
#' @param info A data.frame (or tibble) providing location names, latlon coordinates, and any additional information to be joined to `env`. The column coding for location names must be strictly named `site`, and must be present. Columns coding for latitude and longitude, if included, must be strictly named `lat` and `lon`, not `long`, `longitude` or `x,y`. All additional columns are retained and joined (any column name matching to colnames in `env` will receive the suffix `x_`).
#'
#' @return
#' The same structure provided as input (`env`), augmented with new columns resulting from the joining of the information provided.
#'
#' @section Why use [add_info()] to set latlon:
#' Since EnvLogger data reports include latitude and longitude, it is reasonable to wonder why the [envlogger-package] doesn't collect latlon info from that source. In fact, the latitude and longitude provided in EnvLogger reports is there to potentially facilitate troubleshooting, in case the location of origin of a data report is uncertain due to failure to set meaningful custom logger ids and similar issues. This is because there are many scenarios that can lead to the latlon data stored in the data report files being inaccurate or absent. For example, the smartphone used to download the data has just been turned on, a GPS fix may not yet have been available when the data was downloaded, or may be highly inaccurate. The same happens if the loggers have being deployed in a location where GPS coverage is low or absent (underwater, inside of a cave, in an urban setting, etc.). On top of that, depending on the accuracy of the devices used during each download session (and whether there's only one device being used or multiple), potentially resulting in significant differences between the coordinates recorded on each file retrieved on the same day and from the same site.
#'
#' With all this in mind, sourcing latlon from the data report files would be too unreliable. On top of that, users typically maintain a registry of which loggers have been deployed and where, and that is a much more trustworthy and stable source of information for site geographic coordinates. It is in this context that [add_info()] is provided, as a way to streamline the merging of the logger data imported with the curated location information.
#'
#' @seealso [read_env()], [parse_id()], [summarise_env()]
#' @export
#'
#'
#' @examples
#' env <- read_env(
#'    env_example(c("ptzzy", "ptzzw")),
#'    new_interval = 60,
#'    show_progress = FALSE,
#'    show_warnings = FALSE)
#'
#' env <- parse_id_cctbon(env)
#'
#' add_info(
#'    env,
#'    info = data.frame(
#'        var1 = 1:2,
#'        var3 = 1:2,
#'        site = c("ptzzy", "ptzzw"),
#'        var2 = 1:2,
#'        lat = c(36, 40),
#'        lon = -10,
#'        var4 = 1:2,
#'        exp = 1:2)
#'    )
# --- #
# env <- read_env(env_example(c("ptzzy", "ptzzw")), new_interval = 60, show_progress = FALSE, show_warnings = FALSE) %>% parse_id_cctbon(); info = data.frame(var1 = 1:2, var3 = 1:2, site = c("ptzzy", "ptzzw"), var2 = 1:2, lat = c(36, 40), lon = -10, var4 = 1:2, exp = 1:2)
# add_info(env, info)
add_info <- function(
  env,
  info
) {
  rep <- if (is.list(env) & !tibble::is_tibble(env)) env$report else env

  # assess 'info' structure
  # is a data.frame
  if (!is.data.frame(info) & !tibble::is_tibble(info)) cli::cli_abort(c("x" = "'info' must be a data.frame"))
  cols <- colnames(info)
  # has site
  if ((!"site" %in% cols)) cli::cli_abort(c("x" = "couldn't find any column named 'site'"))
  # has lat and lon
  if ((!"lat" %in% cols) | (!"lon" %in% cols)) cli::cli_warn(c("!" = "couldn't find any columns named 'lat' and/or 'lon'"))

  # append "x_" suffix to all other columns that match a colname that exists in rep
  cols_mandatory <- c("site", "lat", "lon")
  other_matching <- intersect(
    colnames(rep),
    cols
  ) %>%
    setdiff(cols_mandatory)

  if (length(other_matching)) {
    pos <- which(colnames(info) %in% other_matching)
    colnames(info)[pos] <- stringr::str_c("x_", colnames(info)[pos])
  }

  # reorder columns --> site, lat, lon, all other in the order provided
  other_cols <- setdiff(colnames(info), cols_mandatory)
  info <- dplyr::relocate(info, dplyr::all_of(c(cols_mandatory, other_cols)))

  # join to rep
  rep <- rep %>%
    dplyr::left_join(info, by = "site") %>%
    dplyr::relocate(lat, lon, .before = type)

  # return
  rep
}


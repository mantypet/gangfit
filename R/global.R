library(tidyverse)
library(janitor)
library(XML)
library(sf)
library(slider)

#' Read a GPX file
#'
#' @param gpx_file A GPX file
#' @return A data frame
#'
#' @examples
#' read_gpx(here::here((gpx_file = "data/20230617_challenge.gpx"))
#'
read_gpx <- function(gpx_file) {
  gpx <- htmlTreeParse(file = gpx_file, useInternalNodes = TRUE)
  coords <- xpathSApply(gpx, path = "//trkpt", xmlAttrs)
  elev   <- xpathSApply(gpx, path = "//trkpt/ele", xmlValue)
  ts_chr <- xpathSApply(gpx, path = "//trkpt/time", xmlValue)
  hr <- xpathSApply(gpx, path = "//trkpt/extensions", xmlValue)
  dat_df <- data.frame(
    datetime = ymd_hms(ts_chr, tz = "Europe/Helsinki", quiet = TRUE),
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elev),
    hr = as.numeric(hr)
  )
  return(dat_df)
}

#' Normalize a vector to [0, 1]
#'
#' @param x A numeric vector
#' @return A numeric vector
#'
#' @examples
#' normalize(c(1, 2, 3))
normalize <- function(x) {
  if (any(is.na(x)) || any(!is.numeric(x))) stop("Input must be numeric and not contain NA") # nolint
  (x - min(x)) / (max(x) - min(x))
}

#' Wrap double digits with slashes
#'
#' @param x A character vector
#' @return A character vector
#'
#' @examples
#' mark_double_digits(c("1_2", "3_4", "5_6"))
mark_double_digits <- function(x) {
  x <- stringr::str_replace_all(x, "(?<=_\\d{2})", "/")
  x <- stringr::str_replace_all(x, "_", "/")
  x
}

read_generalstrength <- function(csv_file = here::here("data", "20230803_generalstrength.csv")) {
  # read in 20230515_generalstrength.csv
  generalstrength <- read_csv(csv_file, ) %>%
    clean_names() %>%
    mutate(chinup_reps = mark_double_digits(chinup_reps))
  
  generalstrength
}

todo_function <- function() {

  # separate *_reps to multiple columns
  generalstrength_ <- generalstrength %>%
    separate_wider_regex(cols = squat_reps, pattern = c(squat_reps_1 = "\\d", "", squat_reps_2 = "\\d", "", squat_reps_3 = "\\d")) %>%
    # copy separate_wider_regex but with press_reps and bench_reps
    separate_wider_regex(cols = press_reps, pattern = c(press_reps_1 = "\\d", "", press_reps_2 = "\\d", "", press_reps_3 = "\\d")) %>%
    separate_wider_regex(cols = bench_reps, pattern = c(bench_reps_1 = "\\d", "", bench_reps_2 = "\\d", "", bench_reps_3 = "\\d")) %>%
    # again copy separate_wider_regex but with power_clean_reps and to 5 columns
    separate_wider_regex(cols = power_clean_reps, pattern = c(power_clean_reps_1 = "\\d", "", power_clean_reps_2 = "\\d", "", power_clean_reps_3 = "\\d", "", power_clean_reps_4 = "\\d", "", power_clean_reps_5 = "\\d"))
}

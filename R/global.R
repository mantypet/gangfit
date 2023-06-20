library(tidyverse)
library(janitor)

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

todo_function <- function() {
  # read in 20230515_generalstrength.csv
  generalstrength <- read_csv(here::here("data", "20230515_generalstrength.csv")) %>%
    clean_names() %>%
    mutate(chinup_reps = mark_double_digits( chinup_reps))
  
  
  # separate squat_reps to multiple columns
  generalstrength_ <- generalstrength %>%
    separate_wider_regex(cols = squat_reps, pattern = c(squat_reps_1 = "\\d", "", squat_reps_2 = "\\d", "", squat_reps_3 = "\\d")) %>%
    # copy separate_wider_regex but with press_reps and bench_reps
    separate_wider_regex(cols = press_reps, pattern = c(press_reps_1 = "\\d", "", press_reps_2 = "\\d", "", press_reps_3 = "\\d")) %>% 
    separate_wider_regex(cols = bench_reps, pattern = c(bench_reps_1 = "\\d", "", bench_reps_2 = "\\d", "", bench_reps_3 = "\\d")) %>%
    # again copy separate_wider_regex but with power_clean_reps and to 5 columns
    separate_wider_regex(cols = power_clean_reps, pattern = c(power_clean_reps_1 = "\\d", "", power_clean_reps_2 = "\\d", "", power_clean_reps_3 = "\\d", "", power_clean_reps_4 = "\\d", "", power_clean_reps_5 = "\\d"))
}

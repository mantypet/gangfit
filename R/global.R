library(tidyverse)

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

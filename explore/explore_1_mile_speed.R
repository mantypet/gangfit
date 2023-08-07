library(tidyverse)

extrapolate_run_distance <- function(time, distance_from_m, distance_to_m, type = c("seconds", "minutes")) {
  type <- match.arg(type)
  time <- switch (type,
                  "seconds" = time,
                  "minutes" = lubridate::ms(time, roll = TRUE)
  )
  
  out_time <- as.integer(time)/(distance_from_m/distance_to_m)
  
  switch (type,
          "seconds" = out_time,
          "minutes" = lubridate::ms(glue::glue("00:{out_time}"), roll = TRUE)
  )
}

times <- c("1:21", "1:23", "1:20", "1:21", "1:18")
extrapolate_run_distance(times, 360, 1600, type = "minutes")

max_hr <- c(151,161,163,163,167)

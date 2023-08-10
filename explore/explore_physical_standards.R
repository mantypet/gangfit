source(here::here("R/global.R"))

# General strength (1RM 2 x BW)
dl <- 172.5 / 92
# Usable upperbody strength (20 reps)
pu <- 12
# Speed (13 s)
sprint_100m <- 14.5
# Endurance (360 s)
run_1mile <- 445
# Health (BF 10-12 %)
bf <- 10.2

# Examble data
phys <- structure(list(type = c("Novice", "PR", "Gang Fit", "Advanced"),
                       gen_str = c(0, dl, 2, 2.5),
                       upper_body_str = c(5, pu, 20, 25),
                       speed = c(15, sprint_100m, 13, 12),
                       endurance = c(468, run_1mile, 360, 346),
                       health = c(18, bf, 10, 6)),
                  row.names = c(NA, -4L),
                  class = "data.frame")

library(ggradar)

phys_std <- phys %>%
  mutate(across(where(~is.numeric(.x)), ~normalize(.x))) %>%
  mutate(speed = 1 - speed,
         endurance = 1 - endurance,
         health = 1 - health)

ggradar(phys_std)

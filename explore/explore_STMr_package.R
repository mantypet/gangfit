library(tidyverse)
# devtools::install_github("mladenjovanovic/STMr")
library(STMr)

source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20230807_generalstrength.csv")) %>%
  mutate(date_index = as.integer(date) - min(as.integer(date)))

squat.rep <- generalstrength %>%
  filter(squat_sets_completed == 3) %>%
  select(date, date_index, starts_with("squat")) %>%
  mutate(squat_reps = 5,
         perc_1rm = adj_perc_1RM_RIR(squat_reps, max_perc_1RM_func = max_perc_1RM_linear, adjustment = 1),
         weight_1rm = squat_weight/perc_1rm,
         weight_medium = weight_1rm*0.7)

dl.rep <- generalstrength %>%
  filter(deadlift_sets_completed == 1) %>%
  select(date, date_index, starts_with("deadlift")) %>%
  mutate(deadlift_reps = 5,
         perc_1rm = adj_perc_1RM_RIR(deadlift_reps, max_perc_1RM_func = max_perc_1RM_linear, adjustment = 1),
         weight_1rm = deadlift_weight/perc_1rm,
         weight_medium = weight_1rm*0.7)


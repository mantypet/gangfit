source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20240314_generalstrength.csv"))

vahvinressu <- generalstrength %>%
  select(date, starts_with("squat"), starts_with("deadlift"), starts_with("press"), notes) %>%
  filter(date >= "2024-01-01")

vahvinressu.rep <- vahvinressu %>%
  mutate(week_monday = lubridate::floor_date(date, "week"),
         week_sunday = lubridate::ceiling_date(date, "week"),
         squat_weight_lag = lag(squat_weight),
         deadlift_weight_lag = lag(deadlift_weight),
         press_weight_lag = lag(press_weight),
         squat_weight_diff = squat_weight-lag(squat_weight),
         deadlift_weight_diff = deadlift_weight-lag(deadlift_weight),
         press_weight_diff = press_weight-lag(press_weight))

vahvinressu.week_n <- vahvinressu.rep %>%
  group_by(week_monday) %>%
  summarise(squat_weight_start = squat_weight_lag[date == min(date)],
            squat_weight_increment = sum(squat_weight_diff),
            squat_weight_increment_rel = (squat_weight_start+squat_weight_increment)/squat_weight_start,
            deadlift_weight_start = deadlift_weight_lag[date == min(date)],
            deadlift_weight_increment = sum(deadlift_weight_diff),
            deadlift_weight_increment_rel = (deadlift_weight_start+deadlift_weight_increment)/deadlift_weight_start,
            press_weight_start = press_weight_lag[date == min(date)],
            press_weight_increment = sum(press_weight_diff),
            press_weight_increment_rel = (press_weight_start+press_weight_increment)/press_weight_start)

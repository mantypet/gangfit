source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20240314_generalstrength.csv"))

vahvinressu <- generalstrength %>%
  select(date, starts_with("squat"), starts_with("deadlift"), starts_with("press"), notes) %>%
  filter(date >= "2024-01-01")

vahvinressu.rep <- vahvinressu %>%
  mutate(week_monday = lubridate::floor_date(date, "week", week_start = 1),
         week_sunday = lubridate::ceiling_date(date, "week", week_start = 1),
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

### Lopulliset syklit


cycle <- data.frame(week = seq.Date(from = as.Date("2024-02-05"), to = as.Date("2024-08-12"), by = "week")) %>%
  mutate(index = 1:length(week),
         week_num = index+5) %>%
  mutate(cycle = case_when(index %in% 1:8 ~ 1,
                           index %in% 9:18 ~ 2,
                           index %in% 19:28 ~ 3),
         deload_multip = ifelse(index %in% c(3,8,13,18,23,28), 0.85, 1),
         multip = (1.05-(log(index)/100))^(index),
         squat_weight = 105*multip*deload_multip,
         deadlift_weight = 120*multip*deload_multip,
         press_weight = 42*multip*deload_multip) %>%
  mutate(squat_weight_plates = floor(squat_weight / 2.5)*2.5,
         deadlift_weight_plates = floor(deadlift_weight / 2.5)*2.5,
         press_weight_plates = floor(press_weight / 1.25)*1.25)

plot(cycle$index, cycle$squat_weight_plates)
plot(cycle$index, cycle$deadlift_weight_plates)
plot(cycle$index, cycle$press_weight_plates)

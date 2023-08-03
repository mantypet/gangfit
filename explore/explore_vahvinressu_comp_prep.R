source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20230803_generalstrength.csv"))

vahvinressu <- generalstrength %>%
  select(date, starts_with("squat"), starts_with("deadlift"), notes)

end_date_2022 <- as.Date("2022-08-20")
start_date_2022 <- end_date_2022 - 91
end_date_2023 <- as.Date("2023-08-19")
start_date_2023 <- end_date_2023 - 91

vahvinressu22 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2022)) %>%
  filter(date >= start_date_2022 & date <= end_date_2022) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                  TRUE ~ deadlift_weight))

vahvinressu23 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2023)) %>%
  filter(date >= start_date_2023 & date <= end_date_2023) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                     TRUE ~ deadlift_weight))

ggplot() +
  geom_point(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = squat_weight), color = "lightgrey") + 
  geom_point(data = vahvinressu23, aes(x = t, y = squat_weight), color = "darkgreen") +
  geom_smooth(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = squat_weight), color = "lightgrey") + 
  geom_smooth(data = vahvinressu23, aes(x = t, y = squat_weight), color = "darkgreen") +
  xlim(-91, 0) +
  theme_minimal()

ggplot() +
  geom_point(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey") + 
  geom_point(data = vahvinressu23, aes(x = t, y = deadlift_weight), color = "darkgreen") +
  geom_smooth(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey") + 
  geom_smooth(data = vahvinressu23, aes(x = t, y = deadlift_weight), color = "darkgreen") +
  xlim(-91, 0) +
  theme_minimal()

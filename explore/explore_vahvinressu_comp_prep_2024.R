source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20240314_generalstrength.csv"))

vahvinressu <- generalstrength %>%
  select(date, starts_with("squat"), starts_with("deadlift"), notes)

time_to_event <- 190

end_date_2022 <- as.Date("2022-08-20")
start_date_2022 <- end_date_2022 - time_to_event
end_date_2023 <- as.Date("2023-08-19")
start_date_2023 <- end_date_2023 - time_to_event
end_date_2024 <- as.Date("2024-08-17")
start_date_2024 <- end_date_2024 - time_to_event

vahvinressu22 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2022),
         t_pos = t + time_to_event) %>%
  filter(date >= start_date_2022 & date <= end_date_2022) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                  TRUE ~ deadlift_weight))

vahvinressu23 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2023),
         t_pos = t + time_to_event) %>%
  filter(date >= start_date_2023 & date <= end_date_2023) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                     TRUE ~ deadlift_weight))

vahvinressu24 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2024),
         t_pos = t + time_to_event) %>%
  filter(date >= start_date_2024 & date <= end_date_2024) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                     TRUE ~ deadlift_weight))

ggplot() +
  geom_point(data = vahvinressu22, aes(x = t, y = squat_weight), color = "lightgrey") + 
  geom_point(data = vahvinressu23, aes(x = t, y = squat_weight), color = "grey") +
  geom_point(data = vahvinressu24, aes(x = t, y = squat_weight), color = "darkgreen") +
  geom_smooth(data = vahvinressu22, aes(x = t, y = squat_weight), color = "lightgrey",
              method = "loess", fullrange = TRUE, se = FALSE) + 
  geom_smooth(data = vahvinressu23, aes(x = t, y = squat_weight), color = "grey",
              method = "loess", fullrange = TRUE, se = FALSE) +
  geom_smooth(data = vahvinressu24, aes(x = t, y = squat_weight), color = "darkgreen",
              method = "loess", fullrange = TRUE, se = FALSE) +
  xlim(-time_to_event, 30) +
  theme_minimal()

ggplot() +
  geom_point(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey") + 
  geom_point(data = slice_head(vahvinressu23, n = -2), aes(x = t, y = deadlift_weight), color = "grey") +
  geom_point(data = vahvinressu24, aes(x = t, y = deadlift_weight), color = "darkgreen") +
  stat_smooth(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey",
              method = "loess", fullrange = TRUE, se = FALSE) + 
  stat_smooth(data = slice_head(vahvinressu23, n = -2), aes(x = t, y = deadlift_weight), color = "grey",
              method = "loess", fullrange = TRUE, se = FALSE) +
  stat_smooth(data = vahvinressu24, aes(x = t, y = deadlift_weight), color = "darkgreen",
              method = "loess", fullrange = TRUE, se = FALSE) +
  xlim(-time_to_event, 30) +
  theme_minimal()

mdl <- glm(formula = deadlift_weight ~ t_pos,  family = Gamma(link="log"), data = vahvinressu23)

summary(mdl, dispersion = 1) 

newdata <- data.frame("t_pos" = c(110, 112, 117))
predict(mdl, newdata, type = "response")

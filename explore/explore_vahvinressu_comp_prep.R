source(here::here("R/global.R")) 

generalstrength <- read_generalstrength(csv_file = here::here("data", "20230803_generalstrength.csv"))

vahvinressu <- generalstrength %>%
  select(date, starts_with("squat"), starts_with("deadlift"), notes)

# Add rows manually, export broken?
extra <- data.frame(
  "date" = as.Date(c("2023-08-04", "2023-08-07")),
  "squat_weight" = c(137.5, 140),
  "squat_sets_completed" = c(3,3),
  "squat_reps" = c("555", "555"),
  "deadlift_weight" = c(NA, 172.5),
  "deadlift_sets_completed" = c(NA, 1),
  "deadlift_reps" = c(NA, 5),
  "notes"   = c("Notes", "Notes")
)
vahvinressu <- vahvinressu %>%
  bind_rows(extra)

time_to_event <- 120

end_date_2022 <- as.Date("2022-08-20")
start_date_2022 <- end_date_2022 - time_to_event
end_date_2023 <- as.Date("2023-08-19")
start_date_2023 <- end_date_2023 - time_to_event

vahvinressu22 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2022),
         t_pos = t + 120) %>%
  filter(date >= start_date_2022 & date <= end_date_2022) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                  TRUE ~ deadlift_weight))

vahvinressu23 <- vahvinressu %>%
  mutate(t = as.numeric(date - end_date_2023),
         t_pos = t + 120) %>%
  filter(date >= start_date_2023 & date <= end_date_2023) %>%
  mutate(squat_weight = case_when(squat_reps != "555" | squat_sets_completed != 3 ~ NA,
                                  TRUE ~ squat_weight),
         deadlift_weight = case_when(deadlift_reps != 5 | deadlift_sets_completed != 1 ~ NA,
                                     TRUE ~ deadlift_weight))

ggplot() +
  geom_point(data = vahvinressu22, aes(x = t, y = squat_weight), color = "lightgrey") + 
  geom_point(data = vahvinressu23, aes(x = t, y = squat_weight), color = "darkgreen") +
  geom_smooth(data = vahvinressu22, aes(x = t, y = squat_weight), color = "lightgrey",
              method = "loess", fullrange = TRUE, se = FALSE) + 
  geom_smooth(data = vahvinressu23, aes(x = t, y = squat_weight), color = "darkgreen",
              method = "loess", fullrange = TRUE, se = FALSE) +
  xlim(-time_to_event, 30) +
  theme_minimal()

ggplot() +
  geom_point(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey") + 
  geom_point(data = vahvinressu23, aes(x = t, y = deadlift_weight), color = "darkgreen") +
  stat_smooth(data = slice_head(vahvinressu22, n = -1), aes(x = t, y = deadlift_weight), color = "lightgrey",
              method = "loess", fullrange = TRUE, se = FALSE) + 
  stat_smooth(data = vahvinressu23, aes(x = t, y = deadlift_weight), color = "darkgreen",
              method = "loess", fullrange = TRUE, se = FALSE) +
  xlim(-time_to_event, 30) +
  theme_minimal()

mdl <- glm(formula = deadlift_weight ~ t_pos,  family = Gamma(link="log"), data = vahvinressu23)

summary(mdl, dispersion = 1) 

newdata <- data.frame("t_pos" = c(110, 112, 117))
predict(mdl, newdata, type = "response")

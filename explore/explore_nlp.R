source(here::here("R/global.R"))

ss <- read_generalstrength(csv_file = here::here("data/20240129_generalstrength.csv"))

periods <- data.frame(period_start = as.Date(c("2018-11-22", "2019-04-13", "2019-12-30", "2020-06-12", "2022-06-01", "2023-01-06", "2023-03-03", "2024-01-18")),
                      period_end = as.Date(c("2019-03-19", "2019-11-14", "2020-02-19", "2020-10-06", "2022-08-22", "2023-02-27", "2023-08-07", "2024-08-20"))) %>%
  mutate(period = 1) %>%
  mutate(period = cumsum(period))

ss.rep <- ss %>%
  select(date, squat_weight, squat_reps, deadlift_weight, deadlift_reps) %>%
  left_join(periods, by = join_by(between(date, period_start, period_end))) %>%
  filter(!is.na(period)) %>%
  mutate(week_monday = lubridate::floor_date(date, unit = "week", week_start = 1))

ss.rep <- ss.rep %>%
  mutate(session_id = 1:nrow(ss.rep))

ss.rep.diff.long <- ss.rep %>%
  select(-ends_with("_reps")) %>%
  pivot_longer(cols = c(squat_weight, deadlift_weight), names_to = "exercise", values_to = "weight") %>%
  mutate(exercise = str_remove(string = exercise, pattern = "_weight")) %>%
  filter(!is.na(weight)) %>%
  group_by(exercise) %>%
  mutate(lag_date = lag(date),
         diff_date = as.integer(date-lag_date),
         lag_weight = lag(weight),
         diff_weight = weight-lag_weight)

# missed training

ggplot(ss.rep.diff.long %>% filter(diff_date < 60 & diff_weight < 26 & diff_weight > -26)) +
  geom_smooth(aes(x = diff_date, y = diff_weight, color = exercise), method = "loess") +
  geom_point(aes(x = diff_date, y = diff_weight, color = exercise)) +
  # scale_x_continuous(breaks = 2*0:10) +
  # scale_y_continuous(breaks = 5*10:40) +
  theme_minimal()



# period - session analysis
ss.rep.week <- ss.rep %>%
  mutate(squat_weight = replace_na(squat_weight, 0),
         deadlift_weight = replace_na(deadlift_weight, 0)) %>%
  group_by(week_monday, period) %>%
  summarise(squat_weight = max(squat_weight, na.rm = T),
            deadlift_weight = max(deadlift_weight, na.rm = T),
            .groups = "drop") %>%
  mutate(session = 1) %>%
  group_by(period) %>%
  mutate(session = cumsum(session)) %>%
  pivot_longer(cols = c(squat_weight, deadlift_weight), names_to = "exercise", values_to = "weight") %>%
  mutate(weight = ifelse(weight == 0, NA, weight))

nlp.model_data <- ss.rep.week %>%
  mutate(weight = ifelse(weight <= 60, NA, weight)) %>%
  filter(period != 5 & session != 11)

ggplot(nlp.model_data) +
  geom_line(aes(x = session, y = weight, color = factor(period))) +
  scale_x_continuous(breaks = 2*0:10) +
  theme_minimal() +
  facet_wrap(~exercise)

ggplot(nlp.model_data) +
  geom_smooth(aes(x = session, y = weight, color = exercise)) +
  geom_point(aes(x = session, y = weight, color = exercise)) +
  scale_x_continuous(breaks = 2*0:10) +
  scale_y_continuous(breaks = 5*10:40) +
  theme_minimal()

ggplot(filter(nlp.model_data, session %in% 1:4)) +
  geom_smooth(aes(x = session, y = weight, color = exercise), method = "glm") +
  geom_point(aes(x = session, y = weight, color = exercise)) +
  scale_x_continuous(breaks = 2*0:10) +
  scale_y_continuous(breaks = 5*10:40) +
  theme_minimal()

ggplot(filter(nlp.model_data, session %in% 5:10)) +
  geom_smooth(aes(x = session, y = weight, color = exercise), method = "glm") +
  geom_point(aes(x = session, y = weight, color = exercise)) +
  scale_x_continuous(breaks = 2*0:10) +
  scale_y_continuous(breaks = 5*10:40) +
  theme_minimal()

ggplot(filter(nlp.model_data, session > 10)) +
  geom_smooth(aes(x = session, y = weight, color = exercise), method = "glm") +
  geom_point(aes(x = session, y = weight, color = exercise)) +
  scale_x_continuous(breaks = 2*0:10) +
  scale_y_continuous(breaks = 5*10:40) +
  theme_minimal()

nlp.model_data.dl <- nlp.model_data %>%
  filter(exercise == "deadlift_weight")

lm(weight ~ period + session, data = nlp.model_data.dl)


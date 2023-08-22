library(tidyverse)

aika_as_numeric <- function(x) {
  ms(x, roll = TRUE) %>%
    seconds() %>%
    as.numeric()
}

# ajoneuvon veto

ajon21 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2021.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2021")
ajon22 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2022.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2022")
ajon23 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2023.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2023")

ajon <- bind_rows(ajon21,ajon22,ajon23)

ajon.rep <- ajon %>%
  mutate(Aika_s = aika_as_numeric(Aika))

ajon.rep %>%
  filter(Aika_s < 100) %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = Aika_s, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = Aika_s, group = Nimi, color = Sarja), linewidth = 0.7) +
  theme_minimal()


# tykin nosto

tykki21 <- read_csv(here::here("data/vahres23/tykin_lavetin_nosto_2021.csv")) %>%
  mutate(vuosi = "2021")
tykki22 <- read_csv(here::here("data/vahres23/tykin_lavetin_nosto_2022.csv")) %>%
  mutate(vuosi = "2022")
tykki23 <- read_csv(here::here("data/vahres23/tykin_lavetin_nosto_2023.csv")) %>%
  mutate(vuosi = "2023")

tykki <- bind_rows(tykki21,tykki22,tykki23)

tykki.rep <- tykki %>%
  rename(massa = `Lavetin massa`) %>%
  mutate(massa_num = as.numeric(str_remove(massa, "kg")),
         toistot = `Toistoa/min`,
         volyymi = toistot*massa_num)

tykki.rep %>%
  ggplot() +
  geom_boxplot(aes(x = vuosi, y = toistot)) +
  geom_line(aes(x = vuosi, y = mean(toistot, na.rm = TRUE)))

tykki.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = volyymi, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = volyymi, group = Nimi, color = Sarja), linewidth = 0.7) +
  theme_minimal()

tykki.rep %>%
  filter(Sarja %in% c("-75kg", "+75kg")) %>%
  ggplot() +
  geom_smooth(aes(x = vuosi, y = toistot, group = Sarja, color = Sarja), method = "lm") +
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 2.2) +
  theme_minimal()

tykki.rep %>%
  filter(Sarja %in% c("-80kg", "-100kg", "+100kg")) %>%
  ggplot() +
  geom_smooth(aes(x = vuosi, y = toistot, group = Sarja, color = Sarja), method = "lm", se = FALSE) +
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 2.2) +
  theme_minimal()

tykki.summary <- tykki.rep %>%
  group_by(vuosi, Sarja) %>%
  summarise(mean_toistot = mean(toistot, na.rm = T),
            sd_toistot = sd(toistot, na.rm = T),
            n = n(),
            sqrt_n = sqrt(n),
            mean_volyymi = mean(volyymi, na.rm = T),
            sd_volyymi = sd(volyymi, na.rm = T)) %>%
  mutate(upr_toistot = mean_toistot + 1.96*(sd_toistot/sqrt_n),
         lwr_toistot = mean_toistot - 1.96*(sd_toistot/sqrt_n))

tykki.summary %>%
  filter(Sarja %in% c("-80kg", "-100kg", "+100kg")) %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = mean_toistot, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = mean_toistot, group = Sarja, color = Sarja), linewidth = 0.7) +
  geom_ribbon(aes(x = vuosi, ymin = lwr_toistot, ymax = upr_toistot, group = Sarja), alpha = 0.1) +
  theme_minimal()

tykki.rep_m <- tykki.rep %>%
  filter(Sarja %in% c("-80kg", "-100kg", "+100kg"))

fit_tykki <- lm(toistot ~ vuosi + Sarja, data = tykki.rep_m)

summary(fit_tykki)


library(tidyverse)

aika_as_numeric <- function(x) {
  ms(x, roll = TRUE) %>%
    seconds() %>%
    as.numeric()
}

palette_vahres_3 <- c("#c7d9bd","#8da043","#437338")

palette_vahres_5 <- colorRampPalette(c(palette_vahres[1], palette_vahres[3]))( 5 )

# ajoneuvon veto

ajon21 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2021.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2021")
ajon22 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2022.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2022")
ajon23 <- read_csv(here::here("data/vahres23/ajoneuvon_veto_2023.csv"), col_types = "iccccccii") %>%
  mutate(vuosi = "2023")

ajon <- bind_rows(ajon21,ajon22,ajon23)

ajon.rep <- ajon %>%
  mutate(Aika_s = aika_as_numeric(Aika),
         Sarja = factor(Sarja, levels = c("-75kg","+75kg","-80kg","-100kg","+100kg")))

ajon.rep %>%
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
         volyymi = toistot*massa_num) %>%
  mutate(Sarja = factor(Sarja, levels = c("-75kg","+75kg","-80kg","-100kg","+100kg")))

tykki.rep %>%
  ggplot() +
  geom_boxplot(aes(x = vuosi, y = toistot)) +
  geom_line(aes(x = vuosi, y = mean(toistot, na.rm = TRUE)))

tykki.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = toistot, group = Nimi, color = Sarja), linewidth = 0.7) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()

tykki.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = volyymi, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = volyymi, group = Nimi, color = Sarja), linewidth = 0.7) +
  scale_color_manual(values = palette_vahres_5) +
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
         lwr_toistot = mean_toistot - 1.96*(sd_toistot/sqrt_n),
         upr_volyymi = mean_volyymi + 1.96*(sd_volyymi/sqrt_n),
         lwr_volyymi = mean_volyymi - 1.96*(sd_volyymi/sqrt_n))

tykki.summary %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = mean_toistot, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = mean_toistot, group = Sarja, color = Sarja), linewidth = 0.7) +
  geom_ribbon(aes(x = vuosi, ymin = lwr_toistot, ymax = upr_toistot, group = Sarja), alpha = 0.1) +
  theme_minimal()

tykki.summary %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = mean_volyymi, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = mean_volyymi, group = Sarja, color = Sarja), linewidth = 0.7) +
  geom_ribbon(aes(x = vuosi, ymin = lwr_volyymi, ymax = upr_volyymi, group = Sarja), alpha = 0.1) +
  theme_minimal()

##########

tykki.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = toistot, group = Nimi, color = Sarja), linewidth = 0.7) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()


tykki.rep_m <- tykki.rep %>%
  #filter(Sarja %in% c("-80kg", "-100kg", "+100kg")) %>%
  mutate(vuosi_num = as.numeric(vuosi),
         vuosi_center = vuosi_num-mean(vuosi_num),
         vuosi_intercept = vuosi_num-min(vuosi_num))

fit_tykki <- lm(toistot ~ vuosi_center + Sarja, data = tykki.rep_m)

summary(fit_tykki)

# taistelijan pyörä

taist22 <- read_csv(here::here("data/vahres23/taistelijan_pyora_2022.csv")) %>%
  mutate(vuosi = "2022")
taist23 <- read_csv(here::here("data/vahres23/taistelijan_pyora_2023.csv")) %>%
  mutate(vuosi = "2023")

taist <- bind_rows(taist22,taist23)


taist.rep <- taist %>%
  rename(massa = `Kannettava massa`) %>%
  mutate(massa_num = as.numeric(str_remove(massa, "kg")),
         matka = `Metriä`,
         painotettu_matka = matka*massa_num) %>%
  mutate(Sarja = factor(Sarja, levels = c("-75kg","+75kg","-80kg","-100kg","+100kg")))

taist.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = matka, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = matka, group = Nimi, color = Sarja), linewidth = 0.7) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()

#https://zief0002.github.io/gentrified-ladybug/notes/s22-17-lmer-average-change-over-time.html

library(lme4)
library(broom.mixed)
# Fit model
lmer.0 = lmer(matka ~ 1 + (1 | Nimi), data = taist.rep, REML = FALSE)

# Coefficient-level output
tidy(lmer.0, effects = "fixed")

# Obtain random effects
tidy(lmer.0, effects = "ran_vals")

# Obtain student-specific coefficients
tidy(lmer.0, effects = "ran_coefs")

# eg. Petteri's results

# Obtain variance estimates
tidy(lmer.0, effects = "ran_pars")

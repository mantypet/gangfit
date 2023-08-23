source(here::here("R/global.R"))

vahres <- read_vahres()

palette_vahres_5 <- c("#424756", "#A65228", "#e1d798","#8da043", "#437338")

# ajoneuvon veto

ajon.rep <- vahres$ajoneuvon_veto %>%
  mutate(Aika_s = aika_as_numeric(Aika),
         Sarja = factor(Sarja, levels = c("-75kg","+75kg","-80kg","-100kg","+100kg")))

ajon.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = Aika_s, color = Sarja), size = 3) +
  geom_line(aes(x = vuosi, y = Aika_s, group = Nimi, color = Sarja), linewidth = 1) +
  scale_color_manual(values = palette_vahres_5) +
  scale_y_continuous(trans='log10') +
  theme_minimal()


# tykin nosto




tykki.rep <- vahres$tykin_lavetin_nosto %>%
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
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 3) +
  geom_line(aes(x = vuosi, y = toistot, group = Nimi, color = Sarja), linewidth = 1) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()


tykki.rep_m <- tykki.rep %>%
  #filter(Sarja %in% c("-80kg", "-100kg", "+100kg")) %>%
  mutate(vuosi_num = as.numeric(vuosi),
         vuosi_center = vuosi_num-mean(vuosi_num),
         vuosi_intercept = vuosi_num-min(vuosi_num),
         sarja_num = str_remove(Sarja,"kg"))

fit_tykki <- lm(toistot ~ vuosi_center + Sarja, data = tykki.rep_m)

summary(fit_tykki)

# taistelijan pyörä




taist.rep <- vahres$taistelijan_pyora %>%
  rename(massa = `Kannettava massa`) %>%
  mutate(massa_num = as.numeric(str_remove(massa, "kg")),
         matka = `Metriä`,
         painotettu_matka = matka*massa_num,
         vuosi_intercept = as.numeric(vuosi) - 2022) %>%
  mutate(Sarja = factor(Sarja, levels = c("-75kg","+75kg","-80kg","-100kg","+100kg")),
         sarja_num = abs(as.numeric(str_remove(Sarja, "kg"))+10))

taist.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = matka, color = Sarja), size = 2.2) +
  geom_line(aes(x = vuosi, y = matka, group = Nimi, color = Sarja), linewidth = 0.7) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()

#https://zief0002.github.io/gentrified-ladybug/notes/s22-17-lmer-average-change-over-time.html

# Load libraries
library(AICcmodavg)
library(broom.mixed) #for tidy, glance, and augment functions for lme4 models
library(corrr)
#library(educate)
library(lme4) #for fitting mixed-effects models
library(patchwork)
library(texreg)

# Fit model
lmer.0 = lmer(matka ~ 1 + (1 | Nimi), data = taist.rep, REML = FALSE)

# Coefficient-level output
tidy(lmer.0, effects = "fixed")

# Obtain random effects
tidy(lmer.0, effects = "ran_vals")

# Obtain contender-specific coefficients
tidy(lmer.0, effects = "ran_coefs")

# Obtain variance estimates
sd_est <- tidy(lmer.0, effects = "ran_pars")
sd_est

# Total unexplained variance
sum(tidy(lmer.0, effects = "ran_pars")$estimate^2)

# Proportion of unexplained variation that is between-subjects
sd_est$estimate[1]^2/sum(sd_est$estimate^2)

# Proportion of unexplained variation that is within-subjects
sd_est$estimate[2]^2/sum(sd_est$estimate^2)

# Fit unconditional growth model (within subject)
lmer.1.ws = lmer(matka ~ 1 + vuosi_intercept + (1|Nimi), data = taist.rep, REML = FALSE)

# Coefficient-level output
tidy(lmer.1.ws, effects = "fixed")

# SD estimates
tidy(lmer.1.ws, effects = "ran_pars")

# Obtain variance estimates
sd_est <- tidy(lmer.1.ws, effects = "ran_pars")
sd_est

# Total unexplained variance
sum(tidy(lmer.1.ws, effects = "ran_pars")$estimate^2)

# Proportion of unexplained variation that is between-subjects
sd_est$estimate[1]^2/sum(sd_est$estimate^2)

# Proportion of unexplained variation that is within-subjects
sd_est$estimate[2]^2/sum(sd_est$estimate^2)

# Fit unconditional growth model (between subject)
lmer.1.bs = lmer(matka ~ 1 + sarja_num + (1|Nimi), data = taist.rep, REML = FALSE)

# Coefficient-level output
tidy(lmer.1.bs, effects = "fixed")


# SD estimates
tidy(lmer.1.bs, effects = "ran_pars")

# Obtain variance estimates
sd_est <- tidy(lmer.1.bs, effects = "ran_pars")
sd_est

# Total unexplained variance
sum(tidy(lmer.1.bs, effects = "ran_pars")$estimate^2)

# Proportion of unexplained variation that is between-subjects
sd_est$estimate[1]^2/sum(sd_est$estimate^2)

# Proportion of unexplained variation that is within-subjects
sd_est$estimate[2]^2/sum(sd_est$estimate^2)

# Fit unconditional growth model
lmer.2 = lmer(matka ~ 1 + vuosi_intercept + Sarja + (1|Nimi) , data = taist.rep, REML = FALSE)

summary(lmer.2)

# Coefficient-level output
tidy(lmer.2, effects = "fixed")

# Obtain contender-specific coefficients
tidy(lmer.2, effects = "ran_coefs") %>% View()

# SD estimates
tidy(lmer.2, effects = "ran_pars")

# Obtain variance estimates
sd_est <- tidy(lmer.2, effects = "ran_pars")
sd_est

# Total unexplained variance
sum(tidy(lmer.2, effects = "ran_pars")$estimate^2)

# Proportion of unexplained variation that is between-subjects
sd_est$estimate[1]^2/sum(sd_est$estimate^2)

# Proportion of unexplained variation that is within-subjects
sd_est$estimate[2]^2/sum(sd_est$estimate^2)

aictab(
  cand.set = list(lmer.0, lmer.1.ws, lmer.1.bs, lmer.2),
  modnames = c("No change", "Linear growth within subject", "Linear growth between subject", "Linear growth between subject")
)

### Kooste
ajon.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = Aika_s, color = Sarja), size = 3) +
  geom_line(aes(x = vuosi, y = Aika_s, group = Nimi, color = Sarja), linewidth = 1) +
  scale_color_manual(values = palette_vahres_5) +
  scale_y_continuous(trans='log10') +
  theme_minimal()

tykki.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = toistot, color = Sarja), size = 3) +
  geom_line(aes(x = vuosi, y = toistot, group = Nimi, color = Sarja), linewidth = 1) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()

taist.rep %>%
  ggplot() +
  geom_point(aes(x = vuosi, y = matka, color = Sarja), size = 3) +
  geom_line(aes(x = vuosi, y = matka, group = Nimi, color = Sarja), linewidth = 1) +
  scale_color_manual(values = palette_vahres_5) +
  theme_minimal()

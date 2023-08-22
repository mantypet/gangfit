library(tidyverse)

# ajoneuvon veto comparison

ajon22 <- data.frame(toisto = c(0,1,4,10,16,19),
                     aika = c(0,4,10,20,30,35.44)) %>%
  mutate(Vuosi = "2022")
ajon23 <- data.frame(toisto = c(0,1,5,11,16,19),
                     aika = c(0,3,10,20,30,38.53)) %>%
  mutate(Vuosi = "2023")

ajon <- bind_rows(ajon22, ajon23) %>%
  mutate(toistoa_sek = ifelse(aika < 0.01, 0, toisto/aika))

palette_vahres <- c("#c7d9bd","#8da043","#437338")

ggplot(ajon) +
  geom_line(aes(x = aika, y = toisto, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = aika, y = toisto, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Aika (sekuntia)") +
  ylab("Veto") + 
  ggtitle("Sotilasajoneuvon veto")
  

ggplot(ajon) +
  geom_line(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Veto #") +
  ylab("Vetonopeus (vetoa sekunnissa)") +
  ggtitle("Sotilasajoneuvon veto")

# tykin lavetin nosto

tykki22 <- data.frame(toisto = c(0,5,10,15,20,25,30,35),
                     aika = c(0,7,15,23,31,40,49,60)) %>%
  mutate(Vuosi = "2022")

tykki23 <- data.frame(toisto =c(0,5,10,15,20,25,30,35,37),
                     aika = c(0,7,13,21,29,38,47,57,60)) %>%
  mutate(Vuosi = "2023")

tykki <- bind_rows(tykki22, tykki23) %>%
  mutate(toistoa_sek = ifelse(aika < 0.01, 0, toisto/aika))

ggplot(tykki) +
  geom_line(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Toisto #") +
  ylab("Toistonopeus (toistoa sekunnissa)") +
  ggtitle("Tykkilavetin nosto")

# taistelijan pyörä

taist22 <- data.frame(toisto = c(0,20,40,60,80,100,105),
                      aika = c(0,14,26,38,50,61,65)) %>%
  mutate(Vuosi = "2022")

taist23 <- data.frame(toisto =c(0,20,40,60,80,100,120,125),
                      aika = c(0,13,25,37,49,60,71,75)) %>%
  mutate(Vuosi = "2023")

taist <- bind_rows(taist22, taist23) %>%
  mutate(toistoa_sek = ifelse(aika < 0.01, 0, toisto/aika))

ggplot(taist) +
  geom_line(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Matka (m)") +
  ylab("Nopeus (m/s)") +
  ggtitle("Taistelijan pyörä")

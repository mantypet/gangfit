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

ggplot(ajon) +
  geom_line(aes(x = aika, y = toisto, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = aika, y = toisto, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Aika (sekuntia)") +
  ylab("Veto") + 
  ggtitle("Sotilasajoneuvon veto")
  
palette_vahres <- c("#c7d9bd","#8da043","#437338")

ggplot(ajon) +
  geom_line(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), linewidth = 0.8) +
  geom_point(aes(x = toisto, y = toistoa_sek, group = Vuosi, color = Vuosi), size = 2.2) +
  scale_color_manual(values = palette_vahres[c(1,3)]) +
  theme_minimal() +
  xlab("Veto #") +
  ylab("Vetonopeus (vetoa sekunnissa)") +
  ggtitle("Sotilasajoneuvon veto")

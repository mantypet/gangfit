library(tidyverse)

# oma 35.44
auto <- c(42.97, 51.22, 42.34, 37.50, 33.54, 50.87, 35.44)
# oma 35
tykki <- c(26, 5, 35, 39, 29, 27, 35)
# oma 105
jerry <- c(73, 65, 134, 93, 96, 62, 105)

res <- data.frame(auto, tykki, jerry) %>%
  pivot_longer(cols = everything(), names_to = "laji", values_to = "tulos")

library(ggplot2)

ggplot(filter(res, laji == "auto")) +
  geom_boxplot(aes(x = laji, y = tulos)) +
  geom_point(aes(x = laji, y = tulos)) +
  geom_point(aes(x = "auto", y = 35.44), size = 3, color = "red")

ggplot(filter(res, laji == "tykki")) +
  geom_boxplot(aes(x = laji, y = tulos)) +
  geom_point(aes(x = laji, y = tulos)) +
  geom_point(aes(x = "tykki", y = 35), size = 3, color = "red")

ggplot(filter(res, laji == "jerry")) +
  geom_boxplot(aes(x = laji, y = tulos)) +
  geom_point(aes(x = laji, y = tulos)) +
  geom_point(aes(x = "jerry", y = 105), size = 3, color = "red")
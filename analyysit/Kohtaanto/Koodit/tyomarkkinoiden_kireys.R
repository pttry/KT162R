library(tidyverse)
library(ggplot2)
library(gridExtra)

# Työmarkkinoiden tiukkuus

data %>% group_by(aluetyyppi, Kuukausi) %>%
  summarize(avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  mutate(tiukkuus = avoimet_tyopaikat / tyottomat) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  ggplot(aes(y = tiukkuus, x = time, col = aluetyyppi)) + geom_smooth(span = 0.2, se = FALSE)

ggsave("analyysit/Kohtaanto/Kuviot/tiukkuus_aluetyypeittain.png")


data %>% group_by(Kuukausi) %>%
  summarize(avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  mutate(tiukkuus = avoimet_tyopaikat / tyottomat) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  ggplot(aes(y = tiukkuus, x = time)) + geom_line() + geom_smooth(span = 0.2, se = FALSE)

ggsave("analyysit/Kohtaanto/Kuviot/tiukkuus.png")


p1 <- data %>% group_by(Kuukausi) %>%
  summarize(Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            Tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  ggplot(aes(x = Kuukausi, y = Tyottomat, group = 1)) + geom_line() + geom_smooth(span = 0.2)
p2 <- data %>% group_by(Kuukausi) %>%
  summarize(Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            Tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  ggplot(aes(x = Kuukausi, y = Avoimet_tyopaikat, group = 1)) + geom_line() + geom_smooth(span = 0.2)

grid.arrange(p1,p2, ncol = 1) -> p

ggsave("analyysit/Kohtaanto/Kuviot/tyottomat_ja_avoimet_tyopaikat.png", plot = p)

# Työttömät ja vakanssit aluetyypeittäin

library(ggplot2)
library(tidyverse)
library(statfitools)
library(ggptt)

data <- readRDS("data/avoimet_tyopaikat_tyonhakijat.rds")

df <- data %>% filter(Kuukausi == "2018M12") %>%
  group_by(aluetyyppi) %>%
  summarize(tyottomat = sum(Tyottomat, na.rm = TRUE),
            tyovoima = sum(Tyovoima, na.rm = TRUE),
            avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = tyottomat / tyovoima,
         vakanssiaste = avoimet_tyopaikat / (avoimet_tyopaikat + tyovoima)) %>%
  gather(tiedot, value, -aluetyyppi)

absoluuttiset <- df %>% filter(tiedot %in% c("tyottomat", "avoimet_tyopaikat")) %>%
  ggplot(aes(x = aluetyyppi, y = value, fill = tiedot)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  labs(fill = NULL,
       y = NULL,
       x = NULL,
       title = "2019M6") +
  scale_x_discrete(labels = c("Kaupunki",
                              "Kaupungin läheinen maaseutu",
                              "Harvaan asuttu maaseutu",
                              "Pääkaupunkiseutu",
                              "Työssäkäyntikeskus",
                              "Ydinmaaseutu",
                              "Yliopistokaupunki")) +
  scale_fill_discrete(labels = c("Avoimet työpaikat", "Tyottomat tyonhakijat"))

ggsave("analyysit/Kohtaanto/Kuviot/tyottomat_avoimet_tyopaikat_aluetyypeittain.png", plot = absoluuttiset)

suhteelliset <- df %>% filter(tiedot %in% c("tyottomyysaste", "vakanssiaste")) %>%
  ggplot(aes(x = aluetyyppi, y = value, fill = tiedot)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  labs(fill = NULL,
       y = NULL,
       x = NULL,
       title = "2018M12") +
  scale_x_discrete(labels = c("Kaupunki",
                              "Kaupungin läheinen maaseutu",
                              "Harvaan asuttu maaseutu",
                              "Pääkaupunkiseutu",
                              "Työssäkäyntikeskus",
                              "Ydinmaaseutu",
                              "Yliopistokaupunki")) +
  scale_fill_discrete(labels = c( "Työttömyys", "Vakanssiaste"))

grid.arrange(absoluuttiset, suhteelliset + theme(axis.text.y = element_blank()), ncol = 2)

load("~/git_clones/KT162R/data/dat_muutto_aikasarja_km.rda")

library(ggplot2)
library(tidyverse)
library(ggptt)
library(statfitools)

df <- dat_muutto_aikasarja_km %>%
  filter(tiedot %in% c("seutukuntien_valinen_muutto",
                       "seutukuntien_valinen_tyoikaisten_muutto",
                       "seutukuntien_valinen_tyon_siirtyminen",
                       "seutukuntien_valinen_tyota_siirtava_muutto",
                       "seutukuntien_valinen_tyollisten_muutto",
                       "seutukuntien_valinen_tyottomien_tyollistava_muutto",
                       "seutukuntien_valinen_tyovoiman_ulkopuolelta_tyollistava_muutto",
                       "LMA_valinen_muutto",
                       "LMA_valinen_tyoikaisten_muutto",
                       "LMA_valinen_tyon_siirtyminen",
                       "LMA_valinen_tyota_siirtava_muutto",
                       "LMA_valinen_tyollisten_muutto",
                       "LMA_valinen_tyottomien_tyollistava_muutto",
                       "LMA_valinen_tyovoiman_ulkopuolelta_tyollistava_muutto")) %>%
  mutate(alue = ifelse(grepl("LMA", tiedot), "LMA", "seutukunta")) %>%
  mutate(tiedot = gsub("seutukuntien_valinen_", "", tiedot),
         tiedot = gsub("LMA_valinen_", "", tiedot))
  mutate(value = as.integer(value))

df %>%
  filter(alue == "seutukunta") %>%
  ggplot(aes(time, value/1000, colour = tiedot)) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(y = "1000 henkeä", x = NULL, colour = NULL) +
  theme(legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left") +
  scale_colour_discrete(labels = c("Muutto",
                                "Työikäisten muutto",
                                "Työllisten muutto",
                                "Työn siirtyminen",
                                "Työtä siirtävä muutto",
                                "Työttömien muuttaessa työllistyminen",
                                "Työvoiman ulkopuolelta muuttaessa työllistyminen"))

ggsave("analyysit/Pendelointi_ja_muutto/tyon_siirtyminen.png",
       width = 9, height = 6)

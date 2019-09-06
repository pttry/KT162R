# dekomponoidaan muuttoaikasarja muuttoa edeltävän pääasiallisen toiminnan mukaan.

library(tidyverse)
library(ggplot2)
library(ggptt)

set_ptt()

data <- readRDS("data/paa-asiallinen_toiminta_ja_muutto/muuttoaikasarjat_kokomaa_ptoim1_ulos.rds")

  labels1 <- c("24 = eläkeläinen",
              "21 = 0-14 -vuotias",
              "99 = muu työvoiman ulkopuolella oleva",
              "22 = opiskelija, koululainen",
              "25 = varusmies, siviilipalvelusmies",
              "11 = työllinen",
              "12 = työtön",
              "29 = työttömyyseläkkeellä")

  labels2 <- c("Eläkeläiset",
               "0-14 -vuotiaat",
               "Muut työvoiman ulkopuolella olevat",
               "Opiskelijat ja koululaiset",
               "Varus- ja siviilipalvelusmiehet",
               "Työlliset",
               "Työttömät",
               "Työttömyyseläkeläiset")

  # Kuntien väliset muutot

  data %>% filter(tiedot %in% c("kuntien_valinen_tyottomien_muutto",
                              "kuntien_valinen_tyollisten_muutto",
                              "kuntien_valinen_opiskelijoiden_muutto",
                              "kuntien_valinen_lasten_muutto",
                              "kuntien_valinen_palvelusmiesten_muutto",
                              "kuntien_valinen_elakelaisten_muutto",
                              "kuntien_valinen_tyottomyyselakelaisten_muutto",
                              "kuntien_valinen_muutyovoimanulkopuolella_muutto")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  #theme_light(14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  scale_fill_manual(labels = labels2,
                      values = ggptt_palettes$ptt_new) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(labels = deci_comma) +
  labs(x = NULL, y = "Kuntien välisiä muuttoja", caption = "Lähde: Tilastokeskus, PTT")

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/Kuvioita/kuntien_valiset_muutot_ptoim1.png")

  # Seutukuntien väliset muutot

  data %>% filter(tiedot %in% c("seutukuntien_valinen_tyottomien_muutto",
                                "seutukuntien_valinen_tyollisten_muutto",
                                "seutukuntien_valinen_opiskelijoiden_muutto",
                                "seutukuntien_valinen_lasten_muutto",
                                "seutukuntien_valinen_palvelusmiesten_muutto",
                                "seutukuntien_valinen_elakelaisten_muutto",
                                "seutukuntien_valinen_tyottomyyselakelaisten_muutto",
                                "seutukuntien_valinen_muutyovoimanulkopuolella_muutto")) %>%
    ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
    geom_area() +
    theme_light(14) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left") +
    scale_fill_manual(labels = labels2,
                      values = ggptt_palettes$ptt_new) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    scale_y_continuous(labels = deci_comma) +
    labs(x = NULL, y = "Seutukuntien välisiä muuttoja", caption = "Lähde: Tilastokeskus, PTT")

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/Kuvioita/seutukuntien_valiset_muutot_ptoim1.png")

  # Maakuntien väliset muutot

  data %>% filter(tiedot %in% c("maakuntien_valinen_tyottomien_muutto",
                                "maakuntien_valinen_tyollisten_muutto",
                                "maakuntien_valinen_opiskelijoiden_muutto",
                                "maakuntien_valinen_lasten_muutto",
                                "maakuntien_valinen_palvelusmiesten_muutto",
                                "maakuntien_valinen_elakelaisten_muutto",
                                "maakuntien_valinen_tyottomyyselakelaisten_muutto",
                                "maakuntien_valinen_muutyovoimanulkopuolella_muutto")) %>%
    ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
    geom_area() +
    theme_light(14) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left") +
    scale_fill_discrete(labels = labels2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/Kuvioita/maakuntien_valiset_muutot_ptoim1.png")

  # Aluetyyppien väliset muutot

  data %>% filter(tiedot %in% c("aluetyyppien_valinen_tyottomien_muutto",
                                "aluetyyppien_valinen_tyollisten_muutto",
                                "aluetyyppien_valinen_opiskelijoiden_muutto",
                                "aluetyyppien_valinen_lasten_muutto",
                                "aluetyyppien_valinen_palvelusmiesten_muutto",
                                "aluetyyppien_valinen_elakelaisten_muutto",
                                "aluetyyppien_valinen_tyottomyyselakelaisten_muutto",
                                "aluetyyppien_valinen_muutyovoimanulkopuolella_muutto")) %>%
    ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
    geom_area() +
    theme_light(14) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left") +
    scale_fill_discrete(labels = labels2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/Kuvioita/aluetyyppien_valiset_muutot_ptoim1.png")

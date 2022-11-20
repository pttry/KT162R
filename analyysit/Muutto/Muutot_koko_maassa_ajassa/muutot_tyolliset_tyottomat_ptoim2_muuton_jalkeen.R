# dekomponoidaan muuttoaikasarja tyottominen ja työllisten muuton jälkeisen ptoim2 mukaan 

library(tidyverse)
library(ggplot2)

data <- readRDS("W:/Liikkuvuusvalinnat/Aineistot/muuttoaikasarjat_kokomaa_tyolliset_tyottomat_ptoim2_muuton_jalkeen.rds")

# Kuntien väliset muutot

data %>% filter(tiedot %in% c("kuntien_valinen_tyollinen_opiskelijaksi_muutto",
                              "kuntien_valinen_tyollinen_palvelusmieheksi_muutto",
                              "kuntien_valinen_tyollinen_elakelaiseksi_muutto",
                              "kuntien_valinen_tyollinen_tyottomyyselakelaiseksi_muutto",
                              "kuntien_valinen_tyollinen_muutyovoimanulkopuolellaolevaksi_muutto",
                              "kuntien_valinen_tyollinen_tyolliseksi_muutto",
                              "kuntien_valinen_tyollinen_tyottomaksi_muutto")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  theme_light(14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  # scale_fill_discrete(labels = c("24 = eläkeläinen",
  #                                "99 = muu työvoiman ulkopuolella oleva",
  #                                "22 = opiskelija, koululainen",
  #                                "25 = varusmies, siviilipalvelusmies",
  #                                "11 = työllinen",
  #                                "12 = työtön", 
  #                                "29 = työttömyyseläkkeellä")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

ggsave("W:/Liikkuvuusvalinnat/Output_files/migration_timeseries/muutot_tyolliset_tyottomat_ptoim2_muuton_jalkeen/kuntien_valiset_tyolliset.png")

data %>% filter(tiedot %in% c("kuntien_valinen_tyoton_opiskelijaksi_muutto",
                              "kuntien_valinen_tyoton_palvelusmieheksi_muutto",
                              "kuntien_valinen_tyoton_elakelaiseksi_muutto",
                              "kuntien_valinen_tyoton_tyottomyyselakelaiseksi_muutto",
                              "kuntien_valinen_tyoton_muutyovoimanulkopuolellaolevaksi_muutto",
                              "kuntien_valinen_tyoton_tyolliseksi_muutto",
                              "kuntien_valinen_tyoton_tyottomaksi_muutto")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  theme_light(14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  # scale_fill_discrete(labels = c("24 = eläkeläinen",
  #                                "99 = muu työvoiman ulkopuolella oleva",
  #                                "22 = opiskelija, koululainen",
  #                                "25 = varusmies, siviilipalvelusmies",
  #                                "11 = työllinen",
  #                                "12 = työtön", 
  #                                "29 = työttömyyseläkkeellä")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

ggsave("W:/Liikkuvuusvalinnat/Output_files/migration_timeseries/muutot_tyolliset_tyottomat_ptoim2_muuton_jalkeen/kuntien_valiset_tyottomat.png")

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
  scale_fill_discrete(labels = c("24 = eläkeläinen",
                                 "21 = 0-14 -vuotias",
                                 "99 = muu työvoiman ulkopuolella oleva",
                                 "22 = opiskelija, koululainen",
                                 "25 = varusmies, siviilipalvelusmies",
                                 "11 = työllinen",
                                 "12 = työtön", 
                                 "29 = työttömyyseläkkeellä")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

ggsave("W:/Liikkuvuusvalinnat/Output_files/migration_timeseries/muutot_ptoim2_ennen_muuttoa/seutukuntien_valiset_muuttajat_ptoim2_ennen_muuttoa.png")

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
  scale_fill_discrete(labels = c("24 = eläkeläinen",
                                 "21 = 0-14 -vuotias",
                                 "99 = muu työvoiman ulkopuolella oleva",
                                 "22 = opiskelija, koululainen",
                                 "25 = varusmies, siviilipalvelusmies",
                                 "11 = työllinen",
                                 "12 = työtön", 
                                 "29 = työttömyyseläkkeellä")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = NULL, y = "Muuttoja", caption = "Lähde: Tilastokeskus, PTT")

ggsave("W:/Liikkuvuusvalinnat/Output_files/migration_timeseries/muutot_ptoim2_ennen_muuttoa/maakuntien_valiset_muuttajat_ptoim2_ennen_muuttoa.png")

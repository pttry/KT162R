library(tidyverse)
library(ggptt)
library(RColorBrewer)
load("~/git_clones/KT162R/data/dat_muutto_aikasarja_km.rda")

source("R/set.R")

set_proj()

data <- dat_muutto_aikasarja_km %>% filter(tiedot %in% c("tyollistymiset_tyovoiman_ulkopuolelta",
                                                 "tyollistymiset_tyottomyydesta",
                                                 "kuntien_valinen_tyottomien_tyollistava_muutto",
                                                 "seutukuntien_valinen_tyottomien_tyollistava_muutto",
                                                 "maakuntien_valinen_tyottomien_tyollistava_muutto",
                                                 "kuntien_valinen_tyottomien_muutto_ei_tyollisyyteen",
                                                 "seutukuntien_valinen_tyottomien_muutto_ei_tyollisyyteen",
                                                 "maakuntien_valinen_tyottomien_muutto_ei_tyollisyyteen",
                                                 "kuntien_valinen_tyovoiman_ulkopuolelta_tyollistava_muutto",
                                                 "seutukuntien_valinen_tyovoiman_ulkopuolelta_tyollistava_muutto",
                                                 "maakuntien_valinen_tyovoiman_ulkopuolelta_tyollistava_muutto",
                                                 "kuntien_valinen_tyollisten_muutto",
                                                 "seutukuntien_valinen_tyollisten_muutto",
                                                 "maakuntien_valinen_tyollisten_muutto",
                                                 "kuntien_valinen_yrittajien_muutto",
                                                 "seutukuntien_valinen_yrittajien_muutto",
                                                 "maakuntien_valinen_yrittajien_muutto",
                                                 "kuntien_valinen_yrittajasta_yrittajaksi_muutto",
                                                 "seutukuntien_valinen_yrittajasta_yrittajaksi_muutto",
                                                 "maakuntien_valinen_yrittajasta_yrittajaksi_muutto",
                                                 "kuntien_valinen_yrittajasta_palkansaajaksi_muutto",
                                                 "seutukuntien_valinen_yrittajasta_palkansaajaksi_muutto",
                                                 "maakuntien_valinen_yrittajasta_palkansaajaksi_muutto"))

# Yrittajat kuntien välinen

data %>% filter(tiedot %in% c("kuntien_valinen_yrittajien_muutto",
                              "kuntien_valinen_yrittajasta_yrittajaksi_muutto",
                              "kuntien_valinen_yrittajasta_palkansaajaksi_muutto")) %>%
         spread(tiedot, value) %>%
         mutate(palkansaajaksi = kuntien_valinen_yrittajasta_palkansaajaksi_muutto,
                yrittajaksi = kuntien_valinen_yrittajasta_yrittajaksi_muutto,
                muuksi = kuntien_valinen_yrittajien_muutto -
                                  kuntien_valinen_yrittajasta_palkansaajaksi_muutto -
                                  kuntien_valinen_yrittajasta_yrittajaksi_muutto) %>%
         select(time, palkansaajaksi, yrittajaksi, muuksi) %>%
         gather(tiedot, value, -time) %>%
         ggplot(aes(x = time, y = value, fill = tiedot)) +
         geom_area() +
         labs(y = "Muuttajia", x = NULL,
              title = "Yrittäjät",
              fill = NULL) +
         scale_fill_manual(labels = c("Muuksi", "Palkansaajaksi", "Yrittäjäksi"),
                           values = brewer.pal(4, "Blues")[2:4]) +
         scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

# Palkansaajat kuntien välinen

data %>% filter(tiedot %in% c("kuntien_valinen_yrittajien_muutto",
                              "kuntien_valinen_yrittajasta_yrittajaksi_muutto",
                              "kuntien_valinen_yrittajasta_palkansaajaksi_muutto")) %>%
  spread(tiedot, value) %>%
  mutate(palkansaajaksi = kuntien_valinen_yrittajasta_palkansaajaksi_muutto,
         yrittajaksi = kuntien_valinen_yrittajasta_yrittajaksi_muutto,
         muuksi = kuntien_valinen_yrittajien_muutto -
           kuntien_valinen_yrittajasta_palkansaajaksi_muutto -
           kuntien_valinen_yrittajasta_yrittajaksi_muutto) %>%
  select(time, palkansaajaksi, yrittajaksi, muuksi) %>%
  gather(tiedot, value, -time) %>%
  ggplot(aes(x = time, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Muuttajia", x = NULL,
       title = "Yrittäjät",
       fill = NULL) +
  scale_fill_manual(labels = c("Muuksi", "Palkansaajaksi", "Yrittäjäksi"),
                    values = brewer.pal(4, "Blues")[2:4]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

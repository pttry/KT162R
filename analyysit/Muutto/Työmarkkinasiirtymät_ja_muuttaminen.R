
library(ggptt)
library(RColorBrewer)
library(ggpubr)

data <- readRDS("data/nov1/liikkuvuusaikasarja.rds")

source("R/set.R")
set_proj()

title_size = 10
axis_title_size = 10
axis_text_size = 10
colors = RColorBrewer::brewer.pal(5, "Blues")[2:5]

# Palkansaajat seutukuntien välinen

p1 <- data %>% filter(tiedot %in% c("seutukuntien_valinen_palkansaajien_muutto",
                              "seutukuntien_valinen_palkansaajien_yrittajiksi_muutto",
                              "seutukuntien_valinen_palkansaajien_palkansaajiksi_muutto",
                              "seutukuntien_valinen_palkansaajien_tyottomaksi_muutto")) %>%
  spread(tiedot, value) %>%
  mutate(seutukuntien_valinen_palkansaajien_tyovoiman_ulkopuolelle_muutto =
                seutukuntien_valinen_palkansaajien_muutto -
                seutukuntien_valinen_palkansaajien_yrittajiksi_muutto -
                seutukuntien_valinen_palkansaajien_palkansaajiksi_muutto -
                seutukuntien_valinen_palkansaajien_tyottomaksi_muutto) %>%
  select(-seutukuntien_valinen_palkansaajien_muutto) %>%
  gather(tiedot, value, -vuosi) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Muuttajia", x = NULL,
       title = "Palkansaajat",
       fill = NULL) +
  scale_fill_manual(labels = c("Palkansaajaksi",
                               "Tyottomaksi",
                               "Työvoiman ulkopuolelle",
                               "Yrittäjäksi"),
                    values = colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size))

# Yrittajien seutukuntien valinen

# Yrittäjien kokonaismuutto ilmeisesti laskettu väärin. Korjataan tässä laskemalla se työllisten
# ja palkansaajien kokonaismuutosta.

p2 <- data %>% filter(tiedot %in% c("seutukuntien_valinen_tyollisten_muutto",
                                    "seutukuntien_valinen_palkansaajien_muutto",
                              "seutukuntien_valinen_yrittajien_yrittajiksi_muutto",
                              "seutukuntien_valinen_yrittajien_palkansaajiksi_muutto",
                              "seutukuntien_valinen_yrittajien_tyottomaksi_muutto")) %>%
  spread(tiedot, value) %>%
  mutate(seutukuntien_valinen_yrittajien_muutto =
            seutukuntien_valinen_tyollisten_muutto - seutukuntien_valinen_palkansaajien_muutto) %>%
  mutate(seutukuntien_valinen_yrittajien_tyovoiman_ulkopuolelle_muutto =
           seutukuntien_valinen_yrittajien_muutto -
           seutukuntien_valinen_yrittajien_yrittajiksi_muutto -
           seutukuntien_valinen_yrittajien_palkansaajiksi_muutto -
           seutukuntien_valinen_yrittajien_tyottomaksi_muutto) %>%
  select(-seutukuntien_valinen_yrittajien_muutto,
         -seutukuntien_valinen_tyollisten_muutto,
         -seutukuntien_valinen_palkansaajien_muutto) %>%
  gather(tiedot, value, -vuosi) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Muuttajia", x = NULL,
       title = "Yrittäjät",
       fill = NULL) +
  scale_fill_manual(labels = c("Palkansaajaksi",
                               "Tyottomaksi",
                               "Työvoiman ulkopuolelle",
                                "Yrittäjäksi"),
                    values = colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size))

# Tyottomien seutukuntie valinen

p3 <- data %>% filter(tiedot %in% c("seutukuntien_valinen_tyottomien_muutto",
                                    "seutukuntien_valinen_tyottomien_yrittajiksi_muutto",
                                    "seutukuntien_valinen_tyottomien_palkansaajiksi_muutto",
                                    "seutukuntien_valinen_tyottomien_tyottomaksi_muutto")) %>%
  spread(tiedot, value) %>%
  mutate(seutukuntien_valinen_tyottomien_tyovoiman_ulkopuolelle_muutto =
           seutukuntien_valinen_tyottomien_muutto -
           seutukuntien_valinen_tyottomien_yrittajiksi_muutto -
           seutukuntien_valinen_tyottomien_palkansaajiksi_muutto -
           seutukuntien_valinen_tyottomien_tyottomaksi_muutto) %>%
  select(-seutukuntien_valinen_tyottomien_muutto) %>%
  gather(tiedot, value, -vuosi) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Muuttajia", x = NULL,
       title = "Työttömät",
       fill = NULL) +
  scale_fill_manual(labels = c("Palkansaajaksi",
                               "Tyottomaksi",
                               "Työvoiman ulkopuolelle",
                               "Yrittäjäksi"),
                    values = colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size))

# Työvoiman ulkopuolelta seutukuntien valinen

p4 <- data %>% filter(tiedot %in% c("seutukuntien_valinen_tyovoiman_ulkopuolelta_muutto",
                                    "seutukuntien_valinen_tyovoiman_ulkopuolelta_yrittajiksi_muutto",
                                    "seutukuntien_valinen_tyovoiman_ulkopuolelta_palkansaajaksi_muutto",
                                    "seutukuntien_valinen_tyovoiman_ulkopuolelta_tyottomaksi_muutto")) %>%
  spread(tiedot, value) %>%
  mutate(seutukuntien_valinen_tyovoiman_ulkopuolelta_tyovoiman_ulkopuolelle_muutto =
           seutukuntien_valinen_tyovoiman_ulkopuolelta_muutto -
           seutukuntien_valinen_tyovoiman_ulkopuolelta_yrittajiksi_muutto -
           seutukuntien_valinen_tyovoiman_ulkopuolelta_palkansaajaksi_muutto -
           seutukuntien_valinen_tyovoiman_ulkopuolelta_tyottomaksi_muutto) %>%
  select(-seutukuntien_valinen_tyovoiman_ulkopuolelta_muutto) %>%
  gather(tiedot, value, -vuosi) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Muuttajia", x = NULL,
       title = "Työvoiman ulkopuoliset",
       fill = NULL) +
  scale_fill_manual(labels = c("Palkansaajaksi",
                               "Tyottomaksi",
                               "Työvoiman ulkopuolelle",
                               "Yrittäjäksi"),
                    values = colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size))

p <- ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Muutto/tyomarkkinasiirtymat_ja_muuttaminen.png", plot = p,
       width = 8, height = 6)

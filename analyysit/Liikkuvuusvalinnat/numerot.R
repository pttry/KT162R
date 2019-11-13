library(ggptt)
library(RColorBrewer)
library(ggpubr)

data <- readRDS("data/nov12/muuttoliikkuvuusaikasarja_ulos.rds")

source("R/set.R")
set_proj()

title_size = 10
axis_title_size = 10
axis_text_size = 10
colors = RColorBrewer::brewer.pal(5, "Blues")[2:5]
y_upper_limit = 0.15

# Tuodaan nimittäjiä käsin koska unohtu

nimittajat <- data.frame(vuosi = 2006:2015, palkansaajat = c(2021612, 2068095, 2121443, 2130602, 2047320, 2079337, 2098246, 2084311, 2049463, 2026677),
                                            yrittajat = c(236694, 238511, 240124, 239457, 235824, 239111, 248742, 248116, 244572, 239175),
                                            tyottomat = c(281539, 246878, 217121, 231938, 298061, 266528, 255124, 279665, 326346, 360293),
                                            ulkopuoliset = c(2657178, 2663588, 2660942, 2664144, 2708896, 2728571, 2734922, 2750500, 2764256, 2778343))
nimittajat <- nimittajat %>% gather(tiedot, value, -vuosi)

data_kokonaisliikkuvuus <- data %>%
                           filter(tiedot %in% c("palkansaajien_lahtoliikkuvuus",
                                                "palkansaajien_palkansaajaksi_lahtomuutto",
                                                "palkansaajien_yrittajaksi_lahtomuutto",
                                                "yrittajien_lahtoliikkuvuus",
                                                "yrittajien_palkansaajaksi_lahtomuutto",
                                                "yrittajien_yrittajaksi_lahtomuutto",
                                                "tyottomien_lahtoliikkuvuus",
                                                "tyottomien_tyolliseksi_lahtomuutto",
                                                "ulkopuolelta_lahtoliikkuuus",
                                                "ulkopuolelta_tyolliseksi_lahtomuutto")) %>%
                           spread(tiedot, value) %>%
                           group_by(vuosi) %>%
                           summarize(palkansaajien_lahtoliikkuvuus = sum(palkansaajien_lahtoliikkuvuus, na.rm = TRUE),
                                     palkansaajien_palkansaajaksi_lahtomuutto = sum(palkansaajien_palkansaajaksi_lahtomuutto, na.rm = TRUE),
                                     palkansaajien_yrittajaksi_lahtomuutto = sum(palkansaajien_yrittajaksi_lahtomuutto, na.rm = TRUE),
                                     yrittajien_lahtoliikkuvuus = sum(yrittajien_lahtoliikkuvuus, na.rm = TRUE),
                                     yrittajien_palkansaajaksi_lahtomuutto = sum(yrittajien_palkansaajaksi_lahtomuutto, na.rm = TRUE),
                                     yrittajien_yrittajaksi_lahtomuutto = sum(yrittajien_yrittajaksi_lahtomuutto, na.rm = TRUE),
                                     tyottomien_lahtoliikkuvuus = sum(tyottomien_lahtoliikkuvuus, na.rm = TRUE),
                                     tyottomien_tyolliseksi_lahtomuutto = sum(tyottomien_tyolliseksi_lahtomuutto, na.rm = TRUE),
                                     ulkopuolelta_lahtoliikkuvuus = sum(ulkopuolelta_lahtoliikkuuus, na.rm = TRUE),
                                     ulkopuolelta_tyolliseksi_lahtomuutto = sum(ulkopuolelta_tyolliseksi_lahtomuutto, na.rm = TRUE)) %>%
                           mutate(palkansaajien_tyolliseksi_lahtomuutto = palkansaajien_palkansaajaksi_lahtomuutto +
                                                                          palkansaajien_yrittajaksi_lahtomuutto,
                                  yrittajien_tyolliseksi_lahtomuutto = yrittajien_palkansaajaksi_lahtomuutto +
                                                                       yrittajien_yrittajaksi_lahtomuutto) %>%
                           mutate(palkansaajien_liikkuvuus_muutto = palkansaajien_tyolliseksi_lahtomuutto,
                                  palkansaajien_liikkuvuus_pendelointi = palkansaajien_lahtoliikkuvuus - palkansaajien_liikkuvuus_muutto,
                                  yrittajien_liikkuvuus_muutto = yrittajien_tyolliseksi_lahtomuutto,
                                  yrittajien_liikkuvuus_pendelointi = yrittajien_lahtoliikkuvuus - yrittajien_liikkuvuus_muutto,
                                  tyottomien_liikkuvuus_muutto = tyottomien_tyolliseksi_lahtomuutto,
                                  tyottomien_liikkuvuus_pendelointi = tyottomien_lahtoliikkuvuus - tyottomien_liikkuvuus_muutto,
                                  ulkopuolelta_liikkuvuus_muutto = ulkopuolelta_tyolliseksi_lahtomuutto,
                                  ulkopuolelta_liikkuvuus_pendelointi = ulkopuolelta_lahtoliikkuvuus - ulkopuolelta_liikkuvuus_muutto) %>%
                           gather(tiedot, value, -vuosi)

# Palkansaajat

data_palkansaajat <- data_kokonaisliikkuvuus %>% filter(tiedot %in% c("palkansaajien_liikkuvuus_muutto",
                                                                      "palkansaajien_liikkuvuus_pendelointi"))
data_palkansaajat <- rbind(data_palkansaajat, filter(nimittajat, tiedot == "palkansaajat"))

data_palkansaajat <- data_palkansaajat %>% spread(tiedot, value) %>%
                                           mutate(liikkuvuus_muutto_prop = palkansaajien_liikkuvuus_muutto / palkansaajat,
                                                  liikkuvuus_pendelointi_prop = palkansaajien_liikkuvuus_pendelointi / palkansaajat) %>%
                     gather(tiedot, value, -vuosi)

p1 <- data_palkansaajat %>%
          filter(tiedot %in% c("liikkuvuus_muutto_prop", "liikkuvuus_pendelointi_prop")) %>%
                            ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
                              geom_area() +
                              labs(y = "Liikkuvien osuus", x = NULL, fill = NULL, title = "Palkansaajat") +
                              scale_fill_manual(labels = c("Muuttanut", "Pendelöinyt"),
                                                values = ggptt_palettes$vnk[2:1]) +
                              scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                              theme(title = element_text(size = title_size),
                              axis.title = element_text(size = axis_title_size),
                              axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)

# Yrittäjät

data_yrittajat <- data_kokonaisliikkuvuus %>% filter(tiedot %in% c("yrittajien_liikkuvuus_muutto",
                                                                      "yrittajien_liikkuvuus_pendelointi"))
data_yrittajat <- rbind(data_yrittajat, filter(nimittajat, tiedot == "yrittajat"))

data_yrittajat <- data_yrittajat %>% spread(tiedot, value) %>%
  mutate(liikkuvuus_muutto_prop = yrittajien_liikkuvuus_muutto / yrittajat,
         liikkuvuus_pendelointi_prop = yrittajien_liikkuvuus_pendelointi / yrittajat) %>%
  gather(tiedot, value, -vuosi)

p2 <- data_yrittajat %>%
  filter(tiedot %in% c("liikkuvuus_muutto_prop", "liikkuvuus_pendelointi_prop")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Liikkuvien osuus", x = NULL, fill = NULL, title = "Yrittäjät") +
  scale_fill_manual(labels = c("Muuttanut", "Pendelöinyt"),
                    values = ggptt_palettes$vnk[2:1]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)

# Työttömät

data_tyottomat <- data_kokonaisliikkuvuus %>% filter(tiedot %in% c("tyottomien_liikkuvuus_muutto",
                                                                   "tyottomien_liikkuvuus_pendelointi"))
data_tyottomat <- rbind(data_tyottomat, filter(nimittajat, tiedot == "tyottomat"))

data_tyottomat <- data_tyottomat %>% spread(tiedot, value) %>%
  mutate(liikkuvuus_muutto_prop = tyottomien_liikkuvuus_muutto / tyottomat,
         liikkuvuus_pendelointi_prop = tyottomien_liikkuvuus_pendelointi / tyottomat) %>%
  gather(tiedot, value, -vuosi)

p3 <- data_tyottomat %>%
  filter(tiedot %in% c("liikkuvuus_muutto_prop", "liikkuvuus_pendelointi_prop")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Liikkuvien osuus", x = NULL, fill = NULL, title = "Työttömät") +
  scale_fill_manual(labels = c("Muuttanut", "Pendelöinyt"),
                    values = ggptt_palettes$vnk[2:1]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)

# ulkopuoliset

data_ulkopuoliset <- data_kokonaisliikkuvuus %>% filter(tiedot %in% c("ulkopuolelta_liikkuvuus_muutto",
                                                                   "ulkopuolelta_liikkuvuus_pendelointi"))
data_ulkopuoliset <- rbind(data_ulkopuoliset, filter(nimittajat, tiedot == "ulkopuoliset"))

data_ulkopuoliset <- data_ulkopuoliset %>% spread(tiedot, value) %>%
  mutate(liikkuvuus_muutto_prop = ulkopuolelta_liikkuvuus_muutto / ulkopuoliset,
         liikkuvuus_pendelointi_prop = ulkopuolelta_liikkuvuus_pendelointi / ulkopuoliset) %>%
  gather(tiedot, value, -vuosi)

p4 <- data_ulkopuoliset %>%
  filter(tiedot %in% c("liikkuvuus_muutto_prop", "liikkuvuus_pendelointi_prop")) %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Liikkuvien osuus", x = NULL, fill = NULL, title = "Työvoiman ulkopuoliset") +
  scale_fill_manual(labels = c("Muuttanut", "Pendelöinyt"),
                    values = ggptt_palettes$vnk[2:1]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)


p <- ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Liikkuvuusvalinnat/numerot_kuva.png", plot = p,
       width = 8, height = 6)



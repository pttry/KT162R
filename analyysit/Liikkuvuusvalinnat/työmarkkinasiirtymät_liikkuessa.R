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

data <- data %>% filter(tiedot %in% c("palkansaajien_palkansaajaksi_lahtoliikkuvuus",
                                      "palkansaajien_yrittajaksi_lahtoliikkuvuus",
                                      "tyottomien_palkansaajaksi_lahtoliikkuvuus",
                                      "tyottomien_yrittajaksi_lahtoliikkuvuus",
                                      "yrittajien_palkansaajaksi_lahtoliikuvuus",
                                      "yrittajien_yrittajaksi_lahtoliikkuvuus",
                                      "ulkopuolelta_palknasaajaksi_lahtoliikkuvuus",
                                      "ulkopuolelta_yrittajaksi_lahtoliikkuvuus")) %>%
                 spread(tiedot, value) %>%
                 group_by(vuosi) %>%
                 summarize(palkansaajien_palkansaajaksi_lahtoliikkuvuus = sum(palkansaajien_palkansaajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           palkansaajien_yrittajaksi_lahtoliikkuvuus = sum(palkansaajien_yrittajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           tyottomien_palkansaajaksi_lahtoliikkuvuus = sum(tyottomien_palkansaajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           tyottomien_yrittajaksi_lahtoliikkuvuus = sum(tyottomien_yrittajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           yrittajien_palkansaajaksi_lahtoliikkuvuus = sum(yrittajien_palkansaajaksi_lahtoliikuvuus, na.rm = TRUE),
                           yrittajien_yrittajaksi_lahtoliikkuvuus = sum(yrittajien_yrittajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           ulkopuolelta_palkansaajaksi_lahtoliikkuvuus = sum(ulkopuolelta_palknasaajaksi_lahtoliikkuvuus, na.rm = TRUE),
                           ulkopuolelta_yrittajaksi_lahtoliikkuvuus = sum(ulkopuolelta_yrittajaksi_lahtoliikkuvuus, na.rm = TRUE)) %>%
                 gather(tiedot, value, -vuosi)

nimittajat <- data.frame(vuosi = 2006:2015, palkansaajat = c(2021612, 2068095, 2121443, 2130602, 2047320, 2079337, 2098246, 2084311, 2049463, 2026677),
                         yrittajat = c(236694, 238511, 240124, 239457, 235824, 239111, 248742, 248116, 244572, 239175),
                         tyottomat = c(281539, 246878, 217121, 231938, 298061, 266528, 255124, 279665, 326346, 360293),
                         ulkopuoliset = c(2657178, 2663588, 2660942, 2664144, 2708896, 2728571, 2734922, 2750500, 2764256, 2778343))
nimittajat <- nimittajat %>% gather(tiedot, value, -vuosi)

data <- rbind(data, nimittajat)

data <- data %>% spread(tiedot, value) %>%
                 mutate(palkansaajien_palkansaajaksi_prop = palkansaajien_palkansaajaksi_lahtoliikkuvuus / palkansaajat,
                        palkansaajien_yrittajaksi_prop = palkansaajien_yrittajaksi_lahtoliikkuvuus / palkansaajat,
                        yrittajien_yrittajaksi_prop = yrittajien_yrittajaksi_lahtoliikkuvuus / yrittajat,
                        yrittajien_palkansaajaksi_prop = yrittajien_palkansaajaksi_lahtoliikkuvuus / yrittajat,
                        tyottomien_palkansaajaksi_prop = tyottomien_palkansaajaksi_lahtoliikkuvuus / tyottomat,
                        tyottomien_yrittajaksi_prop = tyottomien_yrittajaksi_lahtoliikkuvuus / yrittajat,
                        ulkopuolelta_palkansaajaksi_prop = ulkopuolelta_palkansaajaksi_lahtoliikkuvuus / ulkopuoliset,
                        ulkopuolelta_yrittajaksi_prop = ulkopuolelta_yrittajaksi_lahtoliikkuvuus / ulkopuoliset) %>%
                  gather(tiedot, value, -vuosi)


# Palkansaajat

data_palkansaajat <- data %>% filter(tiedot %in% c("palkansaajien_yrittajaksi_prop",
                                                   "palkansaajien_palkansaajaksi_prop"))

p1 <- data_palkansaajat %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Osuus palkansaajista", x = NULL, fill = NULL, title = "Palkansaajat") +
  scale_fill_manual(labels = c("Palkansaajaksi", "Yrittäjäksi"),
                    values = ggptt_palettes$vnk[2:1]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)

# Yrittäjät

data_yrittajat <- data %>% filter(tiedot %in% c("yrittajien_yrittajaksi_prop",
                                                   "yrittajien_palkansaajaksi_prop"))

p2 <- data_yrittajat %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Osuus yrittäjista", x = NULL, fill = NULL, title = "Yrittäjät") +
  scale_fill_manual(labels = c("Palkansaajaksi", "Yrittäjäksi"),
                    values = ggptt_palettes$vnk[2:1]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(title = element_text(size = title_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size)) +
  ylim(0,y_upper_limit)

# Työttömät

data_tyottomat <- data%>% filter(tiedot %in% c("tyottomien_yrittajaksi_prop",
                                               "tyottomien_palkansaajaksi_prop"))

p3 <- data_tyottomat %>%
  ggplot(aes(x = vuosi, y = value, fill = tiedot)) +
  geom_area() +
  labs(y = "Liikkuvien osuus", x = NULL, fill = NULL, title = "Työttömät") +
  scale_fill_manual(labels = c("Palkansaajaksi", "Yrittäjäksi"),
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


library(RColorBrewer)
library(ggpubr)
library(pxweb)
library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(sf)


data <- readRDS("data/nov12/muuttoliikkuvuusaikasarja_ulos.rds") %>%
  filter(tiedot %in% c("tyolliset", "tyottomat", "tyollisten_lahtoliikkuvuus", "tyottomien_lahtoliikkuvuus")) %>%
  filter(vuosi == 2014)

data_tyolliset <- filter(data, grepl("tyol", tiedot))
data_tyottomat <- filter(data, grepl("tyot", tiedot))

data_tyolliset <- spread(data_tyolliset, tiedot, value) %>%
  mutate(liikkuvien_osuus = tyollisten_lahtoliikkuvuus / tyolliset) %>%
  select(alue, liikkuvien_osuus) %>%
  rename(seutukunta = alue)

data_tyottomat <- spread(data_tyottomat, tiedot, value) %>%
  mutate(liikkuvien_osuus = tyottomien_lahtoliikkuvuus / tyottomat) %>%
  select(alue, liikkuvien_osuus)  %>%
  rename(seutukunta = alue)

data_tyolliset <- as.tibble(data.frame(seutukunta = data_tyolliset$seutukunta,
                                       liikkuvien_osuus = c(1.275781e-02, 3.417869e-02, 4.252814e-02, 6.440260e-02,
                                                            5.305078e-02, 5.200725e-02, 3.022369e-02, 3.811799e-02,
                                                            4.366087e-02, 3.546921e-02, 3.059677e-02, 3.842601e-02,
                                                            4.493231e-02, 5.538704e-02, 4.316273e-02, 6.124357e-02,
                                                            4.992515e-02, 3.188810e-02, 5.076581e-02, 4.256314e-02,
                                                            3.382358e-02, 3.682043e-02, 3.109751e-02, 3.436048e-02,
                                                            5.241368e-02, 3.341809e-02, 4.190182e-02, 4.289328e-02,
                                                            2.998015e-02, 3.667560e-02, 4.778555e-02, 4.568754e-02,
                                                            5.414098e-02, 2.653717e-02, 3.464987e-02, 2.827244e-02,
                                                            3.578335e-02, 5.263158e-02, 4.235669e-02, 4.068649e-02,
                                                            4.074926e-02, 3.980100e-02, 4.390091e-02, 2.944672e-02,
                                                            4.704753e-02, 4.005907e-02, 4.441833e-02, 2.759215e-02, 2.932551e-02,
                                                            2.275505e-02, 4.752985e-02, 3.077366e-02, 2.906425e-02,
                                                            4.441756e-02, 3.049759e-02, 4.334787e-02, 5.041355e-02,
                                                            3.711030e-02, 3.861147e-02, 4.148837e-02, 3.538785e-02,
                                                            4.226204e-02, 2.825894e-02, 5.121573e-02, 4.464286e-02,
                                                            5.226244e-02, 4.476721e-02, 3.985351e-02, 3.331625e-02,
                                                            5.007153e-02)))
data_tyolliset$seutukunta <- as.character(data_tyolliset$seutukunta)

data_tyottomat <- as.tibble(data.frame(seutukunta = data_tyottomat$seutukunta,
                                       liikkuvien_osuus = c(1.382380e-02, 6.473829e-02, 9.084195e-02,
                                                            7.743363e-02, 9.987358e-02, 6.486263e-02,
                                                            3.479668e-02, 6.056236e-02, 9.964413e-02,
                                                            6.515581e-02, 3.466423e-02, 6.005720e-02,
                                                            5.273834e-02, 9.869494e-02, 5.155090e-02,
                                                            7.908429e-02, 9.661836e-02, 3.399902e-02,
                                                            8.800000e-02, 6.424581e-02, 3.507662e-02,
                                                            4.605570e-02, 3.350208e-02, 3.692202e-02,
                                                            6.896552e-02, 3.121466e-02, 3.334344e-02,
                                                            7.552693e-02, 3.268482e-02, 4.118185e-02,
                                                            8.423326e-02, 5.338377e-02, 7.978723e-02,
                                                            2.977503e-02, 4.804393e-02, 3.243848e-02,
                                                            4.414274e-02, 7.466667e-02, 6.829897e-02,
                                                            4.925847e-02, 6.097561e-02, 5.126151e-02,
                                                            7.709924e-02, 4.997542e-02, 8.069164e-02,
                                                            5.588994e-02, 1.522936e-01, 3.303593e-02,
                                                            6.618962e-02, 5.295567e-02, 8.557074e-02,
                                                            5.267423e-02, 3.800720e-02, 9.208310e-02,
                                                            6.099361e-02, 5.875000e-02, 8.675497e-02,
                                                            8.047875e-02, 6.910850e-02, 5.073802e-02,
                                                            4.672435e-02, 4.665314e-02, 3.614973e-02,
                                                            6.939502e-02, 6.174334e-02, 5.033882e-02,
                                                            5.099395e-02, 1.660900e-01, 2.181070e-01,
                                                            1.891892e-01)))
data_tyolliset$seutukunta <- as.character(data_tyolliset$seutukunta)

# Hae kartta

file <- "tilastointialueet:seutukunta4500k_2018"

url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
url2$query <- list(service ="WFS",
                   version ="2.0.0",
                   request ="GetFeature",
                   typename = file,
                   outputFormat ="application/json")

map <- sf::st_read(httr::build_url(url2))

# Remove Ahvenanmaa

map <- filter(map, !(seutukunta %in% c(211, 212, 213)))
data_tyottomat <- filter(data_tyottomat, !(seutukunta %in% c(211, 212, 213)))
data_tyolliset <- filter(data_tyolliset, !(seutukunta %in% c(211, 212, 213)))


liikkuvuus_map_tyottomat <- left_join(map, data_tyottomat, by = "seutukunta")  %>%
  ggplot(aes(fill = liikkuvien_osuus)) +
  geom_sf() +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left") +
  labs(fill = "Työttömät") +
  scale_fill_gradient(low = "white", high = ggptt_palettes$vnk[1],
                      limits = c(0, 0.16),
                      labels = percent_comma,
                      guide = guide_colorbar(barwidth = 10))

liikkuvuus_map_tyolliset <- left_join(map, data_tyolliset, by = "seutukunta")  %>%
  ggplot(aes(fill = liikkuvien_osuus)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = ggptt_palettes$vnk[1],
                      limits = c(0,0.16),
                      labels = percent_comma,
                      guide = guide_colorbar(barwidth = 10)) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left") +
  labs(fill = "Palkansaajat")

kartat <- ggarrange(liikkuvuus_map_tyolliset, liikkuvuus_map_tyottomat, nrow = 1)

ggsave("analyysit/Liikkuvuusvalinnat/liikkuvuus_seutukunnittain_kartta.png", plot = kartat ,
       width = 8, height =6)

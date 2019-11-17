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
                      breaks = c(0.02, 0.07,0.12),
                      labels = percent_comma,
                      guide = guide_colorbar(barwidth = 10))

liikkuvuus_map_tyolliset <- left_join(map, data_tyolliset, by = "seutukunta")  %>%
  ggplot(aes(fill = liikkuvien_osuus)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = ggptt_palettes$vnk[1],
                      breaks = c(0.1, 0.3, 0.5),
                      labels = percent_comma,
guide = guide_colorbar(barwidth = 10)) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left") +
  labs(fill = "Työlliset")

kartat <- grid.arrange(liikkuvuus_map_tyolliset, liikkuvuus_map_tyottomat, nrow = 1)

ggsave("analyysit/Liikkuvuusvalinnat/liikkuvuus_seutukunnittain_kartta.png", plot = kartat )

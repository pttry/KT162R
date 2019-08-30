library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(sf)

atyypit <- readRDS("data/atyypit.rds") %>%
  rename(kunta = kunta18) %>%
  mutate(aluetyyppit = as.factor(aluetyyppi))

file <- paste("tilastointialueet:", "kunta", "4500k_", as.character(2018), sep = "")

url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
url2$query <- list(service ="WFS",
                   version ="2.0.0",
                   request ="GetFeature",
                   typename = file,
                   outputFormat ="application/json")

map <- sf::st_read(httr::build_url(url2))

left_join(map, atyypit, by = "kunta")  %>%
  ggplot(aes(fill = aluetyyppi)) +
  geom_sf() +
  theme_light() +
  labs(fill = NULL) +
  scale_fill_discrete(labels = c("Kaupunki",
                                 "Kaupungin läheinen maaseutu",
                                 "Harvaan asuttu maaseutu",
                                 "Pääkaupunkiseutu",
                                 "Työssäkäyntikeskus",
                                 "Ydinmaaseutu",
                                 "Yliopistokaupunki"))

ggsave("analyysit/Aluetyypit/aluetyyppikartta.png")

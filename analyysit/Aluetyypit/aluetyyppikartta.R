library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(sf)

atyypit <- readRDS("data/atyypit.rds") %>%
  rename(kunta = kunta18) %>%
  mutate(aluetyyppi = as.factor(aluetyyppi))

atyypit$aluetyyppi <- fct_relevel(atyypit$aluetyyppi, c("pk", "yo-kaup", "kaup", "tk_keskus", "kaupms", "ydinms", "ms"))
aluetyyppi_labels = c("Pääkaupunkiseutu", "Yliopistokaupungit", "Kaupungit", "Muut työssäkäyntikeskukset",
                      "Kaupunkien läheinen maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu")

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
  scale_fill_manual(labels = aluetyyppi_labels,
                    values =  c(brewer.pal(4, "Blues"), brewer.pal(3, "Greens")))

ggsave("analyysit/Aluetyypit/aluetyyppikartta.png")

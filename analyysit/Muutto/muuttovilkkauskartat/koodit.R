data(dat_muuttotiedot_kunnittain)

library(ggptt)
library(statfitools)
library(tidyverse)
library(pxweb)
library(gridExtra)

map_data <- dat_muuttotiedot_kunnittain %>%
            spread(Tiedot, values) %>%
            filter(Vuosi == 2018) %>%
            mutate(muuttotapahtumaaste = (tulomuutto + lahtomuutto) / vakiluku) %>%
            rename(kunta = kunta_no)


# Hae kartta

file <- paste("tilastointialueet:", "kunta", "4500k_", as.character(2018), sep = "")

url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
url2$query <- list(service ="WFS",
                   version ="2.0.0",
                   request ="GetFeature",
                   typename = file,
                   outputFormat ="application/json")

map <- sf::st_read(httr::build_url(url2))

muuttotapahtuma_map <- left_join(map, map_data, by = "kunta")  %>%
  ggplot(aes(fill = muuttotapahtumaaste)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = ggptt_palettes$vnk[1]) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left") +
  labs(fill = "Muuttotapahtuma-aste")

ggsave("analyysit/Muutto/muuttovilkkauskartat/muuttovilkkauskartta.png")



# Download data
# px_data <-
#   pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_11ra.px",
#             query = "[path to jsonfile]")
#
# # Convert to data.frame
# px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
#
# data <- data$data
#
# saveRDS(data, "data/asuinalueellaansyntyneidenosuus2018.rds")
# data <- data %>% rename(Kunta = Alue)
# alueet <- sf_get_reg_keytable() %>% select(Knro, Kunta, Mkkoodi, Maakunta, Seutukuntakoodi, Seutukunta, Kuntaryhma)
#
# data <- left_join(data, alueet, by = "Kunta")
#
# atyypit <- readRDS("data/atyypit.rds") %>%
#   rename(Knro = kunta18)
#
# data <- left_join(data, atyypit, by = "Knro") %>%
#   rename(kunta = Knro)
#
# data <- clean_names(data) %>%
#   filter(Vuosi == 2018) %>%
#   select(kunta, Asuinalueellaan_syntyneiden_osuus)

data <- readRDS("data/asuinalueellaansyntyneidenosuus2018.rds")




asuinalueellasyntyneet_map <- left_join(map, data, by = "kunta")  %>%
  ggplot(aes(fill = Asuinalueellaan_syntyneiden_osuus)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = ggptt_palettes$vnk[1]) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left") +
  labs(fill = "Asuinalueellaan syntyneiden osuus")

ggsave("analyysit/Muutto/muuttovilkkauskartat/asuinalueellasyntyneet_map.png")


kartat <- grid.arrange(muuttotapahtuma_map,  asuinalueellasyntyneet_map, ncol = 2)

ggsave("analyysit/Muutto/muuttovilkkauskartat/molemmat.png",
       plot = kartat, width = 8, height = 6)

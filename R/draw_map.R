# Tämä funktio piirtää datan Suomen kartalle käyttäjän määrittelemällä aluejaolla ja aluejaon vuodella.

draw_map <- function(data, vuosi, aluejako, x) {

  file <- paste("tilastointialueet:", aluejako, "4500k_", as.character(vuosi), sep = "")

  url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
  url2$query <- list(service ="WFS",
                     version ="2.0.0",
                     request ="GetFeature",
                     typename = file,
                     outputFormat ="application/json")

  map <- sf::st_read(httr::build_url(url2))

  output <- map %>%
             left_join(data, by = aluejako) %>%
             ggplot(aes(fill = x)) + geom_sf()
  output
}

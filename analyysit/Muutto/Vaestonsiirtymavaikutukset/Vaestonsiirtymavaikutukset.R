# Väestonmuutosvaikutukset
# Arvioidaan kuinka paljon muuttaminen siirtää väestöä eri aluetyypeillä.

# Huom. jos kuvia tallentaa raporttia varten, tulee captionit ja otsikot poistaa.

  library(ggplot2)
  library(tidyverse)
  library(statfitools)
  library(ggptt)
  library(gridExtra)
  library(RColorBrewer)
  library(ggpubr)

  set_ptt()
  theme_update(plot.subtitle = element_text(colour = "grey50", size = 9))

  atyyppi_colour <- c(brewer.pal(6, "Oranges")[6:5], brewer.pal(6, "Greens")[6], brewer.pal(6, "Blues")[6:5] , brewer.pal(6, "Greens")[5:4])


# Load data

  data(dat_muuttotiedot_kunnittain)

  dat_muuttotiedot_kunnittain$aluetyyppi <- fct_relevel(dat_muuttotiedot_kunnittain$aluetyyppi,
                                                        c("PK-seutu", "Muut yliopistokaupungit", "Muut työssäkäyntialueden keskukset", "Muut kaupungit",
                                                          "Kaupunkien läh. maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu"))
  aluetyyppi_labels = c("Pääkaupunkiseutu", "Yliopistokaupungit","Muut työssäkäyntikeskukset", "Kaupungit",
                        "Kaupunkien läheinen maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu")



  # Väestönmuutosvaikutukset maakunnittain, seutukunnittain ja kunnittain #################################


  # Kunnittaiset

  kunnittaiset_vaestomuutosvaikutukset <- dat_muuttotiedot_kunnittain %>%
    filter(values > 0, Tiedot == "nettomuutto") %>%
    group_by(Vuosi) %>%
    summarize(vaestonmuutosvaikutus = sum(values)) %>%
    mutate(tyyppi = "kunnittaiset")

  # seutukunnittaiset

  seutukunnittaiset_vaestomuutosvaikutukset <- dat_muuttotiedot_kunnittain %>%
    filter(Tiedot == "nettomuutto") %>%
    group_by(seutukunta, Vuosi) %>%
    summarize(nettomuutto = sum(values)) %>%
    filter(nettomuutto > 0) %>%
    ungroup() %>%
    group_by(Vuosi) %>%
    summarize(vaestonmuutosvaikutus = sum(nettomuutto)) %>%
    mutate(tyyppi = "seutukunnittaiset")

  # Maakunnittaiset

  maakunnittaiset_vaestomuutosvaikutukset <- dat_muuttotiedot_kunnittain %>%
    filter(Tiedot == "nettomuutto") %>%
    group_by(maakunta, Vuosi) %>%
    summarize(nettomuutto = sum(values)) %>%
    filter(nettomuutto > 0) %>%
    ungroup() %>%
    group_by(Vuosi) %>%
    summarize(vaestonmuutosvaikutus = sum(nettomuutto)) %>%
    mutate(tyyppi = "maakunnittaiset")

  vaestomuutosvaikutukset <- rbind(kunnittaiset_vaestomuutosvaikutukset,
                                   seutukunnittaiset_vaestomuutosvaikutukset,
                                   maakunnittaiset_vaestomuutosvaikutukset)

  vaestomuutosvaikutukset %>% ggplot(aes(x = Vuosi, y = vaestonmuutosvaikutus, color = tyyppi)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = 2) +
    theme_ptt() +
    theme(legend.position = "bottom", legend.justification = "left") +
    theme(legend.title = element_blank()) +
    scale_colour_manual(values = brewer.pal(4, "Blues")[2:4],
                        labels = c("Kunnittaiset",
                                   "Maakunnittaiset",
                                   "Seutukunnittaiset")) +
    labs(y = "Väestönsiirtymävaikutus",
         x = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


  ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/vaestonsiirtymavaikutukset_alueittain.png",
         height = 3,
         width = 7)



# Nettomuutto aluetyypeittäin ############################################

  # Absoluuttiset määrät
p1 <- dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values)) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuutto, color = aluetyyppi)) +
            geom_hline(yintercept = 0, color = "black", linetype = 2) +
            geom_line() +
            scale_colour_manual(values = atyyppi_colour,
                                labels = aluetyyppi_labels) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
            theme(legend.title = element_blank(),
                  legend.position = "bottom",
                  legend.justification = "left") +
            labs(x = NULL,
                 y = "Nettomuuttoja")

     ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/nettomuutto_atyyppi_viiva.png")

   # Muuttoasteet
p2 <-  dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values, na.rm = TRUE),
               vakea = sum(vakiluku, na.rm = TRUE)) %>%
     mutate(nettomuuttoaste = nettomuutto / vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuuttoaste, color = aluetyyppi)) +
            geom_hline(yintercept = 0, color = "black", linetype = 2) +
            geom_line()  +
            scale_colour_manual(values = atyyppi_colour,
                                labels = aluetyyppi_labels) +
            theme(legend.title = element_blank(),
                  legend.position = "bottom",
                  legend.justification = "left") +
            scale_y_continuous(labels = percent_comma) +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
            labs(x = NULL,
                 y = "Nettomuuttoaste")

   ggsave("vignettes/nettomuuttoaste_atyyppi_viiva.png")

p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/aluetyypeittain_molemmat.png", plot = p,
       height = 5,
       width = 8)

# Lähto- ja tulomuutot ###################################################################

    # Eli dekomponoidaan nettomuutto, vihreän ja oranssin palkin summa on nettomuutto.

# Muuta lähtömuuttojen arvot negatiivisiksi

   dat_muuttotiedot_kunnittain_mod <- filter(dat_muuttotiedot_kunnittain, Tiedot %in% c("tulomuutto", "lahtomuutto"))
   dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"] <-
                     -dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"]

   # Absoluuttiset lahto- ja tulomuutot

   dat_muuttotiedot_kunnittain_mod %>%
        group_by(aluetyyppi, Vuosi, Tiedot) %>%
        summarize(muuttoja = sum(values)) %>%
        ungroup() %>%
        ggplot(aes(x = Vuosi, y = muuttoja, fill = Tiedot)) +
               geom_bar(stat = "identity", position = "stack") +
               facet_wrap(~aluetyyppi) +
     scale_y_continuous(labels  = deci_comma) +
     theme(legend.title = element_blank()) +
     scale_fill_manual(labels = c("Lähtömuutto", "Tulomuutto"),
                       values = c(ggptt_palettes$ptt[3], ggptt_palettes$ptt[1])) +
     labs(y = "Muuttoja",
          x = NULL,
          caption = "Lähde: PTT, Tilastokeskus",
          title = "Tulo- ja lähtömuuttoasteet",
          subtitle = "Tulo- ja lähtömuutot vuosittain")

     ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/lahtotulomuutto_atyyppi.png")

  # Lahto- ja tulomuuttoasteet

   dat_muuttotiedot_kunnittain_mod %>%
     group_by(aluetyyppi, Vuosi, Tiedot) %>%
     summarize(muuttoja = sum(values),
               vakea = sum(vakiluku)) %>%
     mutate(muuttoaste = muuttoja/vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = muuttoaste, fill = Tiedot)) +
            geom_bar(stat = "identity", position = "stack") +
            facet_wrap(~aluetyyppi) +
            scale_y_continuous(labels  = deci_comma) +
            theme(legend.title = element_blank()) +
            scale_fill_manual(labels = c("Lähtömuutto", "Tulomuutto"),
                              values = c(ggptt_palettes$ptt[3], ggptt_palettes$ptt[1])) +
            labs(y = "Muuttoaste",
                 x = NULL,
                 caption = "Lähde: PTT, Tilastokeskus",
                 title = "Tulo- ja lähtömuuttoasteet",
                 subtitle = "Tulo- ja lähtömuutot vuosittain suhteessa väkilukuun")

   ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/lahtotulomuuttoasteet_atyyppi.png")




############################## Roskakori ##############################################

# Mun mielestä näitä palkkikuvaajia on vaikeampi lukea kuin viivoja.

# Nettomuutot aluetyypeittäin, palkki
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(aluetyyppi, Vuosi) %>%
  summarize(nettomuutto = sum(values)) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuutto, fill = aluetyyppi)) +
  geom_bar(stat = "identity", position = "stack")  +
  scale_colour_manual(values = atyyppi_colour) +
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  theme(legend.title = element_blank()) +
  labs(x = NULL,
       y = "Nettomuuttoja",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuutot",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain")

ggsave("vignettes/nettomuutto_atyyppi_palkki.png")

# Nettomuutot kuntaryhmittäin, palkki
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(kuntaryhma, Vuosi) %>%
  summarize(nettomuutto = sum(values)) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuutto, fill = kuntaryhma)) +
  geom_bar(stat = "identity", position = "stack")  +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  labs(x = NULL,
       y = "Nettomuuttoja",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuutot",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain")

# Nettomuuttoasteet aluetyypeittäin, palkki
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(aluetyyppi, Vuosi) %>%
  summarize(nettomuutto = sum(values),
            vakea = sum(vakiluku)) %>%
  mutate(nettomuuttoaste = nettomuutto / vakea) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuuttoaste, fill = aluetyyppi)) +
  geom_bar(stat = "identity", position = "stack") +
  # geom_hline(yintercept = 0, color = "grey", size = 1) +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = deci_comma) +
  labs(x = NULL,
       y = "Nettomuuttoaste",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuuttoasteet",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain suhteessa asukaslukuun")

ggsave("vignettes/nettomuuttoaste_atyyppi_palkki.png")

# Nettomuuttoasteet kuntaryhmittäin, palkki
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(kuntaryhma, Vuosi) %>%
  summarize(nettomuutto = sum(values),
            vakea = sum(vakiluku)) %>%
  mutate(nettomuuttoaste = nettomuutto / vakea) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuuttoaste, fill = kuntaryhma)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = deci_comma) +
  labs(x = NULL,
       y = "Nettomuuttoaste",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuuttoasteet",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain suhteessa asukaslukuun")

# Aluetyyppien käyttäminen antaa tarkemman kuvan tilanteesta kuin kuntaryhmien käyttäminen.

# Nettomuutot kuntaryhmittäin, viiva
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(kuntaryhma, Vuosi) %>%
  summarize(nettomuutto = sum(values)) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuutto, color = kuntaryhma)) +
  geom_line() +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  labs(x = NULL,
       y = "Nettomuuttoja",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuutot",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain")

ggsave("analyysit/Muutto/Vaestonsiirtymavaikutukset/nettomuutto_kuntaryhma_viiva.png")

# Nettomuuttoasteet kuntaryhmittäin, viiva
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(kuntaryhma, Vuosi) %>%
  summarize(nettomuutto = sum(values),
            vakea = sum(vakiluku)) %>%
  mutate(nettomuuttoaste = nettomuutto / vakea) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuuttoaste, color = kuntaryhma)) +
  geom_line() +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = deci_comma) +
  labs(x = NULL,
       y = "Nettomuuttoaste",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuuttoasteet",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain suhteessa asukaslukuun")

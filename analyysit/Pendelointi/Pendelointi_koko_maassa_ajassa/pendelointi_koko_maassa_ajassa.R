# Pendelointi koko maassa ajassa

  library(tidyverse)
  library(ggplot2)
  library(ggptt)
  library(gridExtra)
  library(RColorBrewer)

  devtools::load_all()

  set_gg(theme_ptt(), "ptt")
  set_proj()

# Hae data

  data(dat_pendelointi)

  dat_pendelointi <- filter(dat_pendelointi, aluetyyppi != "Koko maa")

  # Pendelöijien absoluuttinen määrä koko maassa.
  # Huom. koko maan summia laskettaessa ei ole väliä summaileekko kohde- vai lähtöalueiden pendelöintejä.
  # Notes: pendelöinti määritelty kuntien välisenä pendelöintinä

  p1 <- dat_pendelointi %>%
    group_by(vuosi) %>%
    summarize(commuters = sum(lahtopendelointi)) %>%
    ungroup() %>%
    ggplot(aes(y = commuters, x = vuosi)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_line() +
    #ggtitle("Pendelöijien määrä Suomessa 1987 - 2015") + #Kuntien välinen pendelöinti
    ylab("Pendelöijien määrä") +
    xlab(NULL) +
    scale_y_continuous(limits = c(400000,800000), labels = deci_comma)

  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/absoluuttinen_maara.png", p1)

  # Pendelöijien osuus työllisistä koko maassa
  # Notes: pendelöinti määritelty kuntien välisenä pendelöintinä

  p2 <- dat_pendelointi %>%
    group_by(vuosi) %>%
    summarize(pendeloijat_yhteensa = sum(lahtopendelointi),
              ei_pendeloijat_yhteensa = sum(asuinkunnassaan_tyossakayvat)) %>%
    mutate(pendelointiosuus = pendeloijat_yhteensa /
             (pendeloijat_yhteensa + ei_pendeloijat_yhteensa)) %>%
    ungroup() %>%
    ggplot(aes(y = pendelointiosuus, x = vuosi)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_line() +
    #ggtitle("Pendelöijien osuus työllisistä Suomessa 1987 - 2015") +
    ylab("Pendelöijien osuus työllisistä") +
    xlab(NULL) +
    scale_y_continuous(limits = c(0.19,0.35), labels = percent_comma)

  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/osuus_tyollisista.png", p2)

  # Kuvaajat rinnakkain

  p3 <- grid.arrange(p1, p2, nrow = 1)
  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/molemmat.png", p3,
         width = 300,
         height = 100,
         units = "mm")


  ################## Pendelöinti kuntien, seutukuntien ja maakuntien välillä ##################

  # load("~/git_clones/KT162R/data/dat_muutto_aikasarja_km.rda")

  dat_pendelointi_alue <- dat_muutto_aikasarja_km %>%
                     filter(tiedot %in% c("kuntien_valinen_pendelointi",
                                          "seutukuntien_valinen_pendelointi",
                                          "maakuntien_valinen_pendelointi",
                                          "tyolliset"))

dat_pendelointi_alue %>%
    group_by(time) %>%
    mutate(osuus = 100 * value / value[tiedot == "tyolliset"]) %>%
    ungroup() %>%
    filter(tiedot != "tyolliset") %>%
    # filter(tiedot != "kuntien_valinen_pendelointi") %>%
    gather(vars, value, value, osuus) %>%
    ggplot(aes(y = value, x = time, col = tiedot)) +
    facet_wrap(vars(vars), scales = "free")+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_line() +
    #ggtitle("Pendelöijien määrä Suomessa 1987 - 2015") + #Kuntien välinen pendelöinti
    ylab("Pendelöijien määrä") +
    xlab(NULL) +
    scale_y_continuous( labels = deci_comma)

p1 <- dat_pendelointi_alue %>%
    filter(tiedot != "tyolliset") %>%
    group_by(tiedot) %>%
    ggplot(aes(y = value, x = time, col = tiedot)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_line() +
    #ggtitle("Pendelöijien määrä Suomessa 1987 - 2015") + #Kuntien välinen pendelöinti
    ylab("Pendelöijien määrä") +
    xlab(NULL) +
    scale_y_continuous( labels = deci_comma)


  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/absoluuttinen_maara.png", p1)

  p2 <- dat_pendelointi_alue %>%
    summarize(pendeloijat_yhteensa = sum(lahtopendelointi),
              ei_pendeloijat_yhteensa = sum(asuinkunnassaan_tyossakayvat)) %>%
    mutate(pendelointiosuus = pendeloijat_yhteensa /
             (pendeloijat_yhteensa + ei_pendeloijat_yhteensa)) %>%
    ggplot(aes(y = pendelointiosuus, x = vuosi)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_line() +
    #ggtitle("Pendelöijien osuus työllisistä Suomessa 1987 - 2015") +
    ylab("Pendelöijien osuus työllisistä") +
    xlab(NULL) +
    scale_y_continuous(limits = c(0.19,0.35), labels = percent_comma)



  ################# Aluetyyppien nettopendelöinti #######################################

  library(ggplot2)
  library(tidyverse)
  library(statfitools)
  library(ggptt)
  library(gridExtra)
  library(RColorBrewer)
  library(ggpubr)

  set_ptt()
  theme_update(plot.subtitle = element_text(colour = "grey50", size = 9))

  atyyppi_colour <- c(brewer.pal(6, "Blues")[3:6] , brewer.pal(6, "Greens")[4:6])



  # Load data

  data(dat_pendelointi)


  data(aluetyyppi)
  aluetyyppi <- rename(aluetyyppi, alue = kunta)
  dat_pendelointi <- left_join(aluetyyppi, dat_pendelointi, by = "alue")
  dat_pendelointi <- dat_pendelointi %>%
                     group_by(aluetyyppi, vuosi) %>%
                     summarize(tulopendelointi = sum(tulopendelointi, na.rm = TRUE),
                               lahtopendelointi = sum(lahtopendelointi, na.rm = TRUE),
                               asuinkunnassaan_tyossakayvat = sum(asuinkunnassaan_tyossakayvat)) %>%
                     mutate(nettopendelointi = tulopendelointi - lahtopendelointi,
                            nettopendelointiaste = (tulopendelointi- lahtopendelointi) /(asuinkunnassaan_tyossakayvat + lahtopendelointi)) %>%
                     gather(Tiedot, values, -aluetyyppi, -vuosi)

  dat_pendelointi <- filter(dat_pendelointi, aluetyyppi != "Koko maa")
  dat_pendelointi$aluetyyppi <- gdata::drop.levels(dat_pendelointi$aluetyyppi)

  dat_pendelointi$aluetyyppi <- fct_relevel(dat_pendelointi$aluetyyppi,
                                                        c("PK-seutu", "Muut yliopistokaupungit", "Muut työssäkäyntialueden keskukset", "Muut kaupungit",
                                                          "Kaupunkien läh. maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu"))
  aluetyyppi_labels = c("Pääkaupunkiseutu", "Yliopistokaupungit","Muut työssäkäyntikeskukset", "Kaupungit",
                        "Kaupunkien läheinen maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu")



  # nettopendelointi #########################################################################

  # Absoluuttiset määrät

  p1 <- dat_pendelointi %>%
    filter(Tiedot == "nettopendelointi") %>%
    ggplot(aes(x = vuosi, y = values, color = aluetyyppi)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = 2, size = 0.5) +
    scale_colour_manual(values = atyyppi_colour,
                        labels = aluetyyppi_labels) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left") +
    labs(x = NULL,
         y = "Nettopendelointi")



  # nettopendelointiasteet aluetyypeittäin, viiva
  p2 <-  dat_pendelointi %>%
    filter(Tiedot == "nettopendelointiaste") %>%
    ggplot(aes(x = vuosi, y = values, color = aluetyyppi)) +
    geom_line()  +
    scale_colour_manual(values = atyyppi_colour,
                       labels = aluetyyppi_labels) +
    geom_hline(yintercept = 0, color = "black", linetype = 2, size = 0.5) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.justification = "left") +
    scale_y_continuous(labels = percent_comma) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = NULL,
         y = "Nettopendelointiaste")


  p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
  ggsave("analyysit/Pendelointi/aluetyypeittain_molemmat.png", plot = p,
         height = 5,
         width = 8)


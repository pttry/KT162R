# Pendelointi koko maassa ajassa

  library(tidyverse)
  library(ggplot2)
  library(ggptt)
  library(gridExtra)

  set_gg(theme_ptt(), "ptt")

# Hae data

  data(dat_pendelointi)

  # Pendelöijien absoluuttinen määrä koko maassa.
  # Huom. koko maan summia laskettaessa ei ole väliä summaileekko kohde- vai lähtöalueiden pendelöintejä.
  # Notes: pendelöinti määritelty kuntien välisenä pendelöintinä

  p1 <- dat_pendelointi %>% group_by(vuosi) %>%
    summarize(commuters = sum(lahtopendelointi)) %>%
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

  p2 <- dat_pendelointi %>% group_by(vuosi) %>%
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

  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/osuus_tyollisista.png", p2)

  # Kuvaajat rinnakkain

  p3 <- grid.arrange(p1, p2, nrow = 1)
  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa/molemmat.png", p3,
         width = 300,
         height = 150,
         units = "mm")

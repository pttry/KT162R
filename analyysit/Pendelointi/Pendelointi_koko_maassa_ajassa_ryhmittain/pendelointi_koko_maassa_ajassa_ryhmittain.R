# Pendelöinti kokomaassa ajassa eri ryhmissa

  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggptt)
  library(statfitools)
  library(RColorBrewer)

  set_ptt(base_size = 18)
  theme_update(plot.subtitle = element_text(colour = "grey50", size = 12))

  atyyppi_colour <- c(brewer.pal(6, "Oranges")[6:4], brewer.pal(6, "Blues")[6:5] , brewer.pal(6, "Greens")[6:5])

  data(dat_pendelointi)

  #### Plot pendelöijien osuus työssäkäyvistä lähtöalueilla koko maassa vuosittain 18 - 64 vuotiaat ####
  #### ikäluokittain

  dat_pendelointi %>% filter(ika != "0 - 17", ika != "65 - 74" & ika != "75 -") %>%
    group_by(vuosi, ika) %>%
    summarize(pendelöijät_yhteensä = sum(tulopendelointi),
              ei_pendelöijät_yhteensä = sum(asuinkunnassaan_tyossakayvat)) %>%
    mutate(pendelöintiosuus = pendelöijät_yhteensä /
             (pendelöijät_yhteensä + ei_pendelöijät_yhteensä)) %>%
    ggplot(aes(y = pendelöintiosuus, x = vuosi, col = ika)) +
    geom_line() +
    theme(legend.title = element_blank()) +
    xlab(NULL) +
    ylab("Pendel?ijien osuus työllisistä") +
    theme(legend.position = "bottom", legend.justification = "left") +
    scale_y_continuous(limits = c(0.12, 0.4), labels = percent_comma) +
    scale_colour_manual(values = ggptt_palettes$ptt[1:6])

  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa_ryhmittain/pendelointi_koko_maassa_ajassa_ikaluokittain.png")

  #### Plot pendelöijien osuus työssäkäyvistä lähtöalueilla koko maassa vuosittain 18 - 64 vuotiaat ####
  #### koulutusasteittain

  dat_pendelointi %>% filter(ika != "0 - 17", ika != "65 - 74" & ika != "75 -") %>%
    group_by(vuosi, koulutusaste) %>%
    summarize(pendelöijät_yhteensä = sum(tulopendelointi),
              ei_pendelöijät_yhteensä = sum(asuinkunnassaan_tyossakayvat)) %>%
    mutate(pendelointiosuus = pendelöijät_yhteensä /
             (pendelöijät_yhteensä + ei_pendelöijät_yhteensä))%>%
    ggplot(aes(y = pendelointiosuus, x = vuosi, col = koulutusaste)) +
    geom_line() +
    labs(color = NULL) +
    xlab(NULL) +
    ylab("Pendelöijien osuus työllisistä") +
    theme(legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "vertical") +
    scale_y_continuous(labels = percent_comma) +
    scale_colour_manual(values = ggptt_palettes$ptt[1:5])

  ggsave("analyysit/Pendelointi/Pendelointi_koko_maassa_ajassa_ryhmittain/pendelointi_koko_maassa_ajassa_koulutus.png")

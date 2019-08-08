# Muuttaminen koko maassa ajassa

  library(tidyverse)
  library(ggplot2)
  library(ggptt)
  library(gridExtra)

  set_gg(theme_ptt(), "ptt")

# Hae data

  data(dat_kokonaismuutto)

# Muuttojen absoluuttinen määrä

 p1 <- dat_kokonaismuutto %>%
    ggplot(aes(x = vuosi, y = muuttoja, col = muuton_tyyppi)) +
           geom_line() +
           theme(legend.title = element_blank()) +
           ylab("Muuttoja") +
           xlab(NULL) +
           #ggtitle("Muuttojen määrä Suomessa 1990 - 2016") +
           theme(legend.position = "bottom", legend.justification = "left") +
           scale_y_continuous(labels = deci_comma,
                              breaks = scales::pretty_breaks(n = 6)) +
           scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
           scale_color_discrete(labels = c("Maakuntien väliset muutot",
                                           "Seutukuntien väliset muutot",
                                           "Kuntien väliset muutot",
                                           "Kuntien sisäiset muutot")) +
    theme(legend.text = element_text(size = 8, family = ),
          text = element_text(size = 10, family = "sans"))

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muutot90_17.png", p1)

# muuttojen suhteellinen määrä (muuttojen absoluuttinen määrä jaettuna väkiluvulla)

 p2 <- dat_kokonaismuutto %>%
   ggplot(aes(x = vuosi, y = muuttoaste, col = muuton_tyyppi)) +
    geom_line() +
    theme(legend.title = element_blank()) +
    ylab("Muuttoaste") + # Muuttojen määrä suhteessa väkilukuun
    xlab(NULL) +
    #ggtitle("Muuttoasteet Suomessa 1990 - 2016") +
    theme(legend.position = "bottom", legend.justification = "left") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    scale_color_discrete(labels = c("Maakuntien väliset muutot",
                                    "Seutukuntien väliset muutot",
                                    "Kuntien väliset muutot",
                                    "Kuntien sisäiset muutot")) +
    theme(legend.text = element_text(size = 8))

 ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muuttoasteet90_17.png", p2)

 p25 <-  dat_kokonaismuutto %>%
   ggplot(aes(x = vuosi, y = muuttoaste, col = muuton_tyyppi)) +
   geom_line() +
   theme(legend.title = element_blank()) +
   ylab("Muuttoaste") + # Muuttojen määrä suhteessa väkilukuun
   xlab(NULL) +
   #ggtitle("Muuttoasteet Suomessa 1990 - 2016") +
   theme(legend.position = "none") +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
   scale_color_discrete(labels = c("Maakuntien väliset muutot",
                                   "Seutukuntien väliset muutot",
                                   "Kuntien väliset muutot",
                                   "Kuntien sisäiset muutot")) +
   theme(legend.text = element_text(size = 8))




 # Kuvaajat rinnakkain

 p3 <- grid.arrange(p1, p25, nrow = 1)
 ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/molemmat.png", p3,
        width = 300,
        height = 150,
        units = "mm")

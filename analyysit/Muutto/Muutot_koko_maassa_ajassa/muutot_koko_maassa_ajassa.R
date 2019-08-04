# Muuttaminen koko maassa ajassa

  library(tidyverse)
  library(ggplot2)
  library(ggptt)
  library(gridExtra)

  set_gg(theme_ptt(), "ptt")

# Hae data

  data(dat_kokonaismuutto)

# Muuttojen absoluuttinen määrä

  dat_kokonaismuutto %>%
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
    theme(legend.text = element_text(size = 8))

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muutot90_17.png")

# muuttojen suhteellinen määrä (muuttojen absoluuttinen määrä jaettuna väkiluvulla)

 dat_kokonaismuutto %>%
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

 ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muuttoasteet90_17.png")

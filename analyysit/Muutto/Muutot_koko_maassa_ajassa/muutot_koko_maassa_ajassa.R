# Muuttaminen koko maassa ajassa

  library(tidyverse)
  library(ggplot2)
  library(ggptt)
  library(gridExtra)
  library(RColorBrewer)
  library(ggpubr)

  set_gg(theme_ptt(), "ptt")
  set_proj()

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
           geom_hline(yintercept = 0, color = "black", linetype = 2) +
           theme(legend.position = "bottom", legend.justification = "left") +
           scale_y_continuous(labels = deci_comma,
                              breaks = scales::pretty_breaks(n = 6)) +
           scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
   scale_color_manual(labels = c("Maakuntien väliset muutot",
                                 "Seutukuntien väliset muutot",
                                 "Kuntien väliset muutot",
                                 "Kuntien sisäiset muutot"),
                      values = brewer.pal(5, "Blues")[2:5]) +
    theme(legend.text = element_text(size = 15, family = "sans" ),
          text = element_text(size = 15, family = "sans"))

  ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muutot90_17.png", p1)

# muuttojen suhteellinen määrä (muuttojen absoluuttinen määrä jaettuna väkiluvulla)

 p2 <- dat_kokonaismuutto %>%
   ggplot(aes(x = vuosi, y = muuttoaste, col = muuton_tyyppi)) +
    geom_line() +
    theme(legend.title = element_blank()) +
    ylab("Muuttoaste") + # Muuttojen määrä suhteessa väkilukuun
    xlab(NULL) +
    #ggtitle("Muuttoasteet Suomessa 1990 - 2016") +
    geom_hline(yintercept = 0, color = "black", linetype = 2) +
    theme(legend.position = "bottom", legend.justification = "left") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    scale_y_continuous(labels = percent_comma) +
    scale_color_manual(labels = c("Maakuntien väliset muutot",
                                    "Seutukuntien väliset muutot",
                                    "Kuntien väliset muutot",
                                    "Kuntien sisäiset muutot"),
                       values = brewer.pal(5, "Blues")[2:5]) +
   theme(legend.text = element_text(size = 15, family = "sans" ),
         text = element_text(size = 15, family = "sans"))

 ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/muuttoasteet90_17.png", p2)


 # Kuvaajat rinnakkain

p3 <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
 ggsave("analyysit/Muutto/Muutot_koko_maassa_ajassa/molemmat.png", p3,
        width = 300,
        height = 150,
        units = "mm")

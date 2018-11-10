# Kuvaajat

set_gg(theme_ptt(), "ptt")

####### Plot muuttojen absoluuttinen määrä ########################################

# Load data
   muuttoaste_data <- readRDS("R/data_clean/muuttoaste_data.rds")
   # Data acquired and prepared in the file R/data_raw/muuttodata_prep.R

# Draw plot
   ggplot(muuttoaste_data, aes(x = vuosi, y = muuttoja, col = muuton_tyyppi)) +
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

# Save plot

####### Plot muuttojen suhteellinen määrä (muuttojen absoluuttinen määrä jaettuna väkiluvulla)  #####

# Load data
   muuttoaste_data <- readRDS("R/data_clean/muuttoaste_data.rds")
   # Data acquired and prepared in the file R/data_raw/muuttodata_prep.R

# Draw plot
   ggplot(muuttoaste_data, aes(x = vuosi, y = muuttoaste, col = muuton_tyyppi)) +
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

# Save plot

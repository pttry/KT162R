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


###### Plot pendelelöijien absoluuttinen määrä koko maassa ################################

   pendelointi_data <- readRDS("R/data_clean/pendelointi_data.rds")
   # Data muokattu Tilastokeskuksen Datasta scriptissä "Pendelöintiaineistojen siistintä"
   # kansiossa Kuvaajien R koodit/Datan siistintä

   # Huom. Koko maan summia laskettaessa ei ole väliä summaileeko kohde- vai lähtöalueiden
   # pendelöintejä

   pendelointi_data %>% group_by(vuosi) %>%
     summarize(commuters = sum(lahtopendelointi)) %>%
     ggplot(aes(y = commuters, x = vuosi)) +
     geom_line() +
     #ggtitle("Pendelöijien määrä Suomessa 1987 - 2015") + #Kuntien välinen pendelöinti
     ylab("Pendelöijien määrä") +
     xlab(NULL) +
     scale_y_continuous(limits = c(400000,800000), labels = deci_comma)

##### Plot pendelöijien osuus työllisistä koko suomessa ##################################

   pendelointi_data %>% group_by(vuosi) %>%
     summarize(pendeloijat_yhteensa = sum(lahtopendelointi),
               ei_pendeloijat_yhteensa = sum(asuinkunnassaan_tyossakayvat)) %>%
     mutate(pendelointiosuus = pendeloijat_yhteensa /
              (pendeloijat_yhteensa + ei_pendeloijat_yhteensa)) %>%
     ggplot(aes(y = pendelointiosuus, x = vuosi)) +
     geom_line() +
     #ggtitle("Pendelöijien osuus työllisistä Suomessa 1987 - 2015") +
     ylab("Pendelöijien osuus työllisistä") +
     xlab(NULL) +
     scale_y_continuous(limits = c(0.19,0.35), labels = percent_comma)

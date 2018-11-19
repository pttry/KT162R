# Kuvaajat

set_gg(theme_ptt(), "ptt")

####### Plot muuttojen absoluuttinen määrä ########################################

# Load data
muuttoaste_data <- readRDS("data/muuttoaste_data.rds")
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
ggsave("vignettes/muutot90_17.png")

####### Plot muuttojen suhteellinen määrä (muuttojen absoluuttinen määrä jaettuna väkiluvulla)  #####

# Load data
muuttoaste_data <- readRDS("data/muuttoaste_data.rds")
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
ggsave("vignettes/muuttoasteet90_17.png")


###### Plot pendelelöijien absoluuttinen määrä koko maassa ################################

pendelointi_data <- readRDS("data/pendelointi_data.rds")
# Data muokattu Tilastokeskuksen Datasta scriptissä "Pendelöintiaineistojen siistintä"
# kansiossa Kuvaajien R koodit/Datan siistintä

# Huom. Koko maan summia laskettaessa ei ole väliä summaileeko kohde- vai lähtöalueiden
# pendelöintejä

pendelointi_data %>% group_by(vuosi) %>%
  summarize(commuters = sum(lahtopendelointi)) %>%
  ggplot(aes(y = commuters, x = vuosi)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
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
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  geom_line() +
  #ggtitle("Pendelöijien osuus työllisistä Suomessa 1987 - 2015") +
  ylab("Pendelöijien osuus työllisistä") +
  xlab(NULL) +
  scale_y_continuous(limits = c(0.19,0.35), labels = percent_comma)

# Nettomuuttokertymä barplot seutukunnittain

# Hae data
  dat_muuttotiedot_kunnittain <- readRDS("data/dat_muuttotiedot_kunnittain.rds")
  nettomuutto_data <- filter(dat_muuttotiedot_kunnittain, Tiedot == "nettomuutto")

nettomuutto_data %>% group_by(seutukunta) %>%
  summarize(nettomuutto = sum(values)) %>%
  ggplot(aes(x = reorder(seutukunta, nettomuutto), y = nettomuutto)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab(NULL) +
  ylab("Nettomuutto") +
  scale_y_continuous(limits = c(-50000, 140000), labels = deci_comma)

####### KUNTIEN VÄLINEN PENDELÖINTI JA KUNTIEN VÄLINEN MUUTTO SUHTEESSA VÄKILUKUUN #############

# Hae data
load("data/dat_lahtopendelointi.rda")
dat_lahtopendelointi <- dat_lahtopendelointi %>%
                        mutate(Vuosi = as.integer(time)) %>%
                        filter(alue == "KOKO MAA",
                               koulutusaste == "Yhteensä",
                               ika == "Kaikki ikäluokat",
                               pendelointi == "Pendelöivät")
muutot <- readRDS("data/muuttoaste_data.rds")
vakiluku <- readRDS("data/vakiluku.rds")

kuntien_valiset_muutot <- filter(muutot, muuton_tyyppi == "kuntien valiset muutot") %>%
  rename(Vuosi = vuosi) %>%
  filter(Vuosi %in% 1990:2016) %>%
  select(Vuosi, muuttoja, vakiluku)

  pendelointi_ja_muutto <- left_join(kuntien_valiset_muutot,
                                   dat_lahtopendelointi,
                                   by = "Vuosi")
pendelointi_ja_muutto <- pendelointi_ja_muutto %>%
  mutate(Kuntien_valinen_pendelointi = values / vakiluku) %>%
  mutate(Kuntien_valinen_muutto = muuttoja / vakiluku) %>%
  select(Vuosi, Kuntien_valinen_pendelointi, Kuntien_valinen_muutto) %>%
  gather(tyyppi, values, c("Kuntien_valinen_pendelointi", "Kuntien_valinen_muutto"))

pendelointi_ja_muutto %>% ggplot(aes(x = Vuosi, y = values, colour = tyyppi)) +
  geom_line() +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(labels = percent_comma) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_blank()) +
  scale_color_discrete(labels = c("Kuntien välinen muutto",
                                  "Kuntien välinen pendelöinti"))

ggsave(filename = "Kuntien välinen pendelöinti ja kuntien välinen muutto.jpeg",
       path = path,
       device = "jpeg",
       width = 17,
       height = 10,
       units = "cm")


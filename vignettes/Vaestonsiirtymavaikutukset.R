# Väestonmuutosvaikutukset aluetyypeittäin

# Load data
dat_muuttotiedot_kunnittain <- readRDS("data/dat_muuttotiedot_kunnittain.rds")

set_ptt()
theme_update(plot.subtitle = element_text(colour = "grey50", size = 9))

atyyppi_colour <- c(brewer.pal(6, "Oranges")[6:4], brewer.pal(6, "Blues")[6:5] , brewer.pal(6, "Greens")[6:5])


# Nettomuutot aluetyypeittäin, viiva
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(aluetyyppi, Vuosi) %>%
  summarize(nettomuutto = sum(values)) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuutto, color = aluetyyppi)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  scale_colour_manual(values = atyyppi_colour) +
  theme(legend.title = element_blank()) +
  labs(x = NULL,
       y = "Nettomuuttoja",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuutot",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain")

ggsave("vignettes/nettomuutto_atyyppi_viiva.png")

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

# Nettomuuttoasteet aluetyypeittäin, viiva
dat_muuttotiedot_kunnittain %>%
  filter(Tiedot == "nettomuutto") %>%
  group_by(aluetyyppi, Vuosi) %>%
  summarize(nettomuutto = sum(values),
            vakea = sum(vakiluku)) %>%
  mutate(nettomuuttoaste = nettomuutto / vakea) %>%
  ungroup() %>%
  ggplot(aes(x = Vuosi, y = nettomuuttoaste, color = aluetyyppi)) +
  geom_line()  +
  scale_colour_manual(values = atyyppi_colour) +
  geom_hline(yintercept = 0, color = "grey", size = 1) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = deci_comma) +
  labs(x = NULL,
       y = "Nettomuuttoaste",
       caption = "Lähde: PTT, Tilastokeskus",
       title = "Nettomuuttoasteet",
       subtitle = "Tulo- ja lähtömuuttojen erotus vuosittain suhteessa asukaslukuun")

ggsave("vignettes/nettomuuttoaste_atyyppi_viiva.png")

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
  geom_hline(yintercept = 0, color = "grey", size = 1) +
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

# Lähto- ja tulomuutot aluetyypeittäin

# Muuta lähtömuuttojen arvot negatiivisiksi
dat_muuttotiedot_kunnittain_mod <- filter(dat_muuttotiedot_kunnittain, Tiedot %in% c("tulomuutto", "lahtomuutto"))
dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"] <-
  -dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"]

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

ggsave("vignettes/lahtotulomuutto_atyyppi.png")

# Lahto- ja tulomuuttoasteet aluetyypeittäin
dat_muuttotiedot_kunnittain_mod <- filter(dat_muuttotiedot_kunnittain, Tiedot %in% c("tulomuutto", "lahtomuutto"))
dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"] <-
  -dat_muuttotiedot_kunnittain_mod$values[dat_muuttotiedot_kunnittain_mod$Tiedot == "lahtomuutto"]

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

ggsave("vignettes/lahtotulomuuttoasteet_atyyppi.png")

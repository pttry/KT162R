# Väestonmuutosvaikutukset aluetyypeittäin

# Load data
  dat_muuttotiedot_kunnittain <- readRDS("data/dat_muuttotiedot_kunnittain.rds")

# Nettomuutot aluetyypeittäin, viiva
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values)) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuutto, color = aluetyyppi)) +
            geom_line()

# Nettomuutot kuntaryhmittäin, viiva
   dat_muuttotiedot_kunnittain %>%
      filter(Tiedot == "nettomuutto") %>%
      group_by(kuntaryhma, Vuosi) %>%
      summarize(nettomuutto = sum(values)) %>%
      ungroup() %>%
      ggplot(aes(x = Vuosi, y = nettomuutto, color = kuntaryhma)) +
             geom_line()

# Nettomuutot aluetyypeittäin, palkki
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values)) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuutto, fill = aluetyyppi)) +
            geom_bar(stat = "identity", position = "stack")

# Nettomuutot kuntaryhmittäin, palkki
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(kuntaryhma, Vuosi) %>%
     summarize(nettomuutto = sum(values)) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuutto, fill = kuntaryhma)) +
           geom_bar(stat = "identity", position = "stack")

# Nettomuuttoasteet aluetyypeittäin, viiva
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values),
               vakea = sum(vakiluku)) %>%
     mutate(nettomuuttoaste = nettomuutto / vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuuttoaste, color = aluetyyppi)) +
     geom_line()

# Nettomuuttoasteet kuntaryhmittäin, viiva
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(kuntaryhma, Vuosi) %>%
     summarize(nettomuutto = sum(values),
               vakea = sum(vakiluku)) %>%
     mutate(nettomuuttoaste = nettomuutto / vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuuttoaste, color = kuntaryhma)) +
     geom_line()

   # Nettomuuttoasteet aluetyypeittäin, palkki
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(aluetyyppi, Vuosi) %>%
     summarize(nettomuutto = sum(values),
               vakea = sum(vakiluku)) %>%
     mutate(nettomuuttoaste = nettomuutto / vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuuttoaste, fill = aluetyyppi)) +
     geom_bar(stat = "identity", position = "stack")

   # Nettomuuttoasteet kuntaryhmittäin, palkki
   dat_muuttotiedot_kunnittain %>%
     filter(Tiedot == "nettomuutto") %>%
     group_by(kuntaryhma, Vuosi) %>%
     summarize(nettomuutto = sum(values),
               vakea = sum(vakiluku)) %>%
     mutate(nettomuuttoaste = nettomuutto / vakea) %>%
     ungroup() %>%
     ggplot(aes(x = Vuosi, y = nettomuuttoaste, fill = kuntaryhma)) +
     geom_bar(stat = "identity", position = "stack")

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
               facet_wrap(~aluetyyppi)

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
     facet_wrap(~aluetyyppi)

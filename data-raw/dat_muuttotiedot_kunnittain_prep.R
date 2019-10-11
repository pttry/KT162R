# Muuttodata kunnittain, aluetyyppitiedot ja väkilukutiedot mukaan. Muuttoasteiden laskenta.

# Get the data
dat_muuttotiedot_kunnittain <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
                 dims = list(Alue = c('*'),
                             Vuosi = c('*'),
                             Sukupuoli = c('SSS'),
                             Ikä = c('SSS'),
                             Tiedot = c('*')),
                 clean = TRUE)

# Change types of Alue and Vuosi and names in Tiedot
   dat_muuttotiedot_kunnittain$Alue <- as.character(dat_muuttotiedot_kunnittain$Alue)
   dat_muuttotiedot_kunnittain$Vuosi <- as.integer(as.character(dat_muuttotiedot_kunnittain$Vuosi))
   levels(dat_muuttotiedot_kunnittain$Tiedot) <- c("lahtomuutto", "nettomuutto", "tulomuutto")

# Change name of Maarianhamina
   dat_muuttotiedot_kunnittain$Alue[dat_muuttotiedot_kunnittain$Alue == "Maarianhamina - Mariehamn"] <- "Maarianhamina"

# Add seutukunta, maakunta and kuntaryhma to the data
  dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain %>%
     filter(Alue != "KOKO MAA") %>%
                        select(-Ikä, -Sukupuoli) %>%
                        mutate(seutukunta = kuntaluokka(Alue, "Seutukunta")) %>%
                        mutate(maakunta = kuntaluokka(Alue, "Maakunta")) %>%
                        mutate(kuntaryhma = kuntaluokka(Alue, "Kuntaryhma"))

# Load aluetyypit and väkilukutiedot
   load("data/aluetyyppi.rda")
   dat_kuntien_vakiluvut <- readRDS("data/dat_kuntien_vakiluvut.rds")

# Add aluetyypit to the data
   dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain %>%
                         rename(kunta = Alue) %>%
                         left_join(aluetyyppi, by = "kunta")

# Add väkilukutiedot to the data
   dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain %>%
     rename(alue = kunta) %>%
     left_join(dat_kuntien_vakiluvut, by = c("alue", "Vuosi"))

# Compute muuttoasteet
   dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain %>%
                         spread(Tiedot, values) %>%
                         mutate(tulomuuttoaste = tulomuutto / vakiluku,
                                lahtomuuttoaste = lahtomuutto / vakiluku,
                                nettomuuttoaste = nettomuutto / vakiluku) %>%
                         gather(Tiedot, values, c("tulomuutto", "lahtomuutto", "nettomuutto",
                                                  "tulomuuttoaste", "lahtomuuttoaste", "nettomuuttoaste"))

# Save data
   usethis::use_data(dat_muuttotiedot_kunnittain, overwrite = TRUE)


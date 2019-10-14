# Muuttodata kunnittain, aluetyyppitiedot ja väkilukutiedot mukaan. Muuttoasteiden laskenta.

library(pxweb)
library(tidyverse)
devtools::load_all()

# Load aluetyypit and väkilukutiedot
load("data/aluetyyppi.rda")
load("data/dat_kuntien_vakiluvut.rda")


# Get the data
dat_muuttotiedot_kunnittain0 <-
   get_pxweb_data(
      url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
      dims = list(
         Alue = c('*'),
         Vuosi = c('*'),
         Sukupuoli = c('SSS'),
         Ikä  = c('SSS'),
         Tiedot = c('*')
      ),
      clean = TRUE
   )

# Change types of Alue and Vuosi and names in Tiedot
dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain0 %>%
   mutate(
      Alue = as.character(Alue),
      Vuosi = as.integer(as.character(Vuosi)),
      Tiedot = fct_recode(Tiedot,
         tulomuutto = "Kuntien välinen tulomuutto",
         lahtomuutto = "Kuntien välinen lähtömuutto",
         nettomuutto = "Kuntien välinen nettomuutto")) %>%
   filter(Alue != "KOKO MAA") %>%
   select(-Ikä, -Sukupuoli) %>%
   rename(kunta = Alue) %>%
   left_join(aluetyyppi, by = "kunta") %>%
   rename(alue = kunta) %>%
   left_join(dat_kuntien_vakiluvut, by = c("alue", "Vuosi")) %>%
   mutate(alue = fct_recode(alue, Maarianhamina = "Maarianhamina - Mariehamn")) %>%
   mutate(seutukunta = kuntaluokka(alue, "Seutukunta")) %>%
   mutate(maakunta = kuntaluokka(alue, "Maakunta")) %>%
   mutate(kuntaryhma = kuntaluokka(alue, "Kuntaryhma")) %>%
   spread(Tiedot, values) %>%
   mutate(
      tulomuuttoaste = tulomuutto / vakiluku,
      lahtomuuttoaste = lahtomuutto / vakiluku,
      nettomuuttoaste = nettomuutto / vakiluku
   ) %>%
   gather(Tiedot, values, c("tulomuutto", "lahtomuutto", "nettomuutto",
                            "tulomuuttoaste", "lahtomuuttoaste", "nettomuuttoaste")
   )


# Save data
   usethis::use_data(dat_muuttotiedot_kunnittain, overwrite = TRUE)


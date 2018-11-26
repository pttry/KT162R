
# Ossilta saatu ruutuaineistosta laskettu


# Työmatkat asuinpaikan mukaan
library(tidyverse)
library(ggptt)

dat_matka_asuinpaikka_aluokka <- haven::read_sav("data-raw/AsuinKpiMaasAggregate1990_2015.sav") %>%
  mutate(time = readr::parse_number(vuosi),
         aluokka = factor(LuokkaKaupMaas, levels = 1:7,
                          labels = c("Sisempi kaupunkialue",
                                     "Ulompi kaupunkialue",
                                     "Kaupungin kehysalue",
                                     "Maaseudun paikalliskeskukset",
                                     "Kaupungin läheinen maaseutu",
                                     "Ydinmaaseutu",
                                     "Harvaan asuttu maaseutu")),
         tyomatka_asuin = as.numeric(Mean_Väestökm)) %>%
  select(time, aluokka, tyomatka_asuin)

dat_matka_tyopaikka_aluokka <- haven::read_sav("data-raw/TyöKpiMaasAggregate1990_2015.sav") %>%
  mutate(time = readr::parse_number(Vuosi),
         aluokka = factor(LuokkaKaupMaas, levels = 1:7,
                          labels = c("Sisempi kaupunkialue",
                                     "Ulompi kaupunkialue",
                                     "Kaupungin kehysalue",
                                     "Maaseudun paikalliskeskukset",
                                     "Kaupungin läheinen maaseutu",
                                     "Ydinmaaseutu",
                                     "Harvaan asuttu maaseutu")),
         tyomatka_tyo = as.numeric(Mean_Väestökm)) %>%
  select(time, aluokka, tyomatka_tyo)

dat_matkat_aluokka <-
  dat_matka_asuinpaikka_aluokka %>%
  left_join(dat_matka_tyopaikka_aluokka, by = c("time", "aluokka"))


usethis::use_data(dat_matkat_aluokka, overwrite = TRUE)


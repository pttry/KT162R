

## Aluetyppiluokitus

# Maaseutualueiden osalta luokitus täältä: https://www.tilastokeskus.fi/tup/msind/msindaluetyypit.html
# Lisäksi työssäkäyntialueen keskus, yliopistokeskus, ja pääkaupunkiseutus

library(tidyverse)

aluetyyppi <- readxl::read_xls("data-raw/msindaluejaot10.xls") %>%
  mutate(kunta_no = as_factor(statfitools::extract_code(Kunta, numbers_as_numeric = FALSE)),
         kunta = as_factor(statfitools::extract_name(Kunta)),
         aluetyyppi = coalesce(luokka2, Alueluokka)) %>%
  select(kunta_no, kunta, aluetyyppi) %>%
  mutate(aluetyyppi = fct_relevel(aluetyyppi,
                                  rev(c("Harvaan asuttu maaseutu", "Ydinmaaseutu", "Kaupunkien läh. maaseutu",
                                  "Kaupungit", "TKA", "YO", "PK seutu")))) %>%
  mutate(aluetyyppi = fct_recode(aluetyyppi,
                                 'Muut kaupungit' = "Kaupungit",
<<<<<<< HEAD
                                 'Muut työssäkäyntialueiden keskukset' = "TKA",
=======
                                 'Muut keskukset' = "TKA",
>>>>>>> b63e37969fe45f9433900acf631e3407fd42204a
                                 'Muut yliopistokaupungit' = "YO",
                                 'PK-seutu' = "PK seutu"))


use_data(aluetyyppi, overwrite = TRUE)

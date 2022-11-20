# Väkilukutiedot

library(pxweb)
library(tidyverse)
library(statfitools)

dat_kuntien_vakiluvut0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px",
                 dims = list(Alue = c('*'),
                             Ikä = c('*'),
                             Sukupuoli = c('SSS'),
                             Vuosi = c('*'),
                             Tiedot = c('vaesto')),
                 clean = TRUE)



# Rename values and remove redundant columns
dat_kuntien_vakiluvut <- dat_kuntien_vakiluvut0 %>%
  clean_times() %>%
  clean_names(to_lower = TRUE) %>%
  group_by(time, alue) %>%
  summarise(vakiluku = values[ika == "Yhteensä"],
            vaki_15_64 = sum(values[ika %in% as.character(15:64)])) %>%
  ungroup() %>%
  mutate(Vuosi = time)



# Population with age groups
dat_kuntien_vaki_ika9 <- dat_kuntien_vakiluvut0 %>%
  clean_times() %>%
  clean_names(to_lower = TRUE) %>%
  mutate(ika = fct_relabel(ika, group_age9)) %>%
  group_by(time, alue, ika) %>%
  summarise(values = sum(values)) %>%
  ungroup()

# for regional type
dat_at_vaki_ika9 <- dat_kuntien_vaki_ika9 %>%
  left_join(rename(aluetyyppi, alue = "kunta"), by = "alue") %>%
  group_by(time, aluetyyppi, ika) %>%
  summarise(values = sum(values)) %>%
  ungroup()



# Save data
usethis::use_data(dat_kuntien_vakiluvut, dat_at_vaki_ika9, overwrite = TRUE)

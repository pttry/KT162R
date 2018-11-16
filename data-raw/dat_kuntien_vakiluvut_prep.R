# Väkilukutiedot

library(pxweb)
library(tidyverse)
library(statfitools)

dat_kuntien_vakiluvut0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_004.px",
                 dims = list(Alue = c('*'),
                             Ikä = c('*'),
                             Sukupuoli = c('S'),
                             Vuosi = c('*'),
                             Tiedot = c('lkm')),
                 clean = TRUE) %>%
  clean_times() %>%
  clean_names(to_lower = TRUE)


# Rename values and remove redundant columns
dat_kuntien_vakiluvut <- dat_kuntien_vakiluvut0 %>%
  group_by(time, alue) %>%
  summarise(vakiluku = values[ika == "Ikäluokat yhteensä"],
            vaki_15_64 = sum(values[ika %in% as.character(15:64)])) %>%
  ungroup() %>%
  mutate(Vuosi = time)


# Save data
use_data(dat_kuntien_vakiluvut, overwrite = TRUE)

# Kuntien sisäinen muutto

kuntien_sisaiset_muutot0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a3.px",
                 dims = list(Sukupuoli = c('SSS'),
                             Ikä = c('SSS'),
                             Vuosi = c('*'),
                             Tiedot = c('vm44')),
                 clean = TRUE)

kuntien_sisaiset_muutot <- kuntien_sisaiset_muutot0 %>%
   select(-Sukupuoli, -Ikä, -Tiedot) %>%
   mutate(Vuosi = as.integer(as.character(Vuosi)))


# Save data
   saveRDS(kuntien_sisaiset_muutot, file = "data/kuntien_sisaiset_muutot.rds")

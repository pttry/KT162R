# Suomen väkilukutiedot 1990-

dat_vakiluku0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_11rd.px",
                 dims = list(Vuosi = c('*'),
                             Sukupuoli = c('SSS'),
                             "Ikä" = c('SSS'),
                             Tiedot = c('vaesto')),
                 clean = TRUE)

dat_vakiluku <- dat_vakiluku0 %>%
   select(Vuosi, vakiluku = values) %>%
   mutate(Vuosi = as.integer(as.character(Vuosi)))


# Save data
   usethis::use_data(dat_vakiluku, overwrite = TRUE)

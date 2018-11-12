# Kuntien sisäinen muutto

data0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a3.px",
                 dims = list(Sukupuoli = c('SSS'),
                             Ikä = c('SSS'),
                             Vuosi = c('*'),
                             Tiedot = c('vm44')),
                 clean = TRUE)

data <- data0

# Remove redundant columns
   data <- data %>% select(-Sukupuoli, -Ikä, -Tiedot)

# Change the type of Vuosi column
   data$Vuosi <- as.integer(as.character(data$Vuosi))

# Name data set
   kuntien_sisaiset_muutot <- data

# Save data
   saveRDS(kuntien_sisaiset_muutot, file = "R/data_clean/kuntien_sisaiset_muutot.rds")

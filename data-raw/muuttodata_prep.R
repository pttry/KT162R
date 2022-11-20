# Muuttojen määrät ja muuttoasteet 1990-2017

library(pxweb)
library(stringr)

data0 <-
   get_pxweb_data(
      url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a1.px",
      dims = list(
         Tuloalue = c('*'),
         Lähtöalue = c('*'),
         Sukupuoli = c('SSS'),
         Vuosi = c('*'),
         Tiedot = c('*')
      ),
      clean = TRUE
   )

data <- data0 %>%
   # Rename variables
   rename(Lahtokunta = Lähtöalue,
          Tulokunta = Tuloalue) %>%
   mutate(
      Tulokunta = str_extract(Tulokunta, "(?<= - ).*"),
      Tulokunta = gsub("Maarianhamina - Mariehamn", "Maarianhamina", Tulokunta),
      Lahtokunta = str_extract(Lahtokunta, "(?<= - ).*"),
      Lahtokunta = gsub("Maarianhamina - Mariehamn", "Maarianhamina", Lahtokunta),
      Vuosi = as.integer(as.character(Vuosi))
   ) %>%
   filter(!is.na(Tulokunta),!is.na(Lahtokunta))

data2 <- data %>%
   mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta")) %>%
   mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

# # Change type of Tulokunta ja Lahtokunta to character
#    data$Tulokunta <- as.character(data$Tulokunta)
#    data$Lahtokunta <- as.character(data$Lahtokunta)
#
# # Remove redundant prefixes from variables Tulokunta and Lahtokunta
#    tulokunnat <- sapply(sapply(data$Tulokunta, strsplit, " - "), '[', 2)
#    lahtokunnat <- sapply(sapply(data$Lahtokunta, strsplit, " - "), '[', 2)
#    names(tulokunnat) <- NULL
#    names(lahtokunnat) <- NULL
#    data$Tulokunta <- tulokunnat
#    data$Lahtokunta <- lahtokunnat
#
# # Change type of Vuosi to integer
#    data$Vuosi <- as.integer(as.character(data$Vuosi))

# Add an indicator telling whether the migration occurs between maakunnat of seutukunnat, takes for a while.
# Memory problems may occur.
# data <- data %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
# data <- data %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

# Alternatively, process data in chunks
data1 <- data[1:500000, ]
data2 <- data[500001:1000000, ]
data3 <- data[1000001:1500000, ]
data4 <- data[1500001:2000000, ]
data5 <- data[2000001:dim(data)[1],]

data1 <-
   data1 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
data1 <-
   data1 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

data2 <-
   data2 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
data2 <-
   data2 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

data3 <-
   data3 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
data3 <-
   data3 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

data4 <-
   data4 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
data4 <-
   data4 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

data5 <-
   data5 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
data5 <-
   data5 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

data <- rbind(data1, data2, data3, data4, data5)


# Name data set
muuttodata <- data2

# Save data
saveRDS(muuttodata, file = "data/muuttodata.rds")
# muuttodata2 <- readRDS(file = "data/muuttodata.rds")

# Compute all yearly migration between maakunnat, seutukunnat and kunnat

# Load väkilukutiedot
data(dat_vakiluku)

maakuntien_valiset_muutot_vuosittain <- data2 %>%
   group_by(Vuosi) %>%
   summarize(muuttoja = sum(values * maakuntien_valinen_muutto)) %>%
   ungroup() %>%
   left_join(dat_vakiluku, by = "Vuosi") %>%
   mutate(muuttoaste = muuttoja / vakiluku)

seutukuntien_valiset_muutot_vuosittain <- data2 %>%
   group_by(Vuosi) %>%
   summarize(muuttoja = sum(values * seutukuntien_valinen_muutto)) %>%
   ungroup() %>%
   left_join(dat_vakiluku, by = "Vuosi") %>%
   mutate(muuttoaste = muuttoja / vakiluku)

kuntien_valiset_muutot_vuosittain <- data2 %>%
   group_by(Vuosi) %>%
   summarize(muuttoja = sum(values)) %>%
   ungroup() %>%
   left_join(dat_vakiluku, by = "Vuosi") %>%
   mutate(muuttoaste = muuttoja / vakiluku)

# Load kuntien sisäiset muutot
kuntien_sisaiset_muutot_vuosittain <-
   readRDS("data/kuntien_sisaiset_muutot.rds") %>%
   rename(muuttoja = "values") %>%
   left_join(dat_vakiluku, by = "Vuosi") %>%
   mutate(muuttoaste = muuttoja / vakiluku)


dat_kokonaismuutto <- bind_rows(
   "maakuntien valiset muutot" = maakuntien_valiset_muutot_vuosittain,
   "seutukuntien valiset muutot" = seutukuntien_valiset_muutot_vuosittain,
   "kuntien valiset muutot" = kuntien_valiset_muutot_vuosittain,
   "kuntien sisaiset muutot" = kuntien_sisaiset_muutot_vuosittain,
   .id = "muuton_tyyppi"
) %>%
   mutate(muuton_tyyppi = as_factor(muuton_tyyppi)) %>%
   rename(vuosi = "Vuosi")


# Save data
usethis::use_data(dat_kokonaismuutto, overwrite = TRUE)

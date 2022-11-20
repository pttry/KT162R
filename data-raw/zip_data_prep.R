# Postinumeroaluetiedot

library(pxweb)
library(tidyverse)
library(statfitools)

  aineistokoodit <- c("1_he_", "2_ko_", "3_hr_", "4_te_", "5_tr_", "6_ra_", "7_tp_", "8_pt_")
  julkaisuvuodet <- 2015:2019

for(julkaisuvuosi in julkaisuvuodet) {

  for(aineistokoodi in aineistokoodit) {

       url <- paste("http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/",
                 as.character(julkaisuvuosi),
                 "/paavo_",
                 aineistokoodi,
                 as.character(julkaisuvuosi),
                 ".px", sep = "")

       zip_data <-
         get_pxweb_data(url = url,
                        dims = list(Postinumeroalue = c('*'),
                                    Tiedot = c('*')),
                        clean = TRUE)

    # Look for the year in the data
       x <- as.character(zip_data[[2]][1])
       year <- as.double(substring(x,nchar(as.character(x))-8, nchar(as.character(x))-5))


       names(zip_data) <- c("alue", "tiedot", "value")
       tiedot <- sapply(zip_data$tiedot, function(x) {substring(x,1,nchar(as.character(x))-11)})
       alue <- sapply(zip_data$alue, function(x) {substring(x,1,5)})
       zip_data$tiedot <- tiedot
       zip_data$alue <- alue
       zip_data <- filter(zip_data, !grepl("KOKO", alue))
       zip_data <- zip_data %>% spread(tiedot, value) %>% clean_names()
       zip_data$vuosi <- year

       filename <- paste("data/zip_data/zip_data_", aineistokoodi, as.character(year), ".rds", sep = "")
       saveRDS(zip_data, filename)
  }

  Sys.sleep(0.01)
  print(paste(julkaisuvuosi, "/", julkaisuvuodet[length(julkaisuvuodet)], " done", sep = ""))
  flush.console()

}





  url <- paste("http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/",
               as.character(2019),
               "/paavo_9_koko_",
               as.character(2019),
               ".px", sep = "")

  zip_data <-
    get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/2019/paavo_9_koko_2019.px",
                   dims = list(Postinumeroalue = c('*'),
                               Tiedot = c('Euref_x', 'Euref_y', 'Pinta_ala')),
                   clean = TRUE)

  names(zip_data) <- c("alue", "tiedot", "value")
  alue <- sapply(zip_data$alue, function(x) {substring(x,1,5)})
  zip_data$alue <- alue
  zip_data <- filter(zip_data, !grepl("KOKO", alue))
  zip_data <- zip_data %>% spread(tiedot, value) %>% clean_names()

  zip_data$vuosi <- julkaisuvuosi
  filename <- paste("data/zip_data/zip_data_9_", as.character(julkaisuvuosi), ".rds", sep = "")
  saveRDS(zip_data, filename)

  Sys.sleep(0.01)
  print(paste(julkaisuvuosi, "/", julkaisuvuodet[length(julkaisuvuodet)], " done", sep = ""))
  flush.console()



  years <- 2012:2017

  for(year in years) {

    counter = 1

    for(aineistokoodi in aineistokoodit) {

    filename <- paste("zip_data_", aineistokoodi, as.character(year), ".rds", sep = "")

    if(filename %in% dir("data/zip_data")) {
       zip_data_temp <- readRDS(paste("data/zip_data/", filename, sep = ""))
    } else {next}

    if(counter == 1) {
      zip_data <- zip_data_temp
    } else {
      zip_data_temp$vuosi <- NULL
      zip_data <- left_join(zip_data, zip_data_temp, by = "alue")
    }

    counter = counter + 1

    }
    zip_data_temp <- readRDS("data/zip_data/zip_data_9_2019.rds")
    zip_data_temp$vuosi <- NULL
    zip_data <- left_join(zip_data, zip_data_temp, by = "alue")

    saveRDS(zip_data, paste("data/zip_data/zip_data", as.character(year), ".rds", sep = ""))

  }

  rm(zip_data); rm(zip_data_temp); rm(filename)


  # Just find all postal code areas possible with their coordinates

  aineistokoodi <- "9_koko_"
  julkaisuvuodet <- 2016:2019

  zip_coord <- data.frame()

  for(julkaisuvuosi in julkaisuvuodet) {

    url <- paste("http://pxnet2.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/",
                   as.character(julkaisuvuosi),
                   "/paavo_",
                   aineistokoodi,
                   as.character(julkaisuvuosi),
                   ".px", sep = "")

      zip_data <-
        get_pxweb_data(url = url,
                       dims = list(Postinumeroalue = c('*'),
                                   Tiedot = c('Euref_x', 'Euref_y')),
                       clean = TRUE)

      names(zip_data) <- c("alue", "tiedot", "value")
      alue <- sapply(zip_data$alue, function(x) {substring(x,1,5)})
      zip_data$alue <- alue
      zip_data <- zip_data %>% spread(tiedot, value)
      names(zip_data) <- c("alue", "x_zip", "y_zip")

      zip_coord <- rbind(zip_coord, zip_data)
  }

  zip_coordinates <- zip_coord[!duplicated(zip_coord$alue),]
  saveRDS(zip_coordinates, file = "data/zip_data/zip_coordinates.rds")

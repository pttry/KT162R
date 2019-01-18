# Muuttojen määrät ja muuttoasteet 1990-2017

data0 <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a1.px",
                 dims = list(Tuloalue = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                             Lähtöalue = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                             Sukupuoli = c('SSS'),
                             Vuosi = c('*'),
                             Tiedot = c('*')),
                 clean = TRUE)

  data <- data0

# Rename variables
   data <- rename(data, Lahtokunta = Lähtöalue)
   data <- rename(data, Tulokunta = Tuloalue)

# Change type of Tulokunta ja Lahtokunta to character
   data$Tulokunta <- as.character(data$Tulokunta)
   data$Lahtokunta <- as.character(data$Lahtokunta)

# Remove redundant prefixes from variables Tulokunta and Lahtokunta
   tulokunnat <- sapply(sapply(data$Tulokunta, strsplit, " - "), '[', 2)
   lahtokunnat <- sapply(sapply(data$Lahtokunta, strsplit, " - "), '[', 2)
   names(tulokunnat) <- NULL
   names(lahtokunnat) <- NULL
   data$Tulokunta <- tulokunnat
   data$Lahtokunta <- lahtokunnat

# Change type of Vuosi to integer
   data$Vuosi <- as.integer(as.character(data$Vuosi))

# Add an indicator telling whether the migration occurs between maakunnat of seutukunnat, takes for a while.
# Memory problems may occur.
   data <- data %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data <- data %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

# Alternatively, process data in chunks
   data1 <- data[1:500000,]
   data2 <- data[500001:1000000,]
   data3 <- data[1000001:1500000,]
   data4 <- data[1500001:2000000,]
   data5 <- data[2000001:dim(data)[1], ]

   data1 <- data1 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data1 <- data1 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

   data2 <- data2 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data2 <- data2 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

   data3 <- data3 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data3 <- data3 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

   data4 <- data4 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data4 <- data4 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

   data5 <- data5 %>% mutate(maakuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Maakunta"))
   data5 <- data5 %>% mutate(seutukuntien_valinen_muutto = is.erialue(Tulokunta, Lahtokunta, "Seutukunta"))

   data <- rbind(data1, data2, data3, data4, data5)


# Name data set
   muuttodata <- data

# Save data
   saveRDS(muuttodata, file = "R/data_clean/muuttodata.rds")

# Compute all yearly migration between maakunnat, seutukunnat and kunnat
   maakuntien_valiset_muutot_vuosittain <- data %>%
     group_by(Vuosi) %>%
     summarize(maakuntien_valisia_muuttoja = sum(values * maakuntien_valinen_muutto))
   seutukuntien_valiset_muutot_vuosittain <- data %>%
     group_by(Vuosi) %>%
     summarize(seutukuntien_valisia_muuttoja = sum(values * seutukuntien_valinen_muutto))
   kuntien_valiset_muutot_vuosittain <- data %>%
     group_by(Vuosi) %>%
     summarize(kuntien_valisia_muuttoja = sum(values))

# Load kuntien sisäiset muutot
    kuntien_sisaiset_muutot_vuosittain <- readRDS("R/data_clean/kuntien_sisaiset_muutot.rds")
    names(kuntien_sisaiset_muutot_vuosittain) <- c("Vuosi", "kuntien_sisaisia_muuttoja")

# Load väkilukutiedot
    data(dat_vakiluku)

# Compute muuttoasteet

    maakuntien_valiset_muutot_vuosittain <- left_join(maakuntien_valiset_muutot_vuosittain,
                                                      dat_vakiluku, by = "Vuosi") %>%
                 mutate(muuttoaste = maakuntien_valisia_muuttoja / dat_vakiluku)
    seutukuntien_valiset_muutot_vuosittain <- left_join(seutukuntien_valiset_muutot_vuosittain,
                                                        dat_vakiluku, by = "Vuosi") %>%
      mutate(muuttoaste = seutukuntien_valisia_muuttoja / dat_vakiluku)
    kuntien_valiset_muutot_vuosittain <- left_join(kuntien_valiset_muutot_vuosittain,
                                                   dat_vakiluku, by = "Vuosi") %>%
      mutate(muuttoaste = kuntien_valisia_muuttoja / dat_vakiluku)
    kuntien_sisaiset_muutot_vuosittain <- left_join(kuntien_sisaiset_muutot_vuosittain,
                                                    dat_vakiluku, by = "Vuosi") %>%
      mutate(muuttoaste = kuntien_sisaisia_muuttoja / dat_vakiluku)

    names(seutukuntien_valiset_muutot_vuosittain) <- c("vuosi", "muuttoja","vakiluku", "muuttoaste")
    names(maakuntien_valiset_muutot_vuosittain) <- c("vuosi", "muuttoja","vakiluku", "muuttoaste")
    names(kuntien_valiset_muutot_vuosittain) <- c("vuosi", "muuttoja","vakiluku", "muuttoaste")
    names(kuntien_sisaiset_muutot_vuosittain) <- c("vuosi", "muuttoja", "vakiluku", "muuttoaste")
    muutot <- rbind(maakuntien_valiset_muutot_vuosittain,
                    seutukuntien_valiset_muutot_vuosittain,
                    kuntien_valiset_muutot_vuosittain,
                    kuntien_sisaiset_muutot_vuosittain)
    muutot$muuton_tyyppi <- as.factor(rep(1:4, each = 28))
    levels(muutot$muuton_tyyppi) <- c("maakuntien valiset muutot",
                                      "seutukuntien valiset muutot",
                                      "kuntien valiset muutot",
                                      "kuntien sisaiset muutot")

# Rename data
    dat_kokonaismuutto <- muutot

# Save data
    usethis::use_data(dat_kokonaismuutto, overwrite = TRUE)


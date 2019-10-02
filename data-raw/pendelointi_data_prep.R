# Pendelöintiaineisto

library(tidyverse)
library(pxweb)

# Lähtö pendelöinti

dat_lahtopendelointi <-
  pxweb_get_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_115n.px",
                 query = list(Alue = c('*'),
                             Pendelöinti = c('*'),
                             Koulutusaste = c('*'),
                             Ikä = c('*'),
                             Vuosi = c('*'),
                             Tiedot =c('*'))) %>%
  statfitools::clean_times() %>%
  statfitools::clean_names(to_lower = TRUE) %>%
  rename(values = tyolliset)


dat_lahtopendelointi_tyyppi <-
  dat_lahtopendelointi %>%
  mutate(alue = fct_recode(alue, Maarianhamina = "Maarianhamina - Mariehamn")) %>%
  # filter(alue != "KOKO MAA") %>%
  left_join(rename(aluetyyppi, alue = "kunta"), by = "alue") %>%
  group_by(aluetyyppi, time, pendelointi, koulutusaste, ika) %>%
  summarise(values = sum(values)) %>%
  ungroup() %>%
  mutate(aluetyyppi = as_factor(aluetyyppi))



# Tulopendelöinti


dat_tulopendelointi <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_115p.px",
                 dims = list("Työpaikan alue" = c('*'),
                             Pendelöinti = c('*'),
                             Koulutusaste = c('*'),
                             Ikä = c('*'),
                             Vuosi = c('*'),
                             Tiedot = c('*') ),
                 clean = TRUE) %>%
  statfitools::clean_times() %>%
  statfitools::clean_names(to_lower = TRUE)


dat_tulopendelointi_tyyppi <-
  dat_tulopendelointi %>%
  rename(alue = tyopaikan_alue) %>%
  mutate(alue = fct_recode(alue, Maarianhamina = "Maarianhamina - Mariehamn")) %>%
  filter(alue != "KOKO MAA") %>%
  left_join(rename(aluetyyppi, alue = "kunta"), by = "alue") %>%
  group_by(aluetyyppi, time, pendelointi, koulutusaste, ika) %>%
  summarise(values = sum(values)) %>%
  ungroup() %>%
  mutate(aluetyyppi = as_factor(aluetyyppi))

usethis::use_data(dat_lahtopendelointi, dat_tulopendelointi, overwrite = TRUE)
usethis::use_data(dat_lahtopendelointi_tyyppi, dat_tulopendelointi_tyyppi, overwrite = TRUE)








lahtopendelointi_data <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_013.px",
                 dims = list(Alue = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                             Pendelöinti = c('*'),
                             Koulutusaste = c('9', '3-4', '5-6', '7-8'),
                             Ikä = c('0-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-'),
                             Vuosi = c('*')),
                 clean = TRUE)

# Change the type of the year variable
  lahtopendelointi_data$Vuosi <- as.integer(as.character(lahtopendelointi_data$Vuosi))

# Lisää seutukunnat, maakunnat ja kuntatyypit dataan Huom! kuntaluokka-funktio tulee olla käytettävissä
# memory issues, computing in two chunks
  lahtopendelointi_data1 <- lahtopendelointi_data[1:400000,]
  lahtopendelointi_data2 <- lahtopendelointi_data[400001:895680,]

  lahtopendelointi_data1 <- mutate(lahtopendelointi_data1, seutukunta = kuntaluokka(Alue, "Seutukunta"))
  lahtopendelointi_data1 <- mutate(lahtopendelointi_data1, maakunta = kuntaluokka(Alue, "Maakunta"))
  lahtopendelointi_data1 <- mutate(lahtopendelointi_data1, kuntaryhmä = kuntaluokka(Alue, "Kuntaryhma"))

  lahtopendelointi_data2 <- mutate(lahtopendelointi_data2, seutukunta = kuntaluokka(Alue, "Seutukunta"))
  lahtopendelointi_data2 <- mutate(lahtopendelointi_data2, maakunta = kuntaluokka(Alue, "Maakunta"))
  lahtopendelointi_data2 <- mutate(lahtopendelointi_data2, kuntaryhmä = kuntaluokka(Alue, "Kuntaryhma"))

  lahtopendelointi_data <- rbind(lahtopendelointi_data1, lahtopendelointi_data2)

# Save data
  saveRDS(lahtopendelointi_data, file = "R/data_clean/lahtopendelointi_data.rds")


tulopendelointi_data <-
    get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_014.px",
                   dims = list("Työpaikan alue" = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                               Pendelöinti = c('*'),
                               Koulutusaste = c('9', '3-4', '5-6', '7-8'),
                               Ikä = c('0-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-'),
                               Vuosi = c('*')),
                   clean = TRUE)

# Change the type of the year variable
   tulopendelointi_data$Vuosi <- as.integer(as.character(tulopendelointi_data$Vuosi))
   names(tulopendelointi_data) <- c("Alue", names(tulopendelointi_data)[2:6])

# Lisää seutukunnat, maakunnat ja kuntatyypit dataan Huom! kuntaluokka-funktio tulee olla käytettävissä
# memory issues, computing in two chunks
   tulopendelointi_data1 <- tulopendelointi_data[1:400000,]
   tulopendelointi_data2 <- tulopendelointi_data[400001:895680,]

   tulopendelointi_data1 <- mutate(tulopendelointi_data1, seutukunta = kuntaluokka(Alue, "Seutukunta"))
   tulopendelointi_data1 <- mutate(tulopendelointi_data1, maakunta = kuntaluokka(Alue, "Maakunta"))
   tulopendelointi_data1 <- mutate(tulopendelointi_data1, kuntaryhmä = kuntaluokka(Alue, "Kuntaryhma"))

   tulopendelointi_data2 <- mutate(tulopendelointi_data2, seutukunta = kuntaluokka(Alue, "Seutukunta"))
   tulopendelointi_data2 <- mutate(tulopendelointi_data2, maakunta = kuntaluokka(Alue, "Maakunta"))
   tulopendelointi_data2 <- mutate(tulopendelointi_data2, kuntaryhmä = kuntaluokka(Alue, "Kuntaryhma"))

   tulopendelointi_data <- rbind(tulopendelointi_data1, tulopendelointi_data2)

# Save data
saveRDS(tulopendelointi_data, file = "R/data_clean/tulopendelointi_data.rds")

# Combine the data

lahtopendelointi_data <- readRDS("R/data_clean/lahtopendelointi_data.rds")
tulopendelointi_data <- readRDS("R/data_clean/tulopendelointi_data.rds")

lahtopendelointi_data <- lahtopendelointi_data %>% spread(Pendelöinti, values)
names(lahtopendelointi_data) <- c("alue", "koulutusaste", "vuosi", "ika", "seutukunta", "maakunta", "kuntaryhma",
                                  "asuinkunnassaan_tyossakayvat", "pendeloivat", "tyolliset_yhteensa")

lahtopendelointi_data <- select(lahtopendelointi_data, - tyolliset_yhteensa) %>%
                         rename(lahtopendelointi = pendeloivat)

tulopendelointi_data <- tulopendelointi_data %>% spread(Pendelöinti, values)
names(tulopendelointi_data) <- c("alue", "koulutusaste", "vuosi", "ika", "seutukunta", "maakunta", "kuntaryhma",
                                 "tyolliset_yhteensa", "asuinkunnassaan_tyossakayvat", "pendeloivat")

tulopendelointi_data <- select(tulopendelointi_data, - tyolliset_yhteensa) %>%
  rename(tulopendelointi = pendeloivat)

dat_pendelointi <- left_join(tulopendelointi_data, lahtopendelointi_data)

# Save data
  usethis::use_data(dat_pendelointi, overwrite = TRUE)



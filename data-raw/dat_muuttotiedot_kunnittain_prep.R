# Muuttodata kunnittain, aluetyyppitiedot ja väkilukutiedot mukaan. Muuttoasteiden laskenta.

<<<<<<< HEAD
# Get the data

library(pxweb)
library(tidyverse)

dat_muuttotiedot_kunnittain <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
                 dims = list(Alue = c('020', '005', '009', '010', '016', '018', '019', '035', '043', '046', '047', '049', '050', '051', '052', '060', '061', '062', '065', '069', '071', '072', '074', '075', '076', '077', '078', '079', '081', '082', '086', '111', '090', '091', '097', '098', '099', '102', '103', '105', '106', '108', '109', '139', '140', '142', '143', '145', '146', '153', '148', '149', '151', '152', '165', '167', '169', '170', '171', '172', '176', '177', '178', '179', '181', '182', '186', '202', '204', '205', '208', '211', '213', '214', '216', '217', '218', '224', '226', '230', '231', '232', '233', '235', '236', '239', '240', '320', '241', '322', '244', '245', '249', '250', '256', '257', '260', '261', '263', '265', '271', '272', '273', '275', '276', '280', '284', '285', '286', '287', '288', '290', '291', '295', '297', '300', '301', '304', '305', '312', '316', '317', '318', '398', '399', '400', '407', '402', '403', '405', '408', '410', '416', '417', '418', '420', '421', '422', '423', '425', '426', '444', '430', '433', '434', '435', '436', '438', '440', '441', '475', '478', '480', '481', '483', '484', '489', '491', '494', '495', '498', '499', '500', '503', '504', '505', '508', '507', '529', '531', '535', '536', '538', '541', '543', '545', '560', '561', '562', '563', '564', '309', '576', '577', '578', '445', '580', '581', '599', '583', '854', '584', '588', '592', '593', '595', '598', '601', '604', '607', '608', '609', '611', '638', '614', '615', '616', '619', '620', '623', '624', '625', '626', '630', '631', '635', '636', '678', '710', '680', '681', '683', '684', '686', '687', '689', '691', '694', '697', '698', '700', '702', '704', '707', '729', '732', '734', '736', '790', '738', '739', '740', '742', '743', '746', '747', '748', '791', '749', '751', '753', '755', '758', '759', '761', '762', '765', '766', '768', '771', '777', '778', '781', '783', '831', '832', '833', '834', '837', '844', '845', '846', '848', '849', '850', '851', '853', '857', '858', '859', '886', '887', '889', '890', '892', '893', '895', '785', '905', '908', '911', '092', '915', '918', '921', '922', '924', '925', '927', '931', '934', '935', '936', '941', '946', '976', '977', '980', '981', '989', '992'),
                             Vuosi = c('*'),
                             Sukupuoli = c('SSS'),
                             Ikä = c('SSS'),
                             Tiedot = c('*')),
                 clean = TRUE)
=======
library(pxweb)
library(tidyverse)
devtools::load_all()

# Load aluetyypit and väkilukutiedot
load("data/aluetyyppi.rda")
load("data/dat_kuntien_vakiluvut.rda")
>>>>>>> b63e37969fe45f9433900acf631e3407fd42204a


# Get the data
dat_muuttotiedot_kunnittain0 <-
   get_pxweb_data(
      url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
      dims = list(
         Alue = c('*'),
         Vuosi = c('*'),
         Sukupuoli = c('SSS'),
         Ikä  = c('SSS'),
         Tiedot = c('*')
      ),
      clean = TRUE
   )

# Change types of Alue and Vuosi and names in Tiedot
dat_muuttotiedot_kunnittain <- dat_muuttotiedot_kunnittain0 %>%
   mutate(
      Alue = as.character(Alue),
      Vuosi = as.integer(as.character(Vuosi)),
      Tiedot = fct_recode(Tiedot,
         tulomuutto = "Kuntien välinen tulomuutto",
         lahtomuutto = "Kuntien välinen lähtömuutto",
         nettomuutto = "Kuntien välinen nettomuutto")) %>%
   filter(Alue != "KOKO MAA") %>%
   select(-Ikä, -Sukupuoli) %>%
   rename(kunta = Alue) %>%
   left_join(aluetyyppi, by = "kunta") %>%
   rename(alue = kunta) %>%
   left_join(dat_kuntien_vakiluvut, by = c("alue", "Vuosi")) %>%
   mutate(alue = fct_recode(alue, Maarianhamina = "Maarianhamina - Mariehamn")) %>%
   mutate(seutukunta = kuntaluokka(alue, "Seutukunta")) %>%
   mutate(maakunta = kuntaluokka(alue, "Maakunta")) %>%
   mutate(kuntaryhma = kuntaluokka(alue, "Kuntaryhma")) %>%
   spread(Tiedot, values) %>%
   mutate(
      tulomuuttoaste = tulomuutto / vakiluku,
      lahtomuuttoaste = lahtomuutto / vakiluku,
      nettomuuttoaste = nettomuutto / vakiluku
   ) %>%
   gather(Tiedot, values, c("tulomuutto", "lahtomuutto", "nettomuutto",
                            "tulomuuttoaste", "lahtomuuttoaste", "nettomuuttoaste")
   )



# Save data
<<<<<<< HEAD
saveRDS(dat_muuttotiedot_kunnittain, file = "data/dat_muuttotiedot_kunnittain.rds")

use_data(dat_muuttotiedot_kunnittain, overwrite = TRUE)
=======
   usethis::use_data(dat_muuttotiedot_kunnittain, overwrite = TRUE)

>>>>>>> b63e37969fe45f9433900acf631e3407fd42204a

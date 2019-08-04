# Työttömät työnhakijat ja avoimet työpaikat alueittain

library(pxweb)
library(tidyverse)
library(readxl)

# Avoimet työpaikat avoinna kuukauden lopussa.

avoimet <-
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_2205.px",
                 dims = list(Alue = c('MK01', 'MK02', 'MK04', 'MK05', 'MK06', 'MK07', 'MK08', 'MK09', 'MK10', 'MK11', 'MK12', 'MK13', 'MK14', 'MK15', 'MK16', 'MK17', 'MK18', 'MK19', 'MK21', 'X'),
                             Ammattiryhmä = c('SSS'),
                             "Työnantajan sektori" = c('SSS'),
                             "Työpaikan työn kesto" = c('SSS'),
                             Kuukausi = c('2019M04'),
                             Tiedot = c('AVPAIKATLOPUSSA')),
                 clean = TRUE)


avoimet <- avoimet %>% select(Alue, Kuukausi, values) %>%
                       filter(Alue != "Tuntematon") %>%
                       separate(Alue, c("maakunta", "nimi"), sep = " ") %>%
                       mutate(maakunta = substring(maakunta, 3,4))


# Työttömät työnhakijat

# Download data
px_data <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_1220.px",
            query = "[path to jsonfile]")

# Convert to data.frame
px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
hakijat <- data$data

names(hakijat) <- c("Alue", "Työllisyys", "Koulutus", "Kuukausi", "values")
hakijat <- hakijat %>% select(Alue, Kuukausi, values) %>%
  filter(Alue != "Tuntematon") %>%
  separate(Alue, c("maakunta", "nimi"), sep = " ") %>%
  mutate(maakunta = substring(maakunta, 3,4))



draw_map(hakijat, 2019, "maakunta", "values")
draw_map(avoimet, 2019, "maakunta", "values")

# PXWEB query
pxweb_query_list <-
  list("Alue"=c("KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU099","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU588","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU911","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Kuukausi"=c("2019M04"),
       "Tiedot"=c("HAKIJAT","TYOTTOMAT","TYOVOIMA","TYOTOSUUS","TH2","TH3","TH4","TH5","TH6","TH7","TH8","PT9","KASSASSA","LOMAUTETTU","LYHTYOVKO","TT10","TT11","KOKLOP","TVK12","VALLOP","MTP13","AVPAIKAT","UUDETAVP"))

# Download data
px_data <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_1001.px",
            query = pxweb_query_list)

# Convert to data.frame
px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

tyo_data <- px_data %>% select(Alue, Kuukausi, "Työvoima", "Työttömät", "Työttömien osuus", "Avoimet työpaikat") %>%
                          mutate(Alue = as.character(Alue))

tyo_data$Alue[107] <- "Koski"
tyo_data$Alue[152] <- "Maarianhamina"
tyo_data$Alue[182] <- "Pedersören"

names(tyo_data) <- c("nimi", "Kuukausi", "tyovoima", "tyottomat", "tyottomyysaste", "avoimet")

tyo_data <- tyo_data %>% mutate(avoimet_osuus = avoimet / tyovoima)

alueet <- read_excel("data-raw/msindaluejaot10.xls") %>%
          separate(Kunta, c("kunta", "nimi"), sep = " ") %>%
          select(kunta, nimi)

tyo_data <- left_join(tyo_data, alueet, by = "nimi") # %>%
     #       select(tyottomyysaste, avoimet_osuus, kunta) %>%
    #        gather(tiedot, value, -kunta)


file <- paste("tilastointialueet:", "kunta", "4500k_", as.character(2018), sep = "")

url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
url2$query <- list(service ="WFS",
                   version ="2.0.0",
                   request ="GetFeature",
                   typename = file,
                   outputFormat ="application/json")

map <- sf::st_read(httr::build_url(url2))

left_join(map, tyo_data, by = "kunta") %>%
  ggplot(aes(fill = tyottomyysaste)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_blank()) +
  labs(fill = "Unemployment rate")

ggsave("C:/Users/juhoa/Google Drive/Labor Economics/Labor Econ presentation/unemployment_map.pdf")

left_join(map, tyo_data, by = "kunta") %>%
  ggplot(aes(fill = log(avoimet_osuus))) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_light() +
  theme(
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_blank()) +
  labs(fill = "ln(Vacancies / Labor force)")

ggsave("C:/Users/juhoa/Google Drive/Labor Economics/Labor Econ presentation/vacancyrate_map.pdf")


left_join(tyo_data, map, by = "kunta") %>%
  ggplot(aes(fill = log(value))) +
  geom_sf() +
  facet_wrap(~tiedot) +
  scale_fill_gradient(low = "darkseagreen1", high = "darkgreen")

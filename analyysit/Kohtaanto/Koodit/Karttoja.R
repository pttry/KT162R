library(pxweb)
library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(sf)


# Hae data pxwebistä

# 1001 -- Työttömät työnhakijat eri ryhmissä, palveluissa olevat ja avoimet työpaikat kuukauden lopussa.

# PXWEB query
pxweb_query_list <-
  list("Alue"=c("KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU099","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU588","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU911","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Kuukausi"=c("2006M01","2006M02","2006M03","2006M04","2006M05","2006M06","2006M07","2006M08","2006M09","2006M10","2006M11","2006M12","2007M01","2007M02","2007M03","2007M04","2007M05","2007M06","2007M07","2007M08","2007M09","2007M10","2007M11","2007M12","2008M01","2008M02","2008M03","2008M04","2008M05","2008M06","2008M07","2008M08","2008M09","2008M10","2008M11","2008M12","2009M01","2009M02","2009M03","2009M04","2009M05","2009M06","2009M07","2009M08","2009M09","2009M10","2009M11","2009M12","2010M01","2010M02","2010M03","2010M04","2010M05","2010M06","2010M07","2010M08","2010M09","2010M10","2010M11","2010M12","2011M01","2011M02","2011M03","2011M04","2011M05","2011M06","2011M07","2011M08","2011M09","2011M10","2011M11","2011M12","2012M01","2012M02","2012M03","2012M04","2012M05","2012M06","2012M07","2012M08","2012M09","2012M10","2012M11","2012M12","2013M01","2013M02","2013M03","2013M04","2013M05","2013M06","2013M07","2013M08","2013M09","2013M10","2013M11","2013M12","2014M01","2014M02","2014M03","2014M04","2014M05","2014M06","2014M07","2014M08","2014M09","2014M10","2014M11","2014M12","2015M01","2015M02","2015M03","2015M04","2015M05","2015M06","2015M07","2015M08","2015M09","2015M10","2015M11","2015M12","2016M01","2016M02","2016M03","2016M04","2016M05","2016M06","2016M07","2016M08","2016M09","2016M10","2016M11","2016M12","2017M01","2017M02","2017M03","2017M04","2017M05","2017M06","2017M07","2017M08","2017M09","2017M10","2017M11","2017M12","2018M01","2018M02","2018M03","2018M04","2018M05","2018M06","2018M07","2018M08","2018M09","2018M10","2018M11","2018M12","2019M01","2019M02","2019M03","2019M04","2019M05","2019M06"),
       "Tiedot"=c("TYOTTOMAT","TYOVOIMA","TYOTOSUUS","AVPAIKAT"))

# Download data
px_data <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/kk/statfin_tyonv_pxt_1001.px",
            query = pxweb_query_list)

# Convert to data.frame
data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Clean data

data <- clean_names(data) %>%
  mutate(vuosi = as.double(substring(Kuukausi, 1,4)))

data$Alue <- as.character(data$Alue)
data$Alue[which(data$Alue == "Maarianhamina - Mariehamn")] <- "Maarianhamina"

# Lisää alue- ja aluetyyppitietoja

data <- data %>% rename(Kunta = Alue)
alueet <- sf_get_reg_keytable() %>% select(Knro, Kunta, Mkkoodi, Maakunta, Seutukuntakoodi, Seutukunta, Kuntaryhma)

data <- left_join(data, alueet, by = "Kunta")

atyypit <- readRDS("data/atyypit.rds") %>%
  rename(Knro = kunta18)

data <- left_join(data, atyypit, by = "Knro") %>%
        rename(kunta = Knro)

# Hae kartta

file <- paste("tilastointialueet:", "kunta", "4500k_", as.character(2018), sep = "")

url2 <- httr::parse_url("https://geo.stat.fi/geoserver/tilastointialueet/wfs")
url2$query <- list(service ="WFS",
                   version ="2.0.0",
                   request ="GetFeature",
                   typename = file,
                   outputFormat ="application/json")

map <- sf::st_read(httr::build_url(url2))


map_data <- filter(data, vuosi == 2018) %>%
  group_by(kunta) %>%
  summarize(Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  mutate(vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima + Tyottomat),
         tyottomyysaste = Tyottomat / Tyovoima)

vakanssiaste_map <- left_join(map, map_data, by = "kunta")  %>%
  ggplot(aes(fill = vakanssiaste)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_blank()) +
  labs(fill = "Vakanssiaste")

tyottomyysaste_map <- left_join(map, map_data, by = "kunta")  %>%
  ggplot(aes(fill = tyottomyysaste)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_light() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_blank()) +
  labs(fill = "Tyottomyysaste")

kartat <- grid.arrange(vakanssiaste_map,  tyottomyysaste_map, ncol = 2)

ggsave("analyysit/Kohtaanto/Kuviot/Kartat/vakanssi_aste_tyottomyys_2018_kuukausika.png", plot = kartat)

ggsave("C:/Users/juhoa/Google Drive/Projects/New job, migrate or commute/Presentation/Graphs/maps.pdf",
       plot = kartat)


# Missä vakanssiaste laskenut

vuodet <- c(2006,2016)

df <- data %>% mutate(tyottomyysaste = Tyottomat / Tyovoima,
                        vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
                 filter(vuosi %in% vuodet) %>%
                 group_by(kunta, vuosi) %>%
                 summarize(vakanssiaste = mean(vakanssiaste, na.rm = TRUE),
                           tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE)) %>%
                 summarize(dvakanssiaste = diff(vakanssiaste)) %>%
                 mutate(vakanssiaste_noussut = dvakanssiaste > 0)

left_join(map, df, by = "kunta")  %>%
  ggplot(aes(fill = vakanssiaste_noussut)) +
  geom_sf() +
  labs(fill = NULL) +
  scale_fill_manual(labels = c(paste("Vakanssiaste laskenut", paste(vuodet, collapse = "-")),
                               paste("Vakanssiaste noussut", paste(vuodet, collapse = "-"))),
                      values = c("red3", "forestgreen"))

# Missä tyottomyysaste laskenut

df <- data %>% mutate(tyottomyysaste = Tyottomat / Tyovoima,
                      vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  filter(vuosi %in% vuodet) %>%
  group_by(kunta, vuosi) %>%
  summarize(vakanssiaste = mean(vakanssiaste, na.rm = TRUE),
            tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE)) %>%
  summarize(dtyottomyysaste = diff(tyottomyysaste)) %>%
  mutate(tyottomyysaste_noussut = dtyottomyysaste > 0)

left_join(map, df, by = "kunta")  %>%
  ggplot(aes(fill = tyottomyysaste_noussut)) +
  geom_sf() +
  labs(fill = NULL) +
  scale_fill_manual(labels = c(paste("Tyottomyysaste laskenut", paste(vuodet, collapse = "-")),
                               paste("Tyottomyysaste noussut", paste(vuodet, collapse = "-"))),
                    values = c("forestgreen", "red3"))




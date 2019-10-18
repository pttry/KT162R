library(tidyverse)

source("R/coef_plot.R")

toimialanames = c("A" = "Maa-, metsä-, ja kalatalous (A)",
                  "B" = "Kaivostoiminta louhinta (B)",
                  "C" = "Teollisuus (C)",
                  "D" = "Sähkö-, kaasu- ja lämpöhuolto, jäähdytys (D)",
                  "E" = "Vesi-, viemäri-, jätevesi- ja jätehuolto; puhtaanapito (E)",
                  "F" = "Rakentaminen (F)",
                  "G" = "Tukku- ja vähittäiskauppa: moottoriajoneuvojen korjaus (G)",
                  "H" = "Kuljetus ja varastointi (H)",
                  "I" = "Majoitus- ja ravitsemustoiminta (I)",
                  "J" = "Informaatio ja viestintä (J)",
                  "K" = "Rahoitus- ja vakuutustoiminta (K)",
                  "L" = "Kiinteistöalan toiminta (L)",
                  "M" = "Ammatillinen, tieteellinen ja tekninen toiminta (M)",
                  "N" = "Hallinto- ja tukipalvelutoiminta (N)",
                  "O" = "Julkinen hallinto, maanpuolustus, sosiaalivakuutus (O)",
                  "P" = "Koulutus (P)",
                  "Q" = "Terveys- ja sosiaalipalvelut (Q)",
                  "R" = "Taiteet, viihde ja virkistys (R)",
                  "S" = "Muu palvelutoiminta (S)",
                  "T" = "Kotitalouksien toiminta työnantajana (T)",
                  "U" = "Kansainvälisten organisaatioiden toiminta (U)",
                  "X" = "Toimiala tuntematon (X)")


###### Toimialat, palkansaajat, conditional #######################################

data <- readRDS("data/oct2/coefficient_plot_data_salaried_ols_ln.rds") %>%
        filter(grepl("toimiala", var)) %>%
        mutate(coefficient = 100*round(coefficient, digits = 3))

data$var <- gdata::drop.levels(data$var)

levels(data$var) <- c("M", "N", "J", "O", "B", "U", "L", "T", "P", "H", "A", "I", "S",
                      "K", "F", "D", "R", "Q", "X", "G", "E")

data <- filter(data, var != "X")

data$var <- gdata::drop.levels(data$var)

data$var <- factor(data$var, levels(data$var)[length(levels(data$var)):1])

data$var <- plyr::revalue(data$var, replace = toimialanames)

data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero teollisuuteen, %") +
  scale_x_discrete(labels = )
# deci_comma

ggsave("analyysit/Pendelointi/regressiotulokset/conditional_toimiala_palkansaajat.png",
       width = 7, height = 6)

############## Toimiala, nonconditional, palkansaajat ###############################################

data <- readRDS("data/oct2/salaried_toimiala.rds") %>%
        mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
               median_tyomatka = round(median_tyomatka, digits = 1))

levels(data$toimiala) <- c("C", "X", "A", "B", "D", "E", "F", "G", "H", "I", "J", "K",
                           "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U")

data <- filter(data, toimiala != "X")

data$toimiala <- gdata::drop.levels(data$toimiala)

data$toimiala <- plyr::revalue(data$toimiala, replace = toimialanames)

data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

ggsave("analyysit/Pendelointi/regressiotulokset/unconditional_toimiala_palkansaajat.png",
       width = 7, height = 7)

# Ammatit, palkansaajat

data <- readRDS("data/oct2/coefficient_plot_data_salaried_ols_ln.rds") %>%
  filter(grepl("ammattikoodi_k", var)) %>%
  mutate(coefficient = 100*round(coefficient, digits = 3),
         se = 100*se)


ammatti_labels <- c("Erityisasiantuntijat",
                    "Johtajat",
                    "Maanviljelijät, metsätyöntekijät ym.",
                    "Muut työntekijät",
                    "Palvelu- ja myyntityöntekijät",
                    "Prosessi- ja kuljetustyöntekijät",
                    "Rakennus-, korjaus- ja valmistustyöntekijät",
                    "Toimisto- ja asiakaspalvelutyöntekijät")

data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1, linetype = 3) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero asiantuntijoihin, %") +
  scale_x_discrete(labels = ammatti_labels)
# deci_comma

ggsave("analyysit/Pendelointi/regressiotulokset/conditional_ammatti.png",
       width = 7,
       height = 4)

############ Ammatit, unconditionals, palkansaajat ##############################


data <- readRDS("data/oct2/salaried_ammatti.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1)) %>%
  filter(!is.na(ammattikoodi_k))

data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(ammattikoodi_k)) %>%
ggplot(aes(y = mean_tyomatka, x = ammattikoodi_k)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = ammattikoodi_k,
                   yend = mean_tyomatka,
                   xend = ammattikoodi_k),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = ammattikoodi_k), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = ammattikoodi_k), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = ammattikoodi_k, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = ammattikoodi_k, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

ggsave("analyysit/Pendelointi/regressiotulokset/unconditional_ammatti_palkansaajat.png",
       width = 7, height = 4)

################ Palkansaajat, henkilökohtaiset muuttujat ############################

data <- readRDS("data/oct2/coefficient_plot_data_salaried_ols_ln.rds") %>%
  filter(!grepl("ammattikoodi_k", var)) %>%
  filter(!grepl("toimiala", var)) %>%
  filter(!grepl("kunta", var)) %>%
  filter(!grepl("Intercept", var)) %>%
  filter(!grepl("ika2", var)) %>%
  mutate(coefficient = 100*round(coefficient, digits = 3),
         se = 100*se)

data$var <- gdata::drop.levels(data$var)
data$var <- factor(data$var,
levels = c("auto_k0",
           "oty1Valtio",
           "oty1Kunta",
           "comm_expTRUE",
           "migr_expTRUE",
           "tyosuhteen_kesto",
           "taajama_k21",
           "hapeMuu hallintaperuste",
           "hapeAsumisoikeusasunnot",
           "hapeVuokralainen",
           "petyYksinhuoltaja",
           "petyYksin asuvat",
           "petyPari, lapsia",
           "spouse_workingTRUE",
           "kturaha_k",
           "ututku_asteTutkijakoulutusaste",
           "ututku_asteKorkea-aste",
           "ututku_asteToinen aste",
           "syntyp2Syntynyt ulkomailla",
           "opiskelijaOpiskelija",
           "ika",
           "sukupNainen"))

personal_labels <- c("Nainen (referenssi: mies)",
                     "Ikä, vuosia",
                     "Opiskelija",
                     "Syntynyt ulkomailla (referenssi: syntynyt Suomessa)",
                     "Toinen aste (referenssi: perusaste)",
                     "Korkea-aste (referenssi: perusaste)",
                     "Tutkijakoulutusaste (referenssi: perusaste)",
                     "Käytettävissä olevat tulot",
                     "Puoliso töissä",
                     "Pari, lapsia (referenssi: pari, ei lapsia)",
                     "Yksin asuvat (referenssi: pari, ei lapsia)",
                     "Yksinhuoltaja (referenssi: pari, ei lapsia)",
                     "Vuokralainen (referenssi: omistusasuja)",
                     "Asumisoikeusasunto (referenssi: omistusasuja)",
                     "Muu hallintaperuste (referenssi: omistusasuja)",
                     "Asuinpaikka taajamassa",
                     "Työsuhteen kesto, päiviä",
                     "Muuttamiskokemus",
                     "Pendelöintikokemus",
                     "Kunta (referenssi: yksityinen)",
                     "Valtio (referenssi: yksityinen)",
                     "Auto käytössä")[22:1]



data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +

  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1.2, linetype = 2)+
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Marginaalivaikutus, %") +
  scale_x_discrete(labels = personal_labels)# deci_comma

ggsave("analyysit/Pendelointi/regressiotulokset/conditional_personal.png",
       width = 7, height = 8)


### Self-employed

# Toimiala

data <- readRDS("data/oct2/coefficient_plot_data_self_employed_tobit.rds") %>%
  filter(grepl("toimiala", var)) %>%
  mutate(coefficient = round(coefficient, digits = 1),
         se = 100*se)

data$var <- gdata::drop.levels(data$var)

levels(data$var) <- c("N", "J", "B", "L", "P", "H", "A", "I", "S", "K", "F", "D", "R", "C", "Q", "G", "E")

data$var <- gdata::drop.levels(data$var)

data$var <- factor(data$var, levels(data$var)[length(levels(data$var)):1])

data$var <- plyr::revalue(data$var, replace = toimialanames)

data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1, linetype = 3) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero johonkin, %")


ggsave("analyysit/Pendelointi/regressiotulokset/conditional_toimiala_yrittajat.png",
       width = 7, height = 6)

# Yrityksen koko

data <- readRDS("data/oct2/coefficient_plot_data_self_employed_ols.rds") %>%
  filter(grepl("yr1_lvl", var) | grepl("yr1_hl", var)) %>%
  mutate(coefficient = round(coefficient, digits = 1),
         se = 100*se)


data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1, linetype = 3) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero johonkin, %") +
  scale_x_discrete(labels = c("Henkilömäärältä keskisuuri yritys",
                              "Henkilömäärältä suuri yritys",
                              "Liikevaihto 10 000 000 - 40 000 000",
                              "Liikevaihto 40 000 000 - "))

ggsave("analyysit/Pendelointi/regressiotulokset/conditional_yrityksen_koko_yrittajat.png",
       width = 7, height = 2)

# personal

data <- readRDS("data/oct2/coefficient_plot_data_self_employed_tobit.rds") %>%
  filter(!grepl("ammattikoodi_k", var)) %>%
  filter(!grepl("toimiala", var)) %>%
  filter(!grepl("aluetyyppi", var)) %>%
  filter(!grepl("yr", var)) %>%
  filter(!grepl("Intercept", var)) %>%
  filter(!grepl("ika2", var)) %>%
  mutate(coefficient = round(coefficient, digits = 1),
         se = 100*se)

data$var <- gdata::drop.levels(data$var)
data$var <- factor(data$var,
                   levels = c("auto_k0",
                              "comm_expTRUE",
                              "migr_expTRUE",
                              "tyosuhteen_kesto",
                              "taajama_k21",
                              "hapeMuu hallintaperuste",
                              "hapeAsumisoikeusasunnot",
                              "hapeVuokralainen",
                              "petyYksinhuoltaja",
                              "petyYksin asuvat",
                              "petyPari, lapsia",
                              "spouse_workingTRUE",
                              "ututku_asteTutkijakoulutusaste",
                              "ututku_asteKorkea-aste",
                              "ututku_asteToinen aste",
                              "syntyp2Syntynyt ulkomailla",
                              "ika",
                              "sukupNainen"))

personal_labels <- c("Nainen (referenssi: mies)",
                     "Ikä, vuosia",
                     "Syntynyt ulkomailla (referenssi: syntynyt Suomessa)",
                     "Toinen aste (referenssi: perusaste)",
                     "Korkea-aste (referenssi: perusaste)",
                     "Tutkijakoulutusaste (referenssi: perusaste)",
                     "Puoliso töissä",
                     "Pari, lapsia (referenssi: pari, ei lapsia)",
                     "Yksin asuvat (referenssi: pari, ei lapsia)",
                     "Yksinhuoltaja (referenssi: pari, ei lapsia)",
                     "Vuokralainen (referenssi: omistusasuja)",
                     "Asumisoikeusasunto (referenssi: omistusasuja)",
                     "Muu hallintaperuste (referenssi: omistusasuja)",
                     "Asuinpaikka taajamassa",
                     "Työsuhteen kesto, päiviä",
                     "Muuttamiskokemus",
                     "Pendelöintikokemus",
                     "Auto käytössä")[18:1]


data %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                color = "red", size = 1, linetype = 3) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero referenssikategorioihin tai marginaalivaikutus, %") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Pendelointi/regressiotulokset/conditional_personal_yrittajat.png",
       width = 7, height = 7)


############## Toimiala, nonconditional, yrittajat ###############################################

data <- readRDS("data/oct2/self_employed_toimiala.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka_suurempi_kuin_nolla, digits = 1),
         median_tyomatka = round(median_tyomatka_suurempi_kuin_nolla, digits = 1))

levels(data$toimiala) <-c("M", "N", "J", "B", "L", "P", "H", "A", "I", "S", "K", "F", "D", "R", "C", "Q", "G", "E")

data <- filter(data, toimiala != "X")

data$toimiala <- gdata::drop.levels(data$toimiala)

data$toimiala <- plyr::revalue(data$toimiala, replace = toimialanames)

data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

ggsave("analyysit/Pendelointi/regressiotulokset/unconditional_toimiala_yrittajat.png",
       width = 7, height = 7)

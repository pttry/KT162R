library(tidyverse)

source("R/coef_plot.R")

toimialanames = c("A" = "Maa-, metsä-, ja kalatalous (A)",
                  "B" = "Kaivostoiminta, louhinta (B)",
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
  geom_errorbar(aes(x = var, ymin = coefficient - 200*data$se, ymax = coefficient + 200*data$se),
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



################ Palkansaajat, henkilökohtaiset muuttujat ############################

data <- readRDS("data/oct2/coefficient_plot_data_salaried_ols_ln.rds") %>%
  filter(!grepl("ammattikoodi_k", var)) %>%
  filter(!grepl("toimiala", var)) %>%
  filter(!grepl("kunta", var)) %>%
  filter(!grepl("Intercept", var)) %>%
  filter(!grepl("ika2", var)) %>%
  mutate(coefficient = 100*round(coefficient, digits = 3),
         se = 100*se)


data_salaried <- data.frame(var = c("sukupNainen",
                           "ika",
                           "syntyp2Syntynyt ulkomailla",
                           "ututku_asteToinen aste",
                           "ututku_asteKorkea-aste",
                           "ututku_asteTutkijakoulutusaste",
                           "migr_exp",
                           "petyPari, ei lapsia",
                           "petyPari, lapsia",
                           "petyYksinhuoltaja",
                           "hapeVuokralainen",
                           "hapeAsumisoikeusasunnot",
                           "hapeMuu hallintaperuste",
                           "tatyRivi- tai ketjutalo",
                           "tatyAsuinkerrostalo",
                           "tatyMuu rakennus",
                           "spouse_working",
                           "tyosuhteen_kesto_kk",
                           "oty1Valtio",
                           "oty1Kunta"),
                   coefficient = c(-0.128,
                                   0.015,
                                   -0.023,
                                   0.049,
                                   0.058,
                                   -0.023,
                                   0.378,
                                   0.077,
                                   0.058,
                                   0.047,
                                   -0.116,
                                   0.065,
                                   -0.045,
                                   -0.270,
                                   -0.507,
                                   -0.449,
                                   -0.023,
                                   -0.001,
                                   -0.080,
                                   -0.197))

dummy_titles = data.frame(var = c("ututku_aste",
                                  "pety",
                                  "hape",
                                  "taty",
                                  "oty1"),
                          coefficient = rep(NA, 5))

data_salaried <- rbind(data_salaried, dummy_titles)

data_salaried$var <- gdata::drop.levels(data_salaried$var)
data_salaried$var <- factor(data_salaried$var,
levels = c("tyosuhteen_kesto",
           "oty1Valtio",
           "oty1Kunta",
           "oty1",
           "comm_expTRUE",
           "migr_expTRUE",
           "tatyMuu rakennus",
           "tatyAsuinkerrostalo",
           "tatyRivi- tai ketjutalo",
           "taty",
           "hapeMuu hallintaperuste",
           "hapeAsumisoikeusasunnot",
           "hapeVuokralainen",
           "hape",
           "spouse_workingTRUE",
           "petyYksinhuoltaja",
           "petyYksin asuvat",
           "petyPari, lapsia",
           "pety",
           "kturaha_k",
           "ututku_asteTutkijakoulutusaste",
           "ututku_asteKorkea-aste",
           "ututku_asteToinen aste",
           "ututku_aste",
           "syntyp2Syntynyt ulkomailla",
           "opiskelijaOpiskelija",
           "ika",
           "sukupNainen"))

#all significant

personal_labels <- c("Nainen ***/",
                     "Ikä, vuosia ***/",
                     "Syntynyt ulkomailla ***/",
                     "Koulutusaste, ref: perusaste",
                     "   Toinen aste ***/",
                     "   Korkea-aste ***/",
                     "   Tutkijakoulutusaste ***/",
                     "Perhetyyppi, ref: asuu yksin",
                     "   Pari, lapsia ***/",
                     "   Yksin asuvat ***/",
                     "   Yksinhuoltaja ***/",
                     "Puoliso töissä ***/",
                     "Hallintaperuste, ref: Omistusasuja",
                     "   Vuokralainen ***/",
                     "   Asumisoikeusasunto ***/",
                     "   Muu hallintaperuste ***/",
                     "Muuttanut ***/",
                     "Työnantajan omistajatyyppi: ref: yksityinen",
                     "   Kunta ***/",
                     "   Valtio ***/",
                     "Työsuhteen kesto, kuukausia")[21:1]

data_salaried$group <- "Palkansaajat"

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
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust = 0)) +
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


################ Henkilökohtaiset, both ###########################


data_palkansaajat <- readRDS("data/oct2/coefficient_plot_data_salaried_ols_ln.rds") %>%
  filter(!grepl("ammattikoodi_k", var)) %>%
  filter(!grepl("toimiala", var)) %>%
  filter(!grepl("kunta", var)) %>%
  filter(!grepl("Intercept", var)) %>%
  filter(!grepl("ika2", var)) %>%
  filter(var != "kturaha_k") %>%
  filter(var != "taajama_k21") %>%
  mutate(coefficient = 100*round(coefficient, digits = 3),
         se = 100*se)
data_palkansaajat$amas <- "Palkansaajat"

data_yrittajat <- readRDS("data/oct2/coefficient_plot_data_self_employed_tobit.rds") %>%
  filter(!grepl("ammattikoodi_k", var)) %>%
  filter(!grepl("toimiala", var)) %>%
  filter(!grepl("aluetyyppi", var)) %>%
  filter(!grepl("yr", var)) %>%
  filter(!grepl("Intercept", var)) %>%
  filter(!grepl("ika2", var)) %>%
  filter(var != "taajama_k21") %>%
  mutate(coefficient = round(coefficient, digits = 1),
         se = 100*se)
data_yrittajat$amas <- "Yrittäjät"

data <- rbind(data_palkansaajat, data_yrittajat)

data$var <- gdata::drop.levels(data$var)
data$var <- factor(data$var,
                   levels = c("auto_k0",
                              "oty1Valtio",
                              "oty1Kunta",
                              "comm_expTRUE",
                              "migr_expTRUE",
                              "tyosuhteen_kesto",
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
                     "Puoliso töissä",
                     "Pari, lapsia (referenssi: pari, ei lapsia)",
                     "Yksin asuvat (referenssi: pari, ei lapsia)",
                     "Yksinhuoltaja (referenssi: pari, ei lapsia)",
                     "Vuokralainen (referenssi: omistusasuja)",
                     "Asumisoikeusasunto (referenssi: omistusasuja)",
                     "Muu hallintaperuste (referenssi: omistusasuja)",
                     "Työsuhteen kesto, päiviä",
                     "Muuttamiskokemus",
                     "Pendelöintikokemus",
                     "Kunta (referenssi: yksityinen)",
                     "Valtio (referenssi: yksityinen)",
                     "Auto käytössä")[20:1]

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
 # geom_point(stat = "identity", color = "#006FB9", size = 10) +
#  geom_text(color = "white", size = 3) +
  coord_flip() +
  facet_wrap(~amas, ncol = 2) +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Kontrolloitu ero referenssikategorioihin tai marginaalivaikutus, %") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Pendelointi/regressiotulokset/personal_both.png",
       width = 8.5, height = 7)

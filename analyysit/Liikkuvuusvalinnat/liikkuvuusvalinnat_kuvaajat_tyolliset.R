library(tidyverse)
leveys = 6


################################# TYÖLLISET ####################################################

################### Selection equation ###########################

marginal_effects_selection_personal <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_employed.rds")


personal_labels = c("Sukupuoli (ref: mies)",
                    "Ikä, vuosia",
                    "Opiskelija, ptoim2, (ref: ei opiskelija)",
                    "Syntynyt ulkomailla (ref: ei syntynyt ulkomailla",
                    "Keskiasteen koulutus (ref: peruskoulu)",
                    "Korkea-asteen koulutus (ref: peruskoulu)",
                    "Tutkija-asteen koulutus (ref: peruskoulu)",
                    "Puoliso töissä",
                    "Pari, lapsia (ref: pari, ei lapsia)",
                    "Asuu yksin (ref: pari, ei lapsia)",
                    "Yksinhuoltaja (ref: pari, ei lapsia)",
                    "Asuu vuokralla (ref: omistuasuja)",
                    "Asumisoikeusasunto (ref: omistusasuja)",
                    "Muu asumismuoto (ref: omistusasuja)",
                    "Muuttokokemus",
                    "Pendelöintikokemus")[16:1]


marginal_effects_selection_personal %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
  #   color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_personal_employed.png",
       width = leveys, height = 6)

##################### labor demand #######################################

marginal_effects_selection_demand <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_demand_employed.rds")

demand_labels = c("Alueen ulkoinen kannustin",
                  "Alueen sisäinen kannustin")[2:1]

marginal_effects_selection_demand %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
  #   color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %") +
  scale_x_discrete(labels = demand_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_demand_employed.png",
       width = leveys, height = 6)

############### Aluetyyppi ###############################################

marginal_effects_selection_aluetyyppit <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_employed.rds")


aluetyyppi_labels = c("Kaupunkien läheinen maaseutu",
                      "Maaseutu",
                      "Pääkaupunkiseutu",
                      "Työssäkäyntikeskus",
                      "Ydinmaaseutu",
                      "Yliopistokaupunki")

marginal_effects_selection_aluetyyppi %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
  #   color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus verrattuna harvaan asuttuun maaseutuun, %") +
  scale_x_discrete(labels = aluetyyppi_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_aluetyyppi_employed.png",
       width = leveys, height = 3)

################## Outcome equation ################################################

# Personal characteristics

marginal_effects_outcome_personal <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

personal_labels = c("Sukupuoli (ref: mies)",
                    "Ikä, vuosia",
                    "Opiskelija, ptoim2, (ref: ei opiskelija)",
                    "Syntynyt ulkomailla (ref: ei syntynyt ulkomailla",
                    "Keskiasteen koulutus (ref: peruskoulu)",
                    "Korkea-asteen koulutus (ref: peruskoulu)",
                    "Tutkija-asteen koulutus (ref: peruskoulu)",
                    "Puoliso töissä",
                    "Pari, lapsia (ref: pari, ei lapsia)",
                    "Asuu yksin (ref: pari, ei lapsia)",
                    "Yksinhuoltaja (ref: pari, ei lapsia)",
                    "Asuu vuokralla (ref: omistuasuja)",
                    "Asumisoikeusasunto (ref: omistusasuja)",
                    "Muu asumismuoto (ref: omistusasuja)",
                    "Muuttokokemus",
                    "Pendelöintikokemus")[16:1]


marginal_effects_outcome_personal %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
  #   color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_personal_employed.png",
       width = leveys, height = 6)

################# Alueiden ominaisuudet ####################

marginal_effects_outcome_alueet <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_employed.rds")

alueet_labels = c("Asuntohintaero",
                  "Etäisyys",
                  "Työn saavutettavuusero",
                  "Työmarkkinoiden kokoero",
                  "Työttömyysaste-ero",
                  "Kohteen vuokra-asujien osuus")

marginal_effects_outcome_alueet %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
  #   color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %") +
  scale_x_discrete(labels = alueet_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_alueet_employed.png",
       width = leveys, height = 4)

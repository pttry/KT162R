library(tidyverse)
# Työttömät

################### Selection equation ###########################


marginal_effects_selection <- readRDS("data/oct15/marginal_effects_selection_equation_unemployed.rds") %>%
                              filter(var != "model")
names(marginal_effects_selection) <- c("coefficient", "var")
marginal_effects_selection$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection$coefficient)), digits = 3)
marginal_effects_selection$var <- as.factor(marginal_effects_selection$var)

# Personal characteristics

marginal_effects_selection_personal <- marginal_effects_selection %>%
                                       filter(var %in% c("ika_t1",
                                                         "sukup_t1Female",
                                                         "pety_t0Couple with children",
                                                         "pety_t0Single parent",
                                                         "pety_t0Living alone",
                                                         "syntyp2Born abroad",
                                                         "opiskelija_t1Student",
                                                         "ututku_aste_t1Secondary education",
                                                         "ututku_aste_t1Tertiary education",
                                                         "ututku_aste_t1Doctoral or equivalent level",
                                                         "hape_t0Rents the dwelling",
                                                         "hape_t0Right of occupancy dwelling",
                                                         "hape_t0Other tenure status",
                                                         "comm_exp_t0TRUE",
                                                         "migr_exp_t0TRUE",
                                                         "spouse_working_t1TRUE"))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)
marginal_effects_selection_personal$var <- factor(marginal_effects_selection_personal$var,
                                                  levels = c("comm_exp_t0TRUE",
                                                             "migr_exp_t0TRUE",
                                                             "hape_t0Other tenure status",
                                                             "hape_t0Right of occupancy dwelling",
                                                             "hape_t0Rents the dwelling",
                                                             "pety_t0Single parent",
                                                             "pety_t0Living alone",
                                                             "pety_t0Couple with children",
                                                             "spouse_working_t1TRUE",
                                                             "ututku_aste_t1Doctoral or equivalent level",
                                                             "ututku_aste_t1Tertiary education",
                                                             "ututku_aste_t1Secondary education",
                                                             "syntyp2Born abroad",
                                                             "opiskelija_t1Student",
                                                             "ika_t1",
                                                             "sukup_t1Female"))

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
                    "Omistuoikeusasunto (ref: omistusasuja)",
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

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_personal_unemployed.png",
       width = 5, height = 6)

##################### labor demand #######################################

marginal_effects_selection_demand <- marginal_effects_selection %>%
  filter(var %in% c("si_index_diff_2",
                    "intraregional_employment_incentive_diff"))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

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

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_demand_unemployed.png",
       width = 5, height = 6)

############### Aluetyyppi ###############################################

marginal_effects_selection_aluetyyppi <- marginal_effects_selection %>%
  filter(grepl("aluetyyppi" , var))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

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

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_aluetyyppi_unemployed.png",
       width = 5, height = 3)

################## Outcome equation ################################################

marginal_effects_outcome <- readRDS("data/oct15/marginal_effects_outcome_equation_unemployed.rds") %>%
  filter(var != "model")
names(marginal_effects_outcome) <- c("coefficient", "var")
marginal_effects_outcome$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome$coefficient)), digits = 3)
marginal_effects_outcome$var <- as.factor(marginal_effects_outcome$var)

# Personal characteristics

marginal_effects_outcome_personal <- marginal_effects_outcome %>%
  filter(var %in% c("ika_t1",
                    "sukup_t1Female",
                    "pety_t0Couple with children",
                    "pety_t0Single parent",
                    "pety_t0Living alone",
                    "syntyp2Born abroad",
                    "opiskelija_t1Student",
                    "ututku_aste_t1Secondary education",
                    "ututku_aste_t1Tertiary education",
                    "ututku_aste_t1Doctoral or equivalent level",
                    "hape_t0Rents the dwelling",
                    "hape_t0Right of occupancy dwelling",
                    "hape_t0Other tenure status",
                    "comm_exp_t0TRUE",
                    "migr_exp_t0TRUE",
                    "spouse_working_t1TRUE"))
marginal_effects_outcome_personal$var <- gdata::drop.levels(marginal_effects_outcome_personal$var)
marginal_effects_outcome_personal$var <- factor(marginal_effects_outcome_personal$var,
                                                  levels = c("comm_exp_t0TRUE",
                                                             "migr_exp_t0TRUE",
                                                             "hape_t0Other tenure status",
                                                             "hape_t0Right of occupancy dwelling",
                                                             "hape_t0Rents the dwelling",
                                                             "pety_t0Single parent",
                                                             "pety_t0Living alone",
                                                             "pety_t0Couple with children",
                                                             "spouse_working_t1TRUE",
                                                             "ututku_aste_t1Doctoral or equivalent level",
                                                             "ututku_aste_t1Tertiary education",
                                                             "ututku_aste_t1Secondary education",
                                                             "syntyp2Born abroad",
                                                             "opiskelija_t1Student",
                                                             "ika_t1",
                                                             "sukup_t1Female"))

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
                    "Omistuoikeusasunto (ref: omistusasuja)",
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

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_personal_unemployed.png",
       width = 5, height = 6)

################# Alueiden ominaisuudet ####################

marginal_effects_outcome_alueet <- marginal_effects_outcome %>%
  filter(var %in% c("etaisyys",
                    "asuntohintaero",
                    "tyomarkkinankokoero",
                    "tyottomyysasteero",
                    "saavutettavuusero",
                    "vuokra_osuus_destin_t1"))
marginal_effects_outcome_alueet$var <- gdata::drop.levels(marginal_effects_outcome_alueet$var)

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

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_alueet_unemployed.png",
       width = 5, height = 4)

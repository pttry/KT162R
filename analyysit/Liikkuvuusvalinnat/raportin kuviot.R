
source("R/set.R")
set_proj()
# Henkilökohtaiset muuttujat

########### SElection

marginal_effects_selection_personal_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_unemployed.rds")
marginal_effects_selection_personal_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_employed.rds")

marginal_effects_selection_personal_employed <- mutate(marginal_effects_selection_personal_employed,
                                                       var = as.character(var),
                                                       var = ifelse(var == "ika_decade_t1", "ika_t1_decade", var))

marginal_effects_selection_personal_unemployed$group <- "Työttömät"
marginal_effects_selection_personal_employed$group <- "Työlliset"

marginal_effects_selection_personal <- rbind(marginal_effects_selection_personal_unemployed,
                                             marginal_effects_selection_personal_employed)

personal_labels = c("Sukupuoli (ref: mies)",
                    "Ikä, kymmeniä vuosia",
                    "Opiskelija, ptoim2, (ref: ei opiskelija)",
                    "Syntynyt ulkomailla (ref: ei syntynyt ulkomailla)",
                    "Keskiasteen koulutus (ref: peruskoulu)",
                    "Korkea-asteen koulutus (ref: peruskoulu)",
                    "Tutkija-asteen koulutus (ref: peruskoulu)",
                    "Puoliso töissä",
                    "Pari, ei lapsia (ref: asuu yksin)",
                    "Pari, lapsia (ref: asuu yksin)",
                    "Yksinhuoltaja (ref: asuu yksin)",
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
  facet_wrap(~group) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/henkilökohtaiset_muuttujat_selection.png")


######## Aluetyyppi

aluetyyppi_labels = c("Kaupunkien läheinen maaseutu",
                      "Harvaan asuttu maaseutu",
                      "Pääkaupunkiseutu",
                      "Työssäkäyntikeskus",
                      "Ydinmaaseutu",
                      "Yliopistokaupunki")

marginal_effects_selection_aluetyyppi_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_unemployed.rds")
marginal_effects_selection_aluetyyppi_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_employed.rds")

marginal_effects_selection_aluetyyppi_unemployed$group <- "Työttömät"
marginal_effects_selection_aluetyyppi_employed$group <- "Työlliset"

marginal_effects_selection_aluetyyppi <- rbind(marginal_effects_selection_aluetyyppi_unemployed,
                                               marginal_effects_selection_aluetyyppi_employed)

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
  facet_wrap(~group) +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus verrattuna muihin kaupunkeihin, %-yksikköä") +
  scale_x_discrete(labels = aluetyyppi_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_aluetyyppi.png",
       width = 8, height = 3)


############ outcome ################################


marginal_effects_outcome_personal_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_unemployed.rds")
marginal_effects_outcome_personal_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

marginal_effects_outcome_personal_employed <- mutate(marginal_effects_outcome_personal_employed,
                                                       var = as.character(var),
                                                       var = ifelse(var == "ika_decade_t1", "ika_t1_decade", var))

marginal_effects_outcome_personal_unemployed$group <- "Työttömät"
marginal_effects_outcome_personal_employed$group <- "Työlliset"

marginal_effects_outcome_personal <- rbind(marginal_effects_outcome_personal_unemployed,
                                             marginal_effects_outcome_personal_employed)

personal_labels = c("Sukupuoli (ref: mies)",
                    "Ikä, kymmeniä vuosia",
                    "Opiskelija, ptoim2, (ref: ei opiskelija)",
                    "Syntynyt ulkomailla (ref: ei syntynyt ulkomailla)",
                    "Keskiasteen koulutus (ref: peruskoulu)",
                    "Korkea-asteen koulutus (ref: peruskoulu)",
                    "Tutkija-asteen koulutus (ref: peruskoulu)",
                    "Puoliso töissä",
                    "Pari, ei lapsia (ref: asuu yksin)",
                    "Pari, lapsia (ref: asuu yksin)",
                    "Yksinhuoltaja (ref: asuu yksin)",
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
  facet_wrap(~group) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/henkilökohtaiset_muuttujat_outcome.png")

############# Alueiden ominaisuudet ##############################

marginal_effects_outcome_alueet_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_unemployed.rds")
marginal_effects_outcome_alueet_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_employed.rds")

marginal_effects_outcome_alueet_unemployed$group <- "Työttömät"
marginal_effects_outcome_alueet_employed$group <- "Työlliset"

marginal_effects_outcome_alueet <- rbind(marginal_effects_outcome_alueet_unemployed,
                                         marginal_effects_outcome_alueet_employed)

alueet_labels = c("Asuntohintaero",
                  "Etäisyys",
                  "Työn saavutettavuusero",
                  "Työmarkkinoiden kokoero",
                  "Työttömyysaste-ero",
                  "Kohteen vuokra-asujien osuus")

marginal_effects_outcome_alueet %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black", size = 1) +
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
  facet_wrap(~group) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = alueet_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_alueet_unemployed.png",
       width = 6, height = 3)


############ Aluetyyppi

marginal_effects_outcome_aluetyyppi_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_unemployed.rds")
marginal_effects_outcome_aluetyyppi_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_employed.rds")

marginal_effects_outcome_aluetyyppi_unemployed$lahde_kohde <- c(rep("Asuinpaikan kunnan tyyppi", 6), rep("Työpaikan kunnan tyyppi", 6))
marginal_effects_outcome_aluetyyppi_employed$lahde_kohde <- c(rep("Asuinpaikan kunnan tyyppi", 6), rep("Työpaikan kunnan tyyppi", 6))

marginal_effects_outcome_aluetyyppi_unemployed$group <- "Työttömät"
marginal_effects_outcome_aluetyyppi_employed$group <- "Työlliset"

marginal_effects_outcome_aluetyyppi <- rbind(marginal_effects_outcome_aluetyyppi_unemployed,
                                             marginal_effects_outcome_aluetyyppi_employed)

marginal_effects_outcome_aluetyyppi$var <- rep(aluetyyppi_labels, 4)

marginal_effects_outcome_aluetyyppi %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black", size = 1) +
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
  facet_grid(lahde_kohde~group) +
  theme_light() +
  labs(x = NULL,
       y = "Kontrolloitu ero muihin kaupunkeihin, %-yksikköä") +
  scale_x_discrete(labels = aluetyyppi_labels)

ggsave("analyysit/Liikkuvuusvalinnat/Kuvaajat/marginal_effects_outcome_aluetyyppi_unemployed.png",
       width = 10, height = 4)


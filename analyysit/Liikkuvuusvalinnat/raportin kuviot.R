
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

personal_labels = c("Nainen ***/***",
                    "Ikä, kymmeniä vuosia ***/***",
                    "Opiskelija ***/***",
                    "Syntynyt ulkomailla /***",
                    "Koulutusaste, ref: perusaste",
                    "   Keskiasteen koulutus */***",
                    "   Korkea-asteen koulutus ***/***",
                    "   Tutkija-asteen koulutus ***/***",
<<<<<<< HEAD
                    "Käytettävissä olevat tulot ***/***",
=======
                    "Käyettävissä olevat tulot ******",
>>>>>>> 9c7598de7c4f4886a48d2bcd3b7c7fa948f83cc4
                    "Perhetyyppi, ref: asuu yksin",
                    "   Pari, ei lapsia /**",
                    "   Pari, lapsia */*",
                    "   Yksinhuoltaja /",
                    "Puoliso töissä ***/*",
                    "Hallintaperuste, ref: omistusasunto",
                    "   Asuu vuokralla ***/***",
                    "   Asumisoikeusasunto */",
                    "   Muu asumismuoto /",
                    "Muuttokokemus ***/***",
                    "Pendelöintikokemus ***/***")[20:1]

# ***p<0.01, **p<0.05, *p<0.1

marginal_effects_selection_personal %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  #geom_errorbar(aes(x = var, ymin = coefficient - 2*marginal_effects_selection_personal$se,
  #                           ymax = coefficient + 2*marginal_effects_selection_personal$se),
     color = "red", size = 1.2, linetype = 2) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  facet_wrap(~group) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust=0)) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = personal_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/henkilökohtaiset_muuttujat_selection.png")


######## Aluetyyppi

aluetyyppi_labels = c("Kaupunkien läheinen maaseutu",       # /*
                      "Harvaan asuttu maaseutu",            # ***/***
                      "Pääkaupunkiseutu",                   # ***/***
                      "Työssäkäyntikeskus",                 # **/**
                      "Ydinmaaseutu",                       # ***/***
                      "Yliopistokaupunki")                  # */

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
  geom_errorbar(aes(x = var, ymin = coefficient - 2*marginal_effects_selection_aluetyyppi$se,
                             ymax = coefficient + 2*marginal_effects_selection_aluetyyppi$se),
<<<<<<< HEAD
     color = "red", size = 0.7, linetype = 1, width = 0.3) +
=======
     color = "red", size = 0.7, linetype = 1) +
>>>>>>> 9c7598de7c4f4886a48d2bcd3b7c7fa948f83cc4
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  facet_wrap(~group) +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust = 0)) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus verrattuna aluetyyppiin muut kaupungit, %-yksikköä") +
  scale_x_discrete(labels = aluetyyppi_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_selection_aluetyyppi.png",
       width = 8, height = 3)


############ outcome ################################


marginal_effects_outcome_personal_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_unemployed.rds")
marginal_effects_outcome_personal_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

marginal_effects_outcome_personal_unemployed$group <- "Työttömät"
marginal_effects_outcome_personal_employed$group <- "Työlliset"

marginal_effects_outcome_personal <- rbind(marginal_effects_outcome_personal_unemployed,
                                             marginal_effects_outcome_personal_employed)

personal_labels = c("Nainen ***/***",
                    "Ikä, kymmeniä vuosia ***/***",
                    "Opiskelija ***/**",
                    "Syntynyt ulkomailla /",
                    "Koulutusaste, ref: perusaste",
                    "   Keskiasteen koulutus /",
                    "   Korkea-asteen koulutus ***/***",
                    "   Tutkija-asteen koulutus /*",
                    "Käytettävissä olevat tulot /***",
                    "Perhetyyppi, ref: asuu yksin",
                    "   Pari, ei lapsia /*",
                    "   Pari, lapsia /",
                    "   Yksinhuoltaja /",
                    "Puoliso töissä ***/***",
                    "Hallintaperuste, ref: omistusasunto",
                    "   Asuu vuokralla ***/",
                    "   Asumisoikeusasunto **/",
                    "   Muu asumismuoto */",
                    "Muuttokokemus ***/***",
                    "Pendelöintikokemus /")[20:1]


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
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust=0)) +
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

alueet_labels = c("Palkkaero",
                  "Työn saavutettavuusero",
                  "Työttömyysaste-ero",
                  "Työmarkkinoiden kokoero",
                  "Kohteen vuokra-asumisen osuus",
                  "Asuntohintaero",
                  "Etäisyys, 10 km")


marginal_effects_outcome_alueet %>%
  ggplot(aes(y = coefficient, x = var, label = coefficient)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = var,
                   yend = coefficient,
                   xend = var),
               color = "#0ABBEC",
               size = 3) +
  geom_errorbar(aes(x = var, ymin = coefficient - 2*marginal_effects_outcome_alueet$se,
                             ymax = coefficient + 2*marginal_effects_outcome_alueet$se),
     color = "red", size = 0.7, linetype = 1, width = 0.6) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  facet_wrap(~group) +
  coord_flip(ylim = c(-1,1)) +
  theme_light() +
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust =0)) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = alueet_labels)


ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_alueet_unemployed.png",
       width = 8, height = 4)


############ Aluetyyppi

marginal_effects_outcome_aluetyyppi_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_unemployed.rds")
marginal_effects_outcome_aluetyyppi_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_employed.rds")

marginal_effects_outcome_aluetyyppi_unemployed$lahde_kohde <- c(rep("Asuinpaikan kunnan tyyppi", 6), rep("Työpaikan kunnan tyyppi", 6))
marginal_effects_outcome_aluetyyppi_employed$lahde_kohde <- c(rep("Asuinpaikan kunnan tyyppi", 6), rep("Työpaikan kunnan tyyppi", 6))

marginal_effects_outcome_aluetyyppi_unemployed$group <- "Työttömät"
marginal_effects_outcome_aluetyyppi_employed$group <- "Työlliset"

marginal_effects_outcome_aluetyyppi <- rbind(marginal_effects_outcome_aluetyyppi_unemployed,
                                             marginal_effects_outcome_aluetyyppi_employed)
aluetyyppi_labels = c("Kaupunkien läheinen maaseutu",
                      "Harvaan asuttu maaseutu",
                      "Pääkaupunkiseutu",
                      "Työssäkäyntikeskus",
                      "Ydinmaaseutu",
                      "Yliopistokaupunki")
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
  geom_errorbar(aes(x = var, ymin = coefficient - 2*marginal_effects_outcome_aluetyyppi$se,
                             ymax = coefficient + 2*marginal_effects_outcome_aluetyyppi$se),
     color = "red", size = 0.7, width = 0.3, linetype = 1) +
  geom_point(stat = "identity", color = "#006FB9", size = 10) +
  geom_text(color = "white", size = 3) +
  coord_flip() +
  facet_grid(lahde_kohde~group) +
  theme_light() +
  labs(x = NULL,
       y = "Kontrolloitu ero aluetyyppiin kaupungit, %-yksikköä") +
  scale_x_discrete(labels = aluetyyppi_labels)

ggsave("analyysit/Liikkuvuusvalinnat/Kuvaajat/marginal_effects_outcome_aluetyyppi_unemployed.png",
       width = 10, height = 6)

############# Työpaikan ominaisuudet ##############################

marginal_effects_outcome_job_unemployed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_job_unemployed.rds")
marginal_effects_outcome_job_employed <- readRDS("data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_job_employed.rds")

marginal_effects_outcome_job_unemployed$group <- "Työttömät"
marginal_effects_outcome_job_employed$group <- "Työlliset"

marginal_effects_outcome_job <- rbind(marginal_effects_outcome_job_unemployed,
                                         marginal_effects_outcome_job_employed)

job_labels = c("Toimipaikan henkilömäärä, ref: pieni yritys",
               "   Keskisuuri yritys",
               "   Suuri yritys",
               "Toimipaikan liikevaihto, ref: - 10 000 000",
               "   10 000 000 - 40 000 000",
               "   40 000 000 - ",
               "Omistajatyyppi, ref: yksityinen",
               "   Valtio",
               "   Kunta")[9:1]

marginal_effects_outcome_job %>%
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
  theme(text = element_text(size = 10, family = "sans"),
        axis.text.y = element_text(hjust =0)) +
  labs(x = NULL,
       y = "Keskimääräinen marginaalivaikutus, %-yksikköä") +
  scale_x_discrete(labels = job_labels)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/marginal_effects_outcome_job_unemployed.png",
       width = 8, height = 4)

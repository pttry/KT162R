library(tidyverse)


############################## TYÖTTÖMÄT ##########################################

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

saveRDS(marginal_effects_selection_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_unemployed.rds")

################## Outcome equation ###################################

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

saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_unemployed.rds")

############################### TYÖLLISET ###################################

################### Selection equation ########################

marginal_effects_selection <- readRDS("data/oct15/marginal_effects_selection_equation_employed.rds") %>%
  filter(var != "model")
names(marginal_effects_selection) <- c("coefficient", "var")
marginal_effects_selection$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection$coefficient)), digits = 3)
marginal_effects_selection$var <- as.factor(marginal_effects_selection$var)


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

saveRDS(marginal_effects_selection_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_employed.rds")


#################### Outcome equation ################################

marginal_effects_outcome <- readRDS("data/oct15/marginal_effects_outcome_equation_employed.rds") %>%
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

saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

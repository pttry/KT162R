library(tidyverse)


############################## TYÖTTÖMÄT ##########################################

################### Selection equation ###########################


marginal_effects_selection <- readRDS("data/nov12/liikkuvuusmalli/unemployed/marginal_effects_selection_equation_unemployed.rds")
names(marginal_effects_selection) <- c("coefficient", "var")
marginal_effects_selection$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection$coefficient)), digits = 3)
marginal_effects_selection$var <- as.factor(marginal_effects_selection$var)

# Personal characteristics

marginal_effects_selection_personal <- marginal_effects_selection %>%
  filter(var %in% c("ika_t1_decade",
                    "sukup_t1Female",
                    "pety_t0Couple without children",
                    "pety_t0Couple with children",
                    "pety_t0Single parent",
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
                                                             "pety_t0Couple with children",
                                                             "pety_t0Couple without children",
                                                             "spouse_working_t1TRUE",
                                                             "ututku_aste_t1Doctoral or equivalent level",
                                                             "ututku_aste_t1Tertiary education",
                                                             "ututku_aste_t1Secondary education",
                                                             "syntyp2Born abroad",
                                                             "opiskelija_t1Student",
                                                             "ika_t1_decade",
                                                             "sukup_t1Female"))

saveRDS(marginal_effects_selection_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_unemployed.rds")

# Labor demand

marginal_effects_selection_demand <- marginal_effects_selection %>%
  filter(var %in% c("intra_si_index_2",
                    "inter_si_index_2",
                    "intra_E_ind",
                    "inter_E_ind"))

saveRDS(marginal_effects_selection_demand,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_demand_unemployed.rds")

# Aluetyyppi

marginal_effects_selection_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/unemployed/marginal_effects_selection_equation_at_unemployed.rds")
namesmarginal_effects_selection_aluetyyppi <- c("coefficient", "var")
marginal_effects_selection_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_aluetyyppi$coefficient)), digits = 3)
marginal_effects_selection_aluetyyppi$var <- as.factor(marginal_effects_selection_aluetyyppi$var)
marginal_effects_selection_aluetyyppi$var <- gdata::drop.levels(marginal_effects_selection_aluetyyppi$var)

saveRDS(marginal_effects_selection_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_unemployed.rds")

################## Outcome equation ###################################

marginal_effects_outcome <- readRDS("data/nov12/liikkuvuusmalli/unemployed/marginal_effects_outcome_equation_unemployed.rds") %>%
  filter(var != "model")
names(marginal_effects_outcome) <- c("coefficient", "var")
marginal_effects_outcome$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome$coefficient)), digits = 3)
marginal_effects_outcome$var <- as.factor(marginal_effects_outcome$var)

# Personal characteristics

marginal_effects_outcome_personal <- marginal_effects_outcome %>%
  filter(var %in% c("ika_t1_decade",
                    "sukup_t1Female",
                    "pety_t0Couple without children",
                    "pety_t0Couple with children",
                    "pety_t0Single parent",
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
                                                           "pety_t0Couple with children",
                                                           "pety_t0Couple without children",
                                                           "spouse_working_t1TRUE",
                                                           "ututku_aste_t1Doctoral or equivalent level",
                                                           "ututku_aste_t1Tertiary education",
                                                           "ututku_aste_t1Secondary education",
                                                           "syntyp2Born abroad",
                                                           "opiskelija_t1Student",
                                                           "ika_t1_decade",
                                                           "sukup_t1Female"))



saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_unemployed.rds")

# Region characteristics

marginal_effects_outcome_alueet <- marginal_effects_outcome %>%
  filter(var %in% c("etaisyys",
                    "asuntohintaero",
                    "tyomarkkinankokoero",
                    "tyottomyysasteero",
                    "saavutettavuusero",
                    "vuokra_osuus_destin_t1"))
marginal_effects_outcome_alueet$var <- gdata::drop.levels(marginal_effects_outcome_alueet$var)

saveRDS(marginal_effects_outcome_alueet,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_unemployed.rds")

# aluetyyppi

marginal_effects_outcome_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/unemployed/marginal_effects_outcome_equation_at_unemployed.rds")
namesmarginal_effects_outcome_aluetyyppi <- c("coefficient", "var")
marginal_effects_outcome_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_aluetyyppi$coefficient)), digits = 3)
marginal_effects_outcome_aluetyyppi$var <- as.factor(marginal_effects_outcome_aluetyyppi$var)
marginal_effects_outcome_aluetyyppi$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi$var)

saveRDS(marginal_effects_outcome_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_unemployed.rds")





############################### TYÖLLISET ###################################

################### Selection equation ###########################


marginal_effects_selection <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_selection_equation_employed.rds")
names(marginal_effects_selection) <- c("coefficient", "var")
marginal_effects_selection$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection$coefficient)), digits = 3)
marginal_effects_selection$var <- as.factor(marginal_effects_selection$var)

# Personal characteristics

marginal_effects_selection_personal <- marginal_effects_selection %>%
  filter(var %in% c("ika_decade_t1",
                    "sukup_t1Female",
                    "pety_t0Couple without children",
                    "pety_t0Couple with children",
                    "pety_t0Single parent",
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
                                                             "pety_t0Couple with children",
                                                             "pety_t0Couple without children",
                                                             "spouse_working_t1TRUE",
                                                             "ututku_aste_t1Doctoral or equivalent level",
                                                             "ututku_aste_t1Tertiary education",
                                                             "ututku_aste_t1Secondary education",
                                                             "syntyp2Born abroad",
                                                             "opiskelija_t1Student",
                                                             "ika_decade_t1",
                                                             "sukup_t1Female"))


saveRDS(marginal_effects_selection_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_personal_employed.rds")

# Labor demand

marginal_effects_selection_demand <- marginal_effects_selection %>%
  filter(var %in% c("intra_si_index_2",
                    "inter_si_index_2",
                    "intra_E_ind",
                    "inter_E_ind"))

saveRDS(marginal_effects_selection_demand,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_demand_employed.rds")

# Aluetyyppi

marginal_effects_selection_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_selection_equation_at_employed.rds")
namesmarginal_effects_selection_aluetyyppi <- c("coefficient", "var")
marginal_effects_selection_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_aluetyyppi$coefficient)), digits = 3)
marginal_effects_selection_aluetyyppi$var <- as.factor(marginal_effects_selection_aluetyyppi$var)
marginal_effects_selection_aluetyyppi$var <- gdata::drop.levels(marginal_effects_selection_aluetyyppi$var)

saveRDS(marginal_effects_selection_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_employed.rds")

################## Outcome equation ###################################

marginal_effects_outcome <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_outcome_equation_employed.rds") %>%
  filter(var != "model")
names(marginal_effects_outcome) <- c("coefficient", "var")
marginal_effects_outcome$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome$coefficient)), digits = 3)
marginal_effects_outcome$var <- as.factor(marginal_effects_outcome$var)

# Personal characteristics

marginal_effects_outcome_personal <- marginal_effects_outcome %>%
  filter(var %in% c("ika_decade_t1",
                    "sukup_t1Female",
                    "pety_t0Couple without children",
                    "pety_t0Couple with children",
                    "pety_t0Single parent",
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
                                                           "pety_t0Couple with children",
                                                           "pety_t0Couple without children",
                                                           "spouse_working_t1TRUE",
                                                           "ututku_aste_t1Doctoral or equivalent level",
                                                           "ututku_aste_t1Tertiary education",
                                                           "ututku_aste_t1Secondary education",
                                                           "syntyp2Born abroad",
                                                           "opiskelija_t1Student",
                                                           "ika_decade_t1",
                                                           "sukup_t1Female"))

saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

# Region characteristics

marginal_effects_outcome_alueet <- marginal_effects_outcome %>%
  filter(var %in% c("etaisyys",
                    "asuntohintaero",
                    "tyomarkkinankokoero",
                    "tyottomyysasteero",
                    "saavutettavuusero",
                    "vuokra_osuus_destin_t1"))
marginal_effects_outcome_alueet$var <- gdata::drop.levels(marginal_effects_outcome_alueet$var)

saveRDS(marginal_effects_outcome_alueet,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_employed.rds")

# aluetyyppi

marginal_effects_outcome_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_outcome_equation_at_employed.rds")
namesmarginal_effects_outcome_aluetyyppi <- c("coefficient", "var")
marginal_effects_outcome_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_aluetyyppi$coefficient)), digits = 3)
marginal_effects_outcome_aluetyyppi$var <- as.factor(marginal_effects_outcome_aluetyyppi$var)
marginal_effects_outcome_aluetyyppi$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi$var)

saveRDS(marginal_effects_outcome_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_employed.rds")



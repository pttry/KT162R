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

# Labor demand

marginal_effects_selection_demand <- marginal_effects_selection %>%
  filter(var %in% c("si_index_diff_2",
                    "intraregional_employment_incentive_diff"))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

saveRDS(marginal_effects_selection_demand,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_demand_unemployed.rds")

# Aluetyyppi

marginal_effects_selection_aluetyyppi <- marginal_effects_selection %>%
  filter(grepl("aluetyyppi" , var))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

saveRDS(marginal_effects_selection_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_unemployed.rds")

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

# Kohteen aluetyyppi

marginal_effects_outcome_aluetyyppi_tp <- marginal_effects_outcome %>%
                                          filter(grepl("aluetyyppi_tp", var))
marginal_effects_outcome_aluetyyppi_tp$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi_tp$var)

saveRDS(marginal_effects_outcome_aluetyyppi_tp,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_tp_unemployed.rds")

# Lähteen aluetyyppi

marginal_effects_outcome_aluetyyppi_ap <- marginal_effects_outcome %>%
  filter(grepl("aluetyyppi_ap", var))
marginal_effects_outcome_aluetyyppi_ap$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi_ap$var)

saveRDS(marginal_effects_outcome_aluetyyppi_ap,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_ap_unemployed.rds")


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

# Aluetyyppi

marginal_effects_selection_aluetyyppi <- marginal_effects_selection %>%
  filter(grepl("aluetyyppi" , var))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

saveRDS(marginal_effects_selection_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_aluetyyppi_employed.rds")


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

# Labor demand

marginal_effects_selection_demand <- marginal_effects_selection %>%
  filter(var %in% c("si_index_diff_2",
                    "intraregional_employment_incentive_diff"))
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)

saveRDS(marginal_effects_selection_demand,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_selection_demand_employed.rds")

# Kohteen aluetyyppi

marginal_effects_outcome_aluetyyppi_tp <- marginal_effects_outcome %>%
  filter(grepl("aluetyyppi_tp", var))
marginal_effects_outcome_aluetyyppi_tp$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi_tp$var)

saveRDS(marginal_effects_outcome_aluetyyppi_tp,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_tp_employed.rds")

# Lähteen aluetyyppi

marginal_effects_outcome_aluetyyppi_ap <- marginal_effects_outcome %>%
  filter(grepl("aluetyyppi_ap", var))
marginal_effects_outcome_aluetyyppi_ap$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi_ap$var)

saveRDS(marginal_effects_outcome_aluetyyppi_ap,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_ap_employed.rds")


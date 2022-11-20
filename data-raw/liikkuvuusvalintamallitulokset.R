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
dummy_titles <- data.frame(var = c("pety",
                                   "ututku_aste",
                                   "hape"),
                           coefficient = rep(NA, 3),
                           se = rep(NA, 3))

vars <- c("ika_t1_decade",
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
          "spouse_working_t1TRUE")

marginal_effects_selection_personal <- data.frame(var = c(vars, "ln_disp_inc_t0"),
                                                  coefficient = c(-1.877662e-02, -1.40253e-03, 3.504731e-03,
                                                                  2.490890e-03, -2.007966e-03, -3.602751e-03,
                                                                  -3.184202e-03, 1.12161e-02, 3.807244e-02,
                                                                  7.064802e-02, -5.487572e-03, -5.546047e-03,
                                                                  3.298334e-03, 3.831141e-02, 1.788559e-2,
                                                                  2.378705e-03, 2.049013e-02),
                                                  se = c(6.627779e-04, 1.211526e-03, 1.930851e-03,
                                                       1.394305e-03, 1.989112e-03, 3.314224e-03,
                                                       1.169685e-03, 1.031573e-03, 2.153423e-03,
                                                       9.277252e-03, 1.386705e-03, 2.618073e-03,
                                                       3.016382e-03, 2.903143e-03, 1.562585e-03,
                                                       1.589220e-03, 1.928470e-03))

marginal_effects_selection_personal <- rbind(marginal_effects_selection_personal, dummy_titles)
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)
marginal_effects_selection_personal$var <- factor(marginal_effects_selection_personal$var,
                                                levels = c("comm_exp_t0TRUE",
                                                           "migr_exp_t0TRUE",
                                                           "hape_t0Other tenure status",
                                                           "hape_t0Right of occupancy dwelling",
                                                           "hape_t0Rents the dwelling",
                                                           "hape",
                                                           "spouse_working_t1TRUE",
                                                           "pety_t0Single parent",
                                                           "pety_t0Couple with children",
                                                           "pety_t0Couple without children",
                                                           "pety",
                                                           "ln_disp_inc_t0",
                                                           "ututku_aste_t1Doctoral or equivalent level",
                                                           "ututku_aste_t1Tertiary education",
                                                           "ututku_aste_t1Secondary education",
                                                           "ututku_aste",
                                                           "syntyp2Born abroad",
                                                           "opiskelija_t1Student",
                                                           "ika_t1_decade",
                                                           "sukup_t1Female"))
marginal_effects_selection_personal$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_personal$coefficient)), digits = 3)
marginal_effects_selection_personal$se <- 100*marginal_effects_selection_personal$se
marginal_effects_selection_personal$var <- as.factor(marginal_effects_selection_personal$var)
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

names(marginal_effects_selection_aluetyyppi) <- c("coefficient", "var")

namesmarginal_effects_selection_aluetyyppi <- c("coefficient", "var")


vars <- marginal_effects_selection_aluetyyppi$var

marginal_effects_selection_aluetyyppi <- data.frame(var = vars,

                                                    coefficient = c(1.1243e-02, 3.0495e-02,
                                                                    -2.6491e-02, 1.7552e-02,
                                                                    3.5075e-02, 6.2339e-03),
                                                    se = c(6.4918e-03, 9.0640e-03, 4.0467e-03,
                                                           7.6186e-03, 8.6280e-03, 5.7693e-03))

# labor demand controlled
marginal_effects_selection_aluetyyppi <- data.frame(var = vars,
                                                    coefficient = c(1.4686e-02, 5.0140e-02,
                                                                    -9.0932e-02, 2.7875e-02,
                                                                    4.2716e-02, 2.5024e-02),
                                                    se = c(6.3541e-03, 8.0784e-03, 6.0275e-03,
                                                           6.8487e-03, 7.0194e-03, 3.6271e-03))

                                                    coefficient = c(1.1234e-02, 3.0495e-02, -2.6491e-02, 1.7552e-02,
                                                                    3.5075e-02, 6.2339e-03),
                                                    se = c(6.4918e-03, 9.0640e-03, 4.0467e-03, 7.6186e-03,
                                                           8.6280e-03, 5.7693e-03))



marginal_effects_selection_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_aluetyyppi$coefficient)), digits = 3)
marginal_effects_selection_aluetyyppi$se <- 100*marginal_effects_selection_aluetyyppi$se
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
dummy_titles <- data.frame(var = c("pety",
                                   "ututku_aste",
                                   "hape"),
                           coefficient = rep(NA, 3))

vars <- c("ika_decade_t1", "sukup_t1Female", "pety_t0Couple without children",
          "pety_t0Couple with children", "pety_t0Single parent", "hape_t0Rents the dwelling",
          "hape_t0Right of occupancy dwelling", "hape_t0Other tenure status", "syntyp2Born abroad",
          "opiskelija_t1Student", "ututku_aste_t1Secondary education", "ututku_aste_t1Tertiary education",
          "ututku_aste_t1Doctoral or equivalent level", "comm_exp_t0TRUE", "migr_exp_t0TRUE",
          "spouse_working_t1TRUE", "ln_disp_inc_t0")

marginal_effects_outcome_personal <- data.frame(var = vars,
                                                coefficient = c(-4.741109e-02, 3.943048e-02, 1.603586e-02,
                                                                -1.243194e-03, -2.003363e-03, 7.270708e-02,
                                                                6.777052e-02, 1.653111e-03, 1.103401e-02,
                                                                -4.706316e-02, 3.738514e-03, 3.894036e-02,
                                                                6.167791e-02, -1.66941e-02, 9.698819e-02,
                                                                -6.77333e-02, -2.707670e-02))

marginal_effects_outcome_personal <- rbind(marginal_effects_outcome_personal, dummy_titles)
marginal_effects_outcome_personal$var <- gdata::drop.levels(marginal_effects_outcome_personal$var)
marginal_effects_outcome_personal$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_personal$coefficient)), digits = 3)

marginal_effects_outcome_personal$var <- factor(marginal_effects_outcome_personal$var,
                                                levels = c("comm_exp_t0TRUE",
                                                           "migr_exp_t0TRUE",
                                                           "hape_t0Other tenure status",
                                                           "hape_t0Right of occupancy dwelling",
                                                           "hape_t0Rents the dwelling",
                                                           "hape",
                                                           "spouse_working_t1TRUE",
                                                           "pety_t0Single parent",
                                                           "pety_t0Couple with children",
                                                           "pety_t0Couple without children",
                                                           "pety",
                                                           "ln_disp_inc_t0",
                                                           "ututku_aste_t1Doctoral or equivalent level",
                                                           "ututku_aste_t1Tertiary education",
                                                           "ututku_aste_t1Secondary education",
                                                           "ututku_aste",
                                                           "syntyp2Born abroad",
                                                           "opiskelija_t1Student",
                                                           "ika_decade_t1",
                                                           "sukup_t1Female"))



saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_unemployed.rds")

# Region characteristics

marginal_effects_outcome_regions <- marginal_effects_outcome %>%
  filter(var %in% c("etaisyys",
                    "asuntohintaero",
                    "tyomarkkinankokoero",
                    "tyottomyysasteero",
                    "saavutettavuusero",
                    "vuokra_osuus_destin_t1"))
marginal_effects_outcome_regions$var <- gdata::drop.levels(marginal_effects_outcome_alueet$var)

marginal_effects_outcome_regions <- data.frame(var = c("etaisyys_10", "asuntohintaero", "tyomarkkinankokoero",
                                                       "tyottomyysasteero", "saavutettavuusero", "vuokra_osuus_destin_t1",
                                                       "palkkaero"),
                                               coefficient = c(3.916020e-03, -7.096974e-03, 9.210507e-03,
                                                               -2.359477e-03, 1.658272e-06, 1.305567e-03,
                                                               5.206028e-03),
                                               se = c(7.610359e-04, 1.284528e-02, 3.788506e-03,
                                                      1.307371e-03, 3.263578e-06, 6.309096e-04,
                                                      4.466116e-02))

marginal_effects_outcome_regions$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_regions$coefficient)), digits = 3)
marginal_effects_outcome_regions$se <- 100*marginal_effects_outcome_regions$se
marginal_effects_outcome_regions$var <- factor(marginal_effects_outcome_regions$var,
                                               levels = c("palkkaero",
                                                          "saavutettavuusero",
                                                          "tyottomyysasteero",
                                                          "tyomarkkinankokoero",
                                                          "vuokra_osuus_destin_t1",
                                                          "asuntohintaero",
                                                          "etaisyys_10"))
saveRDS(marginal_effects_outcome_regions,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_unemployed.rds")



# aluetyyppi

marginal_effects_outcome_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/unemployed/marginal_effects_outcome_equation_at_unemployed.rds")
namesmarginal_effects_outcome_aluetyyppi <- c("coefficient", "var")

vars <- marginal_effects_outcome_aluetyyppi$var


marginal_effects_outcome_aluetyyppi <- data.frame(var = vars,
                                                  coefficient = c(0.00370, -0.00046, -0.02752, 0.01697,
                                                                  0.0116989, 0.0213929, -0.00688, -0.020255,
                                                                  0.042912, 0.023541730, -0.0150572, 0.0235080),
                                                  se = c(0.0134142546, 0.0162713122, 0.0164253163, 0.0116022675,
                                                         0.0143172467, 0.0114241347, 0.0126134210, 0.0147720711,
                                                         0.0142250196, 0.0124274273, 0.0116446262, 0.0123219621),
                                                  coefficient = c(-0.00370, -0.00464, -0.02753, 0.01700,
                                                                  0.01170, 0.02139, -0.006689, -0.0203,
                                                                  0.04291, 0.0235, -0.01506, 0.02350))



marginal_effects_outcome_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_aluetyyppi$coefficient)), digits = 3)
marginal_effects_outcome_aluetyyppi$se <- 100 * marginal_effects_outcome_aluetyyppi$se
marginal_effects_outcome_aluetyyppi$var <- as.factor(marginal_effects_outcome_aluetyyppi$var)
marginal_effects_outcome_aluetyyppi$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi$var)

saveRDS(marginal_effects_outcome_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_unemployed.rds")


# Työpaikan ominaisuust

marginal_effects_outcome_job <- data.frame(var = c("tp1_hl_t1Medium size enterprise", "tp1_hl_t1_Large enterprise",
                                                   "tp1_lvl_t110,000,000 - 40,000,000", "tp1_lvl_t140,000,000 - ",
                                                   "oty1_t1State", "oty1_t1Municipality"),
                                           coefficient = c(0.012225070, 0.097857, -0.0121780,
                                                           -0.017783, 0.1030199, 0.0120578),
                                           se = c(0.0111665, 0.0296358, 0.0116386, 0.016853,
                                                  0.0329451, 0.034378))


marginal_effects_outcome_job$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_job$coefficient)), digits = 3)
marginal_effects_outcome_job$se <- 100*marginal_effects_outcome_job$se

dummy_titles <- data.frame(var = c("lvl",
                                   "hl",
                                   "oty1"),
                           coefficient = rep(NA, 3),
                           se = rep(NA, 3))
marginal_effects_outcome_job <- rbind(marginal_effects_outcome_job, dummy_titles)
marginal_effects_outcome_job$var <- gdata::drop.levels(marginal_effects_outcome_job$var)
marginal_effects_outcome_job$var <- factor(marginal_effects_outcome_job$var,
                                           levels = c("oty1_t1Municipality",
                                                      "oty1_t1State",
                                                      "oty1",
                                                      "tp1_lvl_t140,000,000 - ",
                                                      "tp1_lvl_t110,000,000 - 40,000,000",
                                                      "lvl",
                                                      "tp1_hl_t1_Large enterprise",
                                                      "tp1_hl_t1Medium size enterprise",
                                                      "hl"))

saveRDS(marginal_effects_outcome_job,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_job_unemployed.rds")

############################### TYÖLLISET ###################################

################### Selection equation ###########################


marginal_effects_selection <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_selection_equation_employed.rds")
names(marginal_effects_selection) <- c("coefficient", "var")
marginal_effects_selection$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection$coefficient)), digits = 3)
marginal_effects_selection$var <- as.factor(marginal_effects_selection$var)

marginal_effects_selection <- mutate(marginal_effects_selection,
                                   var = as.character(var),
                                   var = ifelse(var == "ika_decade_t1", "ika_t1_decade", var))

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
dummy_titles <- data.frame(var = c("pety",
                                   "ututku_aste",
                                   "hape"),
                           coefficient = rep(NA, 3),
                           se = rep(NA, 3))

vars <- c("ika_t1_decade",
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
          "spouse_working_t1TRUE")

marginal_effects_selection_personal <- data.frame(var = c(vars, "ln_disp_inc_t0"),
                                                  coefficient = c(-7.789592e-03, -4.417617e-03, 1.071647e-03,
                                                                  -1.870655e-03, -1.788252e-03, -1.477874e-03,
                                                                  -8.188476e-03, 2.492631e-03, 1.144903e-02,
                                                                  2.950937e-02, 5.981112e-03, -6.023947e-03,
                                                                  4.548861e-03, 1.374653e-02, 1.202741e-02,
                                                                  -6.793911e-03, -3.330617e-03),
                                                  se = c(4.293237e-04, 8.204167e-04, 7.489223e-04,
                                                         1.142525e-03, 1.598644e-03, 1.849166e-03,
                                                         2.448991e-03, 1.337287e-03, 1.817620e-03,
                                                         4.226891e-03, 8.617529e-04, 1.812357e-03,
                                                         2.214230e-03, 1.895907e-03, 1.279828e-03,
                                                         1.031760e-03, 1.003903e-03))
marginal_effects_selection_personal$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_personal$coefficient)), digits = 3)
marginal_effects_selection_personal$se <- 100*marginal_effects_selection_personal$se
marginal_effects_selection_personal$var <- as.factor(marginal_effects_selection_personal$var)
marginal_effects_selection_personal <- rbind(marginal_effects_selection_personal, dummy_titles)
marginal_effects_selection_personal$var <- gdata::drop.levels(marginal_effects_selection_personal$var)
marginal_effects_selection_personal$var <- factor(marginal_effects_selection_personal$var,
                                                  levels = c("comm_exp_t0TRUE",
                                                             "migr_exp_t0TRUE",
                                                             "hape_t0Other tenure status",
                                                             "hape_t0Right of occupancy dwelling",
                                                             "hape_t0Rents the dwelling",
                                                             "hape",
                                                             "spouse_working_t1TRUE",
                                                             "pety_t0Single parent",
                                                             "pety_t0Couple with children",
                                                             "pety_t0Couple without children",
                                                             "pety",
                                                             "ln_disp_inc_t0",
                                                             "ututku_aste_t1Doctoral or equivalent level",
                                                             "ututku_aste_t1Tertiary education",
                                                             "ututku_aste_t1Secondary education",
                                                             "ututku_aste",
                                                             "syntyp2Born abroad",
                                                             "opiskelija_t1Student",
                                                             "ika_t1_decade",
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

vars <- marginal_effects_selection_aluetyyppi$var

marginal_effects_selection_aluetyyppi <- data.frame(var = vars,
<<<<<<< HEAD
                                                    coefficient = c(0.00369556, 0.02146480,
                                                                    -0.01589441, 0.01577655,
                                                                    0.01951223, 0.01004649),
                                                    se = c(0.00317665, 0.00742544, 0.00406368,
                                                           0.00711684, 0.00734849, 0.00567277))

# labor demand controlled
marginal_effects_selection_aluetyyppi <- data.frame(var = vars,
                                                    coefficient = c(3.8479e-03, 2.7777e-02,
                                                                    -1.0086e-02, 1.7321e-02,
                                                                    1.8959e-02, 1.4563e-02),
                                                    se = c(2.7989e-03, 6.5170e-03, 1.8772e-03,
                                                           5.9606e-03, 6.8532e-03, 3.3957e-03))
=======
                                                    coefficient = c(0.00369556, 0.02146480, -0.01589441, 0.01577655,
                                                                    0.01951223, 0.01004649),
                                                    se = c(0.00317665, 0.00742544, 0.00406368, 0.00711684,
                                                           0.00734849, 0.00567277))
>>>>>>> 9c7598de7c4f4886a48d2bcd3b7c7fa948f83cc4

marginal_effects_selection_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_selection_aluetyyppi$coefficient)), digits = 3)
marginal_effects_selection_aluetyyppi$se <- 100*marginal_effects_selection_aluetyyppi$se
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

marginal_effects_outcome <- mutate(marginal_effects_outcome,
                                                     var = as.character(var),
                                                     var = ifelse(var == "ika_decade_t1", "ika_t1_decade", var))

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
dummy_titles <- data.frame(var = c("pety",
                                   "ututku_aste",
                                   "hape"),
                           coefficient = rep(NA, 3))


vars <- c("ika_decade_t1", "sukup_t1Female", "pety_t0Couple without children",
          "pety_t0Couple with children", "pety_t0Single parent", "hape_t0Rents the dwelling",
          "hape_t0Right of occupancy dwelling", "hape_t0Other tenure status", "syntyp2Born abroad",
          "opiskelija_t1Student", "ututku_aste_t1Secondary education", "ututku_aste_t1Tertiary education",
          "ututku_aste_t1Doctoral or equivalent level", "comm_exp_t0TRUE", "migr_exp_t0TRUE",
          "spouse_working_t1TRUE", "ln_disp_inc_t0")

marginal_effects_outcome_personal <- data.frame(var = vars,
                                                coefficient = c(-4.100149e-02, 2.470145e-02, -3.685216e-03,
                                                                -1.122665e-02, -2.267835e-03, 7.665948e-02,
                                                                9.477768e-02, 3.767058e-02, -2.127147e-03,
                                                                -5.241075e-02, -5.805070e-03, 3.902501e-02,
                                                                -5.488882e-02, -1.285532e-02, 5.623183e-02,
                                                                -4.867441e-02, -6.984539e-02))

marginal_effects_outcome_personal$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_personal$coefficient)), digits = 3)
vars <- c("sukup_t1Female",
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
"spouse_working_t1TRUE")

marginal_effects_selection_personal <- data.frame(var = c(vars, "ln_disp_inc_t0"),
                                                  coefficient = c(-1.877662e-02, -1.40253e-03, 3.504731e-03,
                                                                  2.490890e-03, -2.007966e-03, -3.602751e-03,
                                                                  -3.184202e-03, 1.12161e-02, 3.807244e-02,
                                                                  7.064802e-02, -5.487572e-03, -5.546047e-03,
                                                                  3.298334e-03, 3.831141e-02, 1.788559e-2,
                                                                  2.378705e-03, 2.049013e-02),
                                                  se = c(6.627779e-04, 1.211526e-03, 1.930851e-03,
                                                         1.394305e-03, 1.989112e-03, 3.314224e-03,
                                                         1.169685e-03, 1.031573e-03, 2.153423e-03,
                                                         9.277252e-03, 1.386705e-03, 2.618073e-03,
                                                         3.016382e-03, 2.903143e-03, 1.562585e-03,
                                                         1.589220e-03, 1.928470e-03))


>>>>>>> 9c7598de7c4f4886a48d2bcd3b7c7fa948f83cc4
marginal_effects_outcome_personal <- rbind(marginal_effects_outcome_personal, dummy_titles)
marginal_effects_outcome_personal$var <- gdata::drop.levels(marginal_effects_outcome_personal$var)
marginal_effects_outcome_personal$var <- factor(marginal_effects_outcome_personal$var,
                                                levels = c("comm_exp_t0TRUE",
                                                           "migr_exp_t0TRUE",
                                                           "hape_t0Other tenure status",
                                                           "hape_t0Right of occupancy dwelling",
                                                           "hape_t0Rents the dwelling",
                                                           "hape",
                                                           "spouse_working_t1TRUE",
                                                           "pety_t0Single parent",
                                                           "pety_t0Couple with children",
                                                           "pety_t0Couple without children",
                                                           "pety",
                                                           "ln_disp_inc_t0",
                                                           "ututku_aste_t1Doctoral or equivalent level",
                                                           "ututku_aste_t1Tertiary education",
                                                           "ututku_aste_t1Secondary education",
                                                           "ututku_aste",
                                                           "syntyp2Born abroad",
                                                           "opiskelija_t1Student",
                                                           "ika_decade_t1",
                                                           "sukup_t1Female"))

saveRDS(marginal_effects_outcome_personal,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_personal_employed.rds")

# Region characteristics

marginal_effects_outcome_regions <- marginal_effects_outcome %>%
  filter(var %in% c("etaisyys",
                    "asuntohintaero",
                    "tyomarkkinankokoero",
                    "tyottomyysasteero",
                    "saavutettavuusero",
                    "vuokra_osuus_destin_t1"))
marginal_effects_outcome_regions$var <- gdata::drop.levels(marginal_effects_outcome_alueet$var)

marginal_effects_outcome_regions <- data.frame(var = c("etaisyys_10", "asuntohintaero", "tyomarkkinankokoero",
                                                        "tyottomyysasteero", "saavutettavuusero", "vuokra_osuus_destin_t1",
                                                        "palkkaero"),
                                              coefficient = c(2.786075e-03, 4.070015e-03, 9.105661e-04,
                                                               -1.110924e-03, 3.113347e-06, 9.14273e-04, 2.451218e-03),
                                              se = c(9.525973e-04, 1.006485e-02, 4.687139e-04, 1.196962e-03,
                                                     3.263318e-06, 5.690279e-04, 3.089978e-02))
marginal_effects_outcome_regions$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_regions$coefficient)), digits = 3)
marginal_effects_outcome_regions$se <- 100*marginal_effects_outcome_regions$se
marginal_effects_outcome_regions$var <- factor(marginal_effects_outcome_regions$var,
                                               levels = c("palkkaero",
                                                          "saavutettavuusero",
                                                          "tyottomyysasteero",
                                                          "tyomarkkinankokoero",
                                                          "vuokra_osuus_destin_t1",
                                                          "asuntohintaero",
                                                          "etaisyys_10"))
saveRDS(marginal_effects_outcome_regions,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_regions_employed.rds")

# aluetyyppi

marginal_effects_outcome_aluetyyppi <- readRDS("data/nov12/liikkuvuusmalli/employed/marginal_effects_outcome_equation_at_employed.rds")
namesmarginal_effects_outcome_aluetyyppi <- c("coefficient", "var")

vars <- marginal_effects_outcome_aluetyyppi$var

marginal_effects_outcome_aluetyyppi <- data.frame(var = vars,

                                                  coefficient = c(-0.0342278, -0.0107271, -0.046906, -0.00320261,
                                                                  -0.014398, -0.005586, -0.020032210, -0.031785,
                                                                  0.047389, 0.03202660, -0.011231070, 0.048708210),
                                                  se = c(0.0196233146, 0.0298412521, 0.0209791823, 0.0198373699,
                                                         0.0238645421, 0.0178547932, 0.0207942811, 0.0221438553,
                                                         0.0214776968, 0.0190089276, 0.0181920082, 0.0219203491))

                                                  coefficient = c(-0.01657, -0.02403, 0.01780,-0.01173,
                                                                  -0.02577, -0.003077, -0.00667, -0.005264,
                                                                  0.007137, 0.00538, -0.002124, 0.00902))


marginal_effects_outcome_aluetyyppi$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_aluetyyppi$coefficient)), digits = 3)
marginal_effects_outcome_aluetyyppi$se <- 100*marginal_effects_outcome_aluetyyppi$se
marginal_effects_outcome_aluetyyppi$var <- as.factor(marginal_effects_outcome_aluetyyppi$var)
marginal_effects_outcome_aluetyyppi$var <- gdata::drop.levels(marginal_effects_outcome_aluetyyppi$var)

saveRDS(marginal_effects_outcome_aluetyyppi,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_aluetyyppi_employed.rds")


# Työpaikan ominaisuust

marginal_effects_outcome_job <- data.frame(var = c("tp1_hl_t1Medium size enterprise", "tp1_hl_t1_Large enterprise",
                                                   "tp1_lvl_t110,000,000 - 40,000,000", "tp1_lvl_t140,000,000 - ",
                                                   "oty1_t1State", "oty1_t1Municipality"),
                                           coefficient = c(-0.01163784, 0.008563990, 0.00011566,
                                                           0.029313, 0.0518936, 0.068102255),
                                           se = c(0.0174555, 0.02616, 0.018959, 0.02686,
                                                  0.034935, 0.0556221))
marginal_effects_outcome_job$coefficient <- 100*round(as.numeric(as.character(marginal_effects_outcome_job$coefficient)), digits = 3)
marginal_effects_outcome_job$se <- 100* marginal_effects_outcome_job$se

dummy_titles <- data.frame(var = c("lvl",
                                   "hl",
                                   "oty1"),
                           coefficient = rep(NA, 3),
                           se = rep(NA, 3))
marginal_effects_outcome_job <- rbind(marginal_effects_outcome_job, dummy_titles)
marginal_effects_outcome_job$var <- gdata::drop.levels(marginal_effects_outcome_job$var)
marginal_effects_outcome_job$var <- factor(marginal_effects_outcome_job$var,
                                                levels = c("oty1_t1Municipality",
                                                           "oty1_t1State",
                                                           "oty1",
                                                           "tp1_lvl_t140,000,000 - ",
                                                           "tp1_lvl_t110,000,000 - 40,000,000",
                                                           "lvl",
                                                           "tp1_hl_t1_Large enterprise",
                                                           "tp1_hl_t1Medium size enterprise",
                                                           "hl"))


saveRDS(marginal_effects_outcome_job,
        "data/liikkuvuusvalintamallitulokset/marginal_effects_outcome_job_employed.rds")



library(pxweb)
library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(stargazer)
library(plm)
source("R/kuntaluokka.R")

set_ptt()

data <- readRDS("data/avoimet_tyopaikat_tyonhakijat.rds")

data <- data %>% filter(vuosi < 2017) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  group_by(vuosi, Kunta) %>%
  summarize(tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE),
         vakanssiaste = mean(vakanssiaste, na.rm = TRUE)) %>%
  rename(Alue = Kunta,
         Vuosi = vuosi) %>%
  ungroup()

data$dtyottomyysaste = c(NA, diff(data$tyottomyysaste))
data$dvakanssiaste = c(NA, diff(data$vakanssiaste))

tulopendelointi_data <- readRDS("data/tulopendelointi_data.rds") %>%
                        group_by(Alue, Pendelöinti, Vuosi) %>%
                        summarize(values = sum(values, na.rm = TRUE)) %>%
                        spread(Pendelöinti, values) %>%
                        clean_names() %>%
                        filter(Vuosi > 2004) %>%
                        ungroup() %>%
                        rename(tulopendeloivat = Pendeloivat)
tulopendelointi_data$dAlueella_tyossakayvat_yhteensa <- c(NA, diff(tulopendelointi_data$Alueella_tyossakayvat_yhteensa))
tulopendelointi_data$dAsuinkunnassaan_tyossakayvat <- c(NA, diff(tulopendelointi_data$Asuinkunnassaan_tyossakayvat))
tulopendelointi_data$dtulopendeloivat <- c(NA, diff(tulopendelointi_data$tulopendeloivat))

lahtopendelointi_data <- readRDS("data/lahtopendelointi_data.rds") %>%
  group_by(Alue, Pendelöinti, Vuosi) %>%
  summarize(values = sum(values, na.rm = TRUE)) %>%
  spread(Pendelöinti, values) %>%
  clean_names() %>%
  filter(Vuosi > 2004) %>%
  ungroup()  %>%
  rename(lahtopendeloivat = Pendeloivat) %>%
  select(-Asuinkunnassaan_tyossakayvat)
lahtopendelointi_data$dTyolliset_yhteensa <- c(NA, diff(lahtopendelointi_data$Tyolliset_yhteensa))
lahtopendelointi_data$dlahtopendeloivat <- c(NA, diff(lahtopendelointi_data$lahtopendeloivat))

pendelointi_data <- left_join(tulopendelointi_data, lahtopendelointi_data, by = c("Alue", "Vuosi")) %>%
                    mutate(nettopendeloivat = lahtopendeloivat - tulopendeloivat)
pendelointi_data <- mutate(pendelointi_data,
                           commuting_balance = (tulopendeloivat - lahtopendeloivat) / Asuinkunnassaan_tyossakayvat)
pendelointi_data$dcommuting_balance <- c(NA, diff(pendelointi_data$commuting_balance))
pendelointi_data$dnettopendeloivat <- c(NA, diff(pendelointi_data$nettopendeloivat))
pendelointi_data <- mutate(pendelointi_data, dnet_commuter_inflow = -dnettopendeloivat / Tyolliset_yhteensa)
pendelointi_data <- filter(pendelointi_data, Vuosi > 2005)

data <- left_join(data, pendelointi_data, by = c("Alue", "Vuosi"))

data <- mutate(data, vuosi = as.factor(Vuosi))
data <- mutate(data, seutukunta = kuntaluokka(Alue, "Seutukunta"))
data <- mutate(data, maakunta = kuntaluokka(Alue, "Maakunta"))

malli0 <- lm(tyottomyysaste ~ vakanssiaste*dcommuting_balance + maakunta + vuosi, data = data)
malli1 <- lm(tyottomyysaste ~ vakanssiaste*dcommuting_balance + maakunta + vuosi, data = data)
malli2 <- lm(tyottomyysaste ~ vakanssiaste*dcommuting_balance + seutukunta + vuosi, data = data)
malli3 <- lm(tyottomyysaste ~ vakanssiaste*commuting_balance + Alue + vuosi, data = data)

malli0 <- lm(tyottomyysaste ~ vakanssiaste*dnet_commuter_inflow + vuosi, data = data)
malli1 <- lm(tyottomyysaste ~ vakanssiaste*dnet_commuter_inflow + maakunta + vuosi, data = data)
malli2 <- lm(tyottomyysaste ~ vakanssiaste*dnet_commuter_inflow + seutukunta + vuosi, data = data)
malli3 <- lm(tyottomyysaste ~ vakanssiaste*dnet_commuter_inflow + Alue + vuosi, data = data)


labels <- c("Vacancy rate", "$\\Delta$ (Commuting balance)", "Vacancy rate x $\\Delta$ (Commuting balance)" )

stargazer( malli1, malli2, malli3, type = "text",
          omit = c("vuosi", "Constant", "Alue", "maakunta", "seutukunta"),
          add.lines = list(c("Constant term", "Yes", "Yes", "Yes"),
                           c("Year fixed effects", "Yes", "Yes", "Yes"),
                           c("Regional fixed effects", "Yes", "No", "No"),
                           c("Sub-regional fixed effects", "No", "Yes", "No"),
                           c("Municipality fixed effects","No", "No", "Yes")),
          covariate.labels = labels,
          dep.var.caption = "",
          dep.var.labels = "Unemployment rate",
          out = "C:/Users/juhoa/Google Drive/Labor market search, report/output_table2.tex")


kunta_data <- readRDS("data/kuntien_avainluvut.rds")

data2 <- left_join(data, kunta_data, by = c("Alue", "vuosi"))

data2 <- mutate(data2, nettomuutto = Kuntien_valinen_muuttovoitto_tappio_henkiloa / Asuinkunnassaan_tyossakayvat)

library(plm)
library(lmtest)

data2 <- mutate(data2, Vuosi = as.factor(Vuosi))

p.df <- pdata.frame(data2, index = c("Alue", "Vuosi"))
panel_model <- plm(tyottomyysaste ~ vakanssiaste*commuting_balance + Alue + vuosi,
                   data = p.df, model = "within", effect = "twoways")

robust_se <- sqrt(diag(vcovHC(panel_model, type = "HC0", cluster = "group")))
coeftest(panel_model, vcov = vcovHC(panel_model, type = "HC0", cluster = "group"))
waldtest(panel_model, vcov = vcovHC(panel_model, type = "HC1", cluster = "group"), test = "F")

labels_plm <- c("Vacancy rate", "Commuting balance", "Vacancy rate x Commuting balance")

stargazer(panel_model, type = "text",
          se = list(robust_se),
          add.lines = list(c("Year fixed effects", "Yes"),
                           c("Municipality fixed effects", "Yes")),
          covariate.labels = labels_plm,
          dep.var.caption = "",
          dep.var.labels = "Unemployment rate",
          out = "C:/Users/juhoa/Google Drive/Labor market search, report/plm_output_table.tex")

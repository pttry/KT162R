

library(pxweb)
library(tidyverse)
library(statfitools)
library(ggplot2)
library(ggptt)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)

set_ptt()

data <- readRDS("data/avoimet_tyopaikat_tyonhakijat.rds")


# Koko maan Beveridge-käyrä, kuukausittain, kausitasoitus ja trendaus

data_kokomaa <- data %>% group_by(Kuukausi) %>%
                         summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
                                   Tyovoima = sum(Tyovoima, na.rm = TRUE),
                                   Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
                         mutate(tyottomyysaste = Tyottomat / Tyovoima,
                                vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
                         mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
                                vuosi = substring(Kuukausi, 1,4),
                                time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
                         mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
                                tyottomyysaste_trend = trend_series(tyottomyysaste, time),
                                vakanssiaste_sa = sa_series(vakanssiaste, time),
                                vakanssiaste_trend = trend_series(vakanssiaste, time),
                                tyottomat_sa = sa_series(Tyottomat, time),
                                tyottomat_trend = trend_series(Tyottomat, time),
                                avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
                                avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time)) %>%
                         mutate(vuosi_label = ifelse(grepl("M01", Kuukausi), vuosi, ""))

data_kokomaa %>% ggplot(aes(x = tyottomat_sa, y = avoimet_tyopaikat_sa, label = vuosi_label)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
                        geom_text(color = "black") +
                        labs(y = "Avoimet työpaikat",
                             x = "Tyottomat")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kokomaa_kuukausittain_absoluuttiset_sa.png")

data_kokomaa %>% ggplot(aes(x = tyottomat_trend, y = avoimet_tyopaikat_trend, label = vuosi_label)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[1]) +
  geom_text(color = "black") +
  labs(y = "Avoimet työpaikat",
       x = "Tyottomat")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kokomaa_kuukausittain_absoluuttiset_trend.png")

data_kokomaa %>% ggplot(aes(x = tyottomyysaste_sa, y = vakanssiaste_sa, label = time)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[1]) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kokomaa_kuukausittain_asteet_sa.png")


data_kokomaa %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend, label = vuosi_label)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[1]) +
  geom_text(color = "black") +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys") +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kokomaa_kuukausittain_asteet_trend.png")


 ggsave("C:/Users/juhoa/Google Drive/Projects/New job, migrate or commute/Presentation/Graphs/beveridge.pdf")


# Beveridge-käyrät kuntaryhmittäin, kuukausittain, kausitasoitus ja trendaus

data_kuntaryhmat <- data %>% group_by(Kuukausi, Kuntaryhma) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(Kuntaryhma) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time))

data_kuntaryhmat %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  facet_wrap(~Kuntaryhma) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kuntaryhmittain_asteet_trend.png")

data_kuntaryhmat %>% ggplot(aes(x = tyottomyysaste_sa, y = vakanssiaste_sa)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  facet_wrap(~Kuntaryhma) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kuntaryhmittain_asteet_sa.png")

data_kuntaryhmat %>% ggplot(aes(x = tyottomat_trend, y = avoimet_tyopaikat_trend)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  facet_wrap(~Kuntaryhma) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kuntaryhmittain_absoluuttiset_trend.png")

data_kuntaryhmat %>% ggplot(aes(x = tyottomat_sa, y = avoimet_tyopaikat_sa)) +
  geom_point(size = 3) +
  geom_path(size = 1) +
  facet_wrap(~Kuntaryhma) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/kuntaryhmittain_absoluuttiset_sa.png")

################## Beveridge-käyrät aluetyypeittäin, kuukausittain, kausitasoitus ja trendaus #################

data$aluetyyppi <- fct_relevel(data$aluetyyppi, c("pk", "yo-kaup", "kaup", "tk_keskus", "kaupms", "ydinms", "ms"))
aluetyyppi_labels = c("Pääkaupunkiseutu", "Yliopistokaupungit", "Kaupungit", "Muut työssäkäyntikeskukset",
                      "Kaupunkien läheinen maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu")

data_atyypit <- data %>% group_by(Kuukausi, aluetyyppi) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(aluetyyppi) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time)) %>%
  mutate(vuosi_label = ifelse(grepl("M01", Kuukausi) & vuosi %in% c(2006, 2012, 2018), vuosi, ""))

data_atyypit %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend, label = vuosi_label)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[1]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_text(color = "black") +
                        facet_wrap(~aluetyyppi, labeller = labeller(aluetyyppi = labels)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
                        labs(y = "Vakanssiaste",
                             x = "Työttömyys") +
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13)) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

labels = c(pk = "Pääkaupunkiseutu",
           "yo-kaup" = "Yliopistokaupungit",
           kaup = "Kaupungit",
           tk_keskus = "Muut työssäkäyntikeskukset",
           kaupms = "Kaupunkien läheinen maaseutu",
           ydinms = "Ydinmaaseutu",
           ms = "Harvaan asuttu maaseutu")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain_asteet_trend.png",
       width = 10, height = 10)

data_atyypit %>% ggplot(aes(x = tyottomyysaste_sa, y = vakanssiaste_sa)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  facet_wrap(~aluetyyppi, labeller = labeller(aluetyyppi = labels)) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain_asteet_sa.png")

data_atyypit %>% ggplot(aes(x = tyottomat_sa, y = avoimet_tyopaikat_sa)) +
  geom_point(size = 1) +
                        geom_path(size = 1) +
                     facet_wrap(~aluetyyppi) +
                     geom_hline(yintercept = 0) +
                     geom_vline(xintercept = 0) +
  labs(y = "Avoimet työpaikat",
       x = "Työttömät",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT")

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain_absoluuttiset_sa.png")

data_atyypit %>% ggplot(aes(x = tyottomat_trend, y = avoimet_tyopaikat_trend, label = vuosi_label)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[1]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_text(color = "black") +
  facet_wrap(~aluetyyppi, labeller = labeller(aluetyyppi = labels)) +
  labs(y = "Avoimet työpaikat",
       x = "Työttömät") +
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))


ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain_absoluuttiset_trend.png",
       width = 10, height = 10)

# Demeaned

data_kokomaa <- data %>% group_by(Kuukausi) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  select(Kuukausi, tyottomyysaste, vakanssiaste) %>%
  rename(tyottomyysaste_kokomaa = tyottomyysaste, vakanssiaste_kokomaa = vakanssiaste)

data_kokomaa2 <- data_kokomaa[rep(1:162, each = 7),]

data_atyypit_demeaned <- data %>% group_by(Kuukausi, aluetyyppi) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(vakanssiaste_kokomaa = data_kokomaa2$vakanssiaste_kokomaa,
         tyottomyysaste_kokomaa = data_kokomaa2$tyottomyysaste_kokomaa,
         tyottomyysaste = (Tyottomat / Tyovoima) - tyottomyysaste_kokomaa,
         vakanssiaste = (Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) - vakanssiaste_kokomaa) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(aluetyyppi) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time)) %>%
  mutate(vuosi_label = ifelse(grepl("M01", Kuukausi) & vuosi %in% c(2006, 2018), vuosi, ""))

data_atyypit_demeaned %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend, label = vuosi_label)) +
  geom_point(size = 0.5, color = ggptt_palettes$vnk[1]) +
  geom_path(size = 0.5, color = ggptt_palettes$vnk[2]) +
geom_text(color = "black", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  facet_wrap(~aluetyyppi, labeller = labeller(aluetyyppi = labels)) +
  labs(y = "Vakanssiaste, erotus koko maan vakanssiasteesta",
       x = "Työttömyys, erotus koko maan työttömyysasteesta") +
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white")) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypit_demeaned.png",
       width = 12,
       height = 8)

#####################################################################################################################

# Pehkonen et al. 2018 replica

suuret_seutukunnat <- c("Helsinki", "Tampere", "Turku", "Oulu", "Jyväskylä", "Kuopio", "Seinäjoki")

data <- data %>% mutate(stkluokka = ifelse(Seutukunta %in% suuret_seutukunnat,
                                                                             "suuret_seutukunnat",
                                                                             "muut_seutukunnat"))

data_stkluokat <- data %>% group_by(Kuukausi, stkluokka) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(stkluokka) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time))

data_stkluokat <- data_stkluokat %>%
                  mutate(vuosi_label = ifelse(grepl("M01", Kuukausi), vuosi, ""))

p1 <- data_stkluokat %>%  ggplot(aes(x = tyottomat_trend, y = avoimet_tyopaikat_trend,
                              color = stkluokka, label = vuosi_label)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text(color = "black") +
  labs(y = "Avoimet tyopaikat",
       x = "Tyottomat",
       color = NULL) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("Muut seutukunnat",
                                "Suuret seutukunnat"),
                     values = brewer.pal(3, "Blues")[2:3])

p2 <- data_stkluokat %>%  ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend,
                                     color = stkluokka, label = vuosi_label)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text(color = "black") +
  labs(y = "Vakanssiaste",
       x = "Tyottomyys",
       color = NULL) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("Muut seutukunnat",
                                "Suuret seutukunnat"),
                     values = brewer.pal(3, "Blues")[2:3]) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/suuret_pienet_seutukunnat.png", plot = p,
       width = 12,
       height = 6)


# Vuosikeskiarvot

data_stkluokat2 <- data %>% group_by(Kuukausi, stkluokka) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(vuosi = substring(Kuukausi, 1,4)) %>%
  group_by(vuosi, stkluokka) %>%
  summarize(tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE),
            Tyovoima = mean(Tyovoima, na.rm = TRUE),
            Tyottomat = mean(Tyottomat, na.rm = TRUE),
            Avoimet_tyopaikat = mean(Avoimet_tyopaikat, na.rm = TRUE),
            vakanssiaste = mean(vakanssiaste, na.rm = TRUE))%>%
  filter(vuosi < 2019)


data_stkluokat2 %>% ggplot(aes(x = tyottomyysaste, y = vakanssiaste,
                               col = stkluokka, label = vuosi)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text() +
  labs(y = "Vakanssiaste",
       x = "Tyottomyysaste",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT") +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

############################################################################################################


################### Neljä aluetyyppiä ######################################

atyyppi4 <- c("kaupms" = "Kaupunkien läheinen maaseutu",
             "tk_keskus" = "Kaupunki",
             "ydinms" = "Ydinmaaseutu",
             "ms" = "Harvaan asuttu maaseutu",
             "pk" = "Kaupunki",
             "kaup" = "Kaupunki",
             "yo-kaup" = "Kaupunki")

data2 <- mutate(data, aluetyyppi4 = plyr::revalue(as.factor(aluetyyppi), atyyppi4 ))

data_atyypit <- data2 %>% group_by(Kuukausi, aluetyyppi4) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(vuosi = substring(Kuukausi, 1,4)) %>%
  group_by(vuosi, aluetyyppi4) %>%
  summarize(Tyottomat = mean(Tyottomat, na.rm = TRUE),
            Tyovoima = mean(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = mean(Avoimet_tyopaikat, na.rm = TRUE))%>%
  filter(vuosi < 2019)


data_atyypit %>% ggplot(aes(x = Tyottomat, y = Avoimet_tyopaikat, label = vuosi)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
 # geom_text() +
  facet_wrap(~aluetyyppi4) +
  labs(y = "Avoimet työpaikat",
       x = "Tyottomat",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT") +
  scale_y_continuous(labels = deci_comma) +
  scale_x_continuous(labels = deci_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain4_absoluuttiset_kokomaa_kuukausika.png")

data2 <- mutate(data, aluetyyppi4 = plyr::revalue(as.factor(aluetyyppi), atyyppi4 ))

data_atyypit <- data2 %>% group_by(Kuukausi, aluetyyppi4) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(vuosi = substring(Kuukausi, 1,4)) %>%
  group_by(vuosi, aluetyyppi4) %>%
  summarize(tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE),
            Tyovoima = mean(Tyovoima, na.rm = TRUE),
            vakanssiaste = mean(vakanssiaste, na.rm = TRUE))%>%
  filter(vuosi < 2019)


data_atyypit %>% ggplot(aes(x = tyottomyysaste, y = vakanssiaste, label = vuosi)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
   geom_text() +
  facet_wrap(~aluetyyppi4) +
  labs(y = "Vakanssiaste",
       x = "Tyottomyysaste",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT") +
  scale_y_continuous(labels = deci_comma) +
  scale_x_continuous(labels = deci_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/aluetyypeittain4_asteet_kokomaa_kuukausika.png")


###############################################################################################

  # by commuting balance

data("dat_pendelointi")

kuntakoodit <- readRDS(here::here("data/kuntakoodit2017.rds")) %>%
  rename(kunta = Knro, alue = Kunta)
kuntakoodit$alue <- as.character(kuntakoodit$alue)

pendelointi_kunnittain <- dat_pendelointi %>%
  filter(vuosi == 2016) %>%
  group_by(alue) %>%
  summarize(tulopendelointi = sum(tulopendelointi),
            lahtopendelointi = sum(lahtopendelointi),
            asuinkunnassaan_tyossakayvat = sum(asuinkunnassaan_tyossakayvat)) %>%
  mutate(kunnassa_tyossakayvat = tulopendelointi + asuinkunnassaan_tyossakayvat)

pendelointi_kunnittain$alue <- as.character(pendelointi_kunnittain$alue)

pendelointi_kunnittain <- left_join(pendelointi_kunnittain, kuntakoodit, by = "alue")


pendelointi_kunnittain <- pendelointi_kunnittain %>%
  mutate(commuting_balance = (tulopendelointi - lahtopendelointi)/asuinkunnassaan_tyossakayvat,
         openness_index = (tulopendelointi + lahtopendelointi)/asuinkunnassaan_tyossakayvat,
         ln_commuting_balance = log(commuting_balance),
         ln_openness_index = log(openness_index))

breaks <- seq(from = min(pendelointi_kunnittain$commuting_balance)-0.00001,
    to = max(pendelointi_kunnittain$commuting_balance)+0.000001,
    length.out = 4)

pendelointi_kunnittain$commuting_balance_discrete <- cut(pendelointi_kunnittain$commuting_balance, breaks)

balance <- pendelointi_kunnittain %>%
  select(commuting_balance, commuting_balance_discrete, kunta) %>%
  rename(Knro = kunta)

data <- left_join(data, balance, by = "Knro")

levels(data$commuting_balance_discrete) <- c("Matala", "Keskiverto", "Korkea")

data_balance <- data %>% group_by(Kuukausi, commuting_balance_discrete) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(commuting_balance_discrete) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time))

data_balance <- data_balance %>%
  mutate(vuosi_label = ifelse(grepl("M01", Kuukausi), vuosi, ""),
         vuosi_label2 = ifelse(grepl("2006M01", Kuukausi) | grepl("2019M01", Kuukausi), vuosi, ""))

p1 <- data_balance %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend, label = vuosi_label2)) +
  geom_point(size = 1, color = ggptt_palettes$vnk[2]) +
  geom_path(size = 1, color = ggptt_palettes$vnk[1]) +
  geom_text(color = "black") +
  facet_wrap(~commuting_balance_discrete) +
  labs(y = "Vakanssiaste",
       x = "Työttömyysaste",
       title = "Pendelöintitasapaino") +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 15)) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/commuting_balance_beveridge.png", width = 8, height = 3.5)

levels(data$Kuntaryhma) <- c("Rural municipalities", "Semi-urban municipalities", "Urban municipalities")

data_kuntaryhmat <- data %>% group_by(Kuukausi, Kuntaryhma) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(Kuntaryhma) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time))

p2 <- data_kuntaryhmat %>% ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_sa)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  facet_wrap(~Kuntaryhma) +
  labs(y = "Vacancy rate",
       x = "Unemployment rate",
       title = "Type of municipality")

p <- grid.arrange(p2, p1, ncol = 1)

ggsave("C:/Users/juhoa/Google Drive/Labor market search, report/beverigde_curves.png", plot = p)

stargazer(aluetyypit, type = "text", summary = FALSE,
          out = "aluetyypit.html")

############################################################
# Pehkonen et al. 2018 replica w. rakennemuutosseutukunnast

suuret_seutukunnat <- c("Helsinki", "Tampere", "Turku", "Oulu", "Jyväskylä", "Kuopio", "Seinäjoki")
rakennemuutos_seutukunnat <- c("Kouvola", "Kokkola", "Salo", "Forssa", "Jakobstadsregionen", "Imatra", "Kotka-Hamina", "Etelä-Pirkanmaa")

data <- data %>% mutate(stkluokka = ifelse(Seutukunta %in% suuret_seutukunnat,
                                           "suuret_seutukunnat",
                                           ifelse(Seutukunta %in% rakennemuutos_seutukunnat, "rakennemuutosseutukunnat", "muut_seutukunnat")))

data_stkluokat <- data %>% group_by(Kuukausi, stkluokka) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  group_by(stkluokka) %>%
  mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
         tyottomyysaste_trend = trend_series(tyottomyysaste, time),
         vakanssiaste_sa = sa_series(vakanssiaste, time),
         vakanssiaste_trend = trend_series(vakanssiaste, time),
         tyottomat_sa = sa_series(Tyottomat, time),
         tyottomat_trend = trend_series(Tyottomat, time),
         avoimet_tyopaikat_sa = sa_series(Avoimet_tyopaikat, time),
         avoimet_tyopaikat_trend = trend_series(Avoimet_tyopaikat, time))

data_stkluokat <- data_stkluokat %>%
  mutate(vuosi_label = ifelse(grepl("M01", Kuukausi), vuosi, ""),
         vuosi_label2 = ifelse(grepl("2006M01", Kuukausi) | grepl("2019M01", Kuukausi), vuosi, ""))

p1 <- data_stkluokat %>%  ggplot(aes(x = tyottomat_trend, y = avoimet_tyopaikat_trend,
                                     color = stkluokka, label = vuosi_label2)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text(color = "black") +
  labs(y = "Avoimet tyopaikat",
       x = "Tyottomat",
       color = NULL) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("Muut seutukunnat",
                                "Suuret seutukunnat",
                                "Rakennemuutoksen seutukunnat"),
                     values = brewer.pal(4, "Blues")[2:4])

p2 <- data_stkluokat %>%  ggplot(aes(x = tyottomyysaste_trend, y = vakanssiaste_trend,
                                     color = stkluokka, label = vuosi_label)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text(color = "black") +
  labs(y = "Vakanssiaste",
       x = "Työttömyys",
       color = NULL) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12)) +
  scale_color_manual(labels = c("Muut seutukunnat",
                                "Rakennemuutoksen seutukunnat",
                                "Suuret seutukunnat" ),
                     values = ggptt_palettes$ptt[1:3])
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/suuret_pienet_rakennemuutos_seutukunnat.png")

p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/Beveridge/suuret_pienet_rakennemuutos_seutukunnat.png", plot = p,
       width = 12,
       height = 6)


# Vuosikeskiarvot

data_stkluokat2 <- data %>% group_by(Kuukausi, stkluokka) %>%
  summarize(Tyottomat = sum(Tyottomat, na.rm = TRUE),
            Tyovoima = sum(Tyovoima, na.rm = TRUE),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE)) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima)) %>%
  mutate(vuosi = substring(Kuukausi, 1,4)) %>%
  group_by(vuosi, stkluokka) %>%
  summarize(tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE),
            Tyovoima = mean(Tyovoima, na.rm = TRUE),
            Tyottomat = mean(Tyottomat, na.rm = TRUE),
            Avoimet_tyopaikat = mean(Avoimet_tyopaikat, na.rm = TRUE),
            vakanssiaste = mean(vakanssiaste, na.rm = TRUE))%>%
  filter(vuosi < 2019)


data_stkluokat2 %>% ggplot(aes(x = tyottomyysaste, y = vakanssiaste,
                               col = stkluokka, label = vuosi)) +
  geom_point(size = 1) +
  geom_path(size = 1) +
  geom_text() +
  labs(y = "Vakanssiaste",
       x = "Tyottomyysaste",
       caption = "Lähde: Tilastokeskus, Työnvälitystilasto (TEM), PTT") +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)

############################################################################################################

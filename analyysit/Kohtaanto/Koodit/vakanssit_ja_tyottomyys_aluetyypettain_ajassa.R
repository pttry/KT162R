library(tidyverse)
library(statfitools)
library(ggptt)
library(ggplot2)
library(gridExtra)
library(forcats)
library(ggpubr)

data <- readRDS("data/avoimet_tyopaikat_tyonhakijat.rds")

# Muokkaa data

df <- data %>% mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
                                 vuosi = substring(Kuukausi, 1,4),
                                 time = as.Date(paste(vuosi, kuukausi, sep = "-")),
                                 aluetyyppi = as.factor(aluetyyppi)) %>%
         group_by(aluetyyppi, time) %>%
         summarize(avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
                   tyottomat = sum(Tyottomat, na.rm = TRUE),
                   tyovoima = sum(Tyovoima, na.rm = TRUE)) %>%
         mutate(tyottomyysaste = tyottomat / tyovoima,
                vakanssiaste = avoimet_tyopaikat / (avoimet_tyopaikat + tyovoima - tyottomat)) %>%
         mutate(tyottomyysaste_sa = sa_series(tyottomyysaste, time),
                tyottomyysaste_trend = trend_series(tyottomyysaste, time),
                vakanssiaste_sa = sa_series(vakanssiaste, time),
                vakanssiaste_trend = trend_series(vakanssiaste, time),
                tyottomat_sa = sa_series(tyottomat, time),
                tyottomat_trend = trend_series(tyottomat, time),
                avoimet_tyopaikat_sa = sa_series(avoimet_tyopaikat, time),
                avoimet_tyopaikat_trend = trend_series(avoimet_tyopaikat, time))

df$aluetyyppi <- fct_relevel(df$aluetyyppi, c("pk", "yo-kaup", "kaup", "tk_keskus", "kaupms", "ydinms", "ms"))
aluetyyppi_labels = c("Pääkaupunkiseutu", "Yliopistokaupungit", "Kaupungit", "Muut työssäkäyntikeskukset",
                      "Kaupunkien läheinen maaseutu", "Ydinmaaseutu", "Harvaan asuttu maaseutu")

set_ptt()

# Työttömyys- ja vakanssiasteet

p1 <- df %>% ggplot(aes(x = time, y = tyottomyysaste_trend, col = aluetyyppi)) +
  geom_line() +
  labs(y = "Työttömyysaste, trenditasoitettu",
       x = NULL,
       color = NULL) +
  scale_color_manual(labels = aluetyyppi_labels,
                     values = c(brewer.pal(4, "Blues"), brewer.pal(3, "Greens"))) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12)) +
  guides(col = guide_legend(nrow = 3))

p2<- df %>% ggplot(aes(x = time, y = vakanssiaste_trend, col = aluetyyppi)) +
  geom_line() +
  labs(y = "Vakanssiaste, trenditasoitettu",
       x = NULL,
       color = NULL) +
  scale_color_manual(labels = aluetyyppi_labels,
                     values = c(brewer.pal(4, "Blues"), brewer.pal(3, "Greens"))) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12)) +
  guides(col = guide_legend(nrow = 3))

p <- ggarrange(p1, p2, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/tyottomat_avoimet_tyopaikat_ajassa_aluetyypeittain.png",
       plot = p,
       width = 8,
       height = 7)


# Työttömät ja avoimet työpaikat

p3 <- df %>% ggplot(aes(x = time, y = tyottomat_trend, col = aluetyyppi)) +
  geom_line() +
  labs(y = "Työttömät, trenditasoitettu",
       x = NULL,
       color = NULL) +
  scale_color_manual(labels = aluetyyppi_labels,
                     values = c(brewer.pal(4, "Blues"), brewer.pal(3, "Greens"))) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12)) +
  guides(col = guide_legend(nrow = 3))

p4<- df %>% ggplot(aes(x = time, y = avoimet_tyopaikat_trend, col = aluetyyppi)) +
  geom_line() +
  labs(y = "Avoimet työpaikat, trenditasoitettu",
       x = NULL,
       color = NULL) +
  scale_color_manual(labels = aluetyyppi_labels,
                     values = c(brewer.pal(4, "Blues"), brewer.pal(3, "Greens"))) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12)) +
  guides(col = guide_legend(nrow = 3))

p <- ggarrange(p3, p4, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/tyottomat_avoimet_tyopaikat_ajassa_aluetyypeittain.png",
       plot = p,
       width = 8,
       height = 7)


p <- ggarrange(p1, p2, p3, p4, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/tyomarkkinat_ajassa_aluetyypeittain.png",
       plot = p,
       width = 8,
       height = 14)

nimittajat <- df %>%  filter(grepl("2018", time)) %>%
  group_by( time) %>%
  summarize(avoimet_tyopaikat = sum(avoimet_tyopaikat, na.rm = TRUE),
            tyottomat = sum(tyottomat, na.rm = TRUE),
            tyovoima = sum(tyovoima)) %>%
  summarize(avoimet_tyopaikat = mean(avoimet_tyopaikat),
            tyottomat = mean(tyottomat),
            tyovoima = mean(tyovoima))

table <- df %>% filter(grepl("2018", time)) %>%
       group_by(aluetyyppi, time) %>%
       summarize(avoimet_tyopaikat = sum(avoimet_tyopaikat, na.rm = TRUE),
                 tyottomat = sum(tyottomat, na.rm = TRUE),
                 tyovoima = sum(tyovoima)) %>%
       group_by(aluetyyppi) %>%
       summarize(avoimet_tyopaikat_vka = round(mean(avoimet_tyopaikat), digits = 1),
                 tyottomat_vka = round(mean(tyottomat), digits = 1),
                 tyovoima_vka = round(mean(tyovoima), digits = 1)) %>%
       mutate(avoimet_tyopaikat_vka_osuus = round(avoimet_tyopaikat_vka / nimittajat$avoimet_tyopaikat, digits = 2),
              tyottomat_vka_osuus = round(tyottomat_vka / nimittajat$tyottomat, digits = 2),
              tyovoima_vka_osuus = round(tyovoima_vka / nimittajat$tyovoima, digits = 2),
              aluetyyppi = aluetyyppi_labels)

table %>% select(aluetyyppi, avoimet_tyopaikat_vka, tyottomat_vka, tyovoima_vka) %>%
  stargazer(summary = FALSE, type = "text",
            out = "analyysit/Kohtaanto/Taulukot/tyottomat_vakanssit_aluetyypeittain.html")

table %>% select(aluetyyppi, avoimet_tyopaikat_vka_osuus, tyottomat_vka_osuus, tyovoima_vka_osuus) %>%
       stargazer(summary = FALSE, type = "text",
                 out = "analyysit/Kohtaanto/Taulukot/tyottomat_vakanssit_aluetyypeittain_osuus.html")

df %>% filter(grepl("2018", time)) %>%
  group_by(aluetyyppi, time) %>%
  summarize(avoimet_tyopaikat = sum(avoimet_tyopaikat, na.rm = TRUE),
            tyottomat = sum(tyottomat, na.rm = TRUE),
            tyovoima = sum(tyovoima)) %>%
  group_by(aluetyyppi) %>%
  summarize(avoimet_tyopaikat_vka = mean(avoimet_tyopaikat),
            tyottomat_vka = mean(tyottomat),
            tyovoima_vka = mean(tyovoima)) %>%
  mutate(vakanssiaste_vka = round(avoimet_tyopaikat_vka / (avoimet_tyopaikat_vka + tyovoima_vka - tyottomat_vka), digits = 2),
         tyottomyysaste_vka = round(tyottomat_vka / tyovoima_vka, digits = 2)) %>%
  select(aluetyyppi, vakanssiaste_vka, tyottomyysaste_vka) %>%
  mutate(aluetyyppi = aluetyyppi_labels) %>%
  stargazer(summary = FALSE, type = "text",
            out = "analyysit/Kohtaanto/Taulukot/tyottomyysasteeet_vakanssiasteet_aluetyypeittain.html")

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
                vakanssiaste = avoimet_tyopaikat / (avoimet_tyopaikat + tyovoima + tyottomat)) %>%
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

p1 <- df %>% ggplot(aes(x = time, y = tyottomat_trend, col = aluetyyppi)) +
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

p2<- df %>% ggplot(aes(x = time, y = avoimet_tyopaikat_trend, col = aluetyyppi)) +
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

p <- ggarrange(p1, p2, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("analyysit/Kohtaanto/Kuviot/tyottomat_avoimet_tyopaikat_ajassa_aluetyypeittain.png",
       plot = p,
       width = 8,
       height = 7)


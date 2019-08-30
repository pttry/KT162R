# Työmarkkinoiden tiukkuus

data %>% group_by(aluetyyppi, Kuukausi) %>%
  summarize(avoimet_tyopaikat = sum(Avoimet_tyopaikat, na.rm = TRUE),
            tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  mutate(tiukkuus = avoimet_tyopaikat / tyottomat) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-"))) %>%
  ggplot(aes(y = tiukkuus, x = time, col = aluetyyppi)) + geom_smooth(span = 0.2, se = FALSE)

ggsave("analyysit/Kohtaanto/Kuviot/tiukkuus_aluetyypeittain.png")

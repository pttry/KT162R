# Työmarkkinoiden alueelliset erot

# Onko työmarkkinoiden alueelliset erot kaventuneet?

# Currently using coefficient of variation

data_kokomaa <- data %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima - Tyottomat)) %>%
  mutate(kuukausi = paste(substring(Kuukausi, 6,7), "01", sep = "-"),
         vuosi = substring(Kuukausi, 1,4),
         time = as.Date(paste(vuosi, kuukausi, sep = "-")))

vakanssiastevar_plot <- data_kokomaa %>% group_by(time) %>%
  summarize(tyottomyys_var = sqrt(var(tyottomyysaste, na.rm = TRUE)) / mean(tyottomyysaste, na.rm = TRUE),
            vakanssiaste_var = sqrt(var(vakanssiaste, na.rm = TRUE)) / mean(tyottomyysaste, na.rm = TRUE)) %>%
  gather(tiedot, value, -time) %>%
  filter(tiedot == "vakanssiaste_var") %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  geom_smooth(span = 0.3) +
  labs(x = NULL,
       y = "Vakanssiasteiden variaatiokerroin") +
  scale_y_continuous(labels = percent_comma)

tyottomyysastevar_plot <- data_kokomaa %>% group_by(time) %>%
  summarize(tyottomyys_var = sqrt(var(tyottomyysaste, na.rm = TRUE)) / mean(tyottomyysaste, na.rm = TRUE),
            vakanssiaste_var = sqrt(var(vakanssiaste, na.rm = TRUE)) / mean(tyottomyysaste, na.rm = TRUE)) %>%
  gather(tiedot, value, -time) %>%
  filter(tiedot == "tyottomyys_var") %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  geom_smooth(span = 0.3) +
  labs(x = NULL,
       y = "Tyottomyysasteiden variaatiokerroin") +
  scale_y_continuous(labels = percent_comma)

alueelliset_erot_ajassa_plot <- grid.arrange(vakanssiastevar_plot, tyottomyysastevar_plot)

ggsave("analyysit/Kohtaanto/Kuviot/alueelliset_erot_ajassa.png", plot = alueelliset_erot_ajassa_plot,
       width = 8,
       height = 5)


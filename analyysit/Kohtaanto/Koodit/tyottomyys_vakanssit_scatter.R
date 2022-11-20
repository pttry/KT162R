
tyot_vakanssi_ku_dat <- readRDS(here("data/avoimet_tyopaikat_tyonhakijat.rds"))

tyot_vakanssi_ku_dat %>%
  # filter(vuosi == 2018) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima - Tyottomat)) %>%
  group_by(Kunta, vuosi) %>%
  summarise_at(vars(tyottomyysaste, vakanssiaste, Tyottomat), mean, na.rm = TRUE) %>%
  ungroup() %>%
  filter(Tyottomat > 1000) %>%
  ggplot(aes(tyottomyysaste, vakanssiaste, size = Tyottomat)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm") +
  facet_wrap(~ vuosi)

tyot_vakanssi_ku_dat %>%
  filter(vuosi == 2018,
         aluetyyppi %in% c("yo-kaup", "tk_keskus")) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima - Tyottomat)) %>%
  group_by(Kunta, vuosi, aluetyyppi) %>%
  summarise_at(vars(tyottomyysaste, vakanssiaste, Tyottomat), mean, na.rm = TRUE) %>%
  ungroup() %>%
  filter(Tyottomat > 10) %>%
  ggplot(aes(tyottomyysaste, vakanssiaste, size = Tyottomat)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm") +
  facet_wrap(~ aluetyyppi)

tyot_vakanssi_ku_dat %>%
  # filter(vuosi == 2018) %>%
  mutate(tyottomyysaste = Tyottomat / Tyovoima,
         vakanssiaste = Avoimet_tyopaikat / (Avoimet_tyopaikat + Tyovoima - Tyottomat)) %>%
  group_by(Maakunta, vuosi) %>%
  summarise_at(vars(tyottomyysaste, vakanssiaste, Tyottomat), mean, na.rm = TRUE) %>%
  ungroup() %>%
  # filter(Tyottomat > 1000) %>%
  ggplot(aes(tyottomyysaste, vakanssiaste, size = Tyottomat)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm") +
  facet_wrap(~ vuosi)

tyot_vakanssi_ku_dat %>%
  # filter(vuosi == 2018) %>%
  group_by(Seutukunta, vuosi) %>%
  summarise(tyottomyysaste = sum(Tyottomat, na.rm = TRUE) / sum(Tyovoima, na.rm = TRUE),
            vakanssiaste = sum(Avoimet_tyopaikat, na.rm = TRUE) / sum(Avoimet_tyopaikat + Tyovoima - Tyottomat, na.rm = TRUE),
            Tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter(Tyottomat > 1000) %>%
  ggplot(aes(tyottomyysaste, vakanssiaste, size = Tyottomat)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm") +
  facet_wrap(~ vuosi)

tyot_vakanssi_ku_dat %>%
  filter(Maakunta != "Ahvenanmaa - Åland") %>%
  # filter(vuosi == 2018) %>%
  group_by(Kunta, vuosi) %>%
  summarise(tyottomyysaste = sum(Tyottomat, na.rm = TRUE) / sum(Tyovoima, na.rm = TRUE),
            vakanssiaste = sum(Avoimet_tyopaikat, na.rm = TRUE) / sum(Tyottomat, na.rm = TRUE),
            Tyottomat = sum(Tyottomat, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter(Tyottomat > 1000) %>%
  ggplot(aes(tyottomyysaste, vakanssiaste, size = Tyottomat)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm") +
  facet_wrap(~ vuosi)

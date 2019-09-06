
table <- readRDS("data/tyottomyyden_kesto_ja_muuttaminen2015_ulos.rds")

table %>% select(-n, -muuttanut, -tyollistynyt, -muuttaessa_tyollistynyt) %>%
  gather(tiedot, value, -viim_tyott_kesto__t0_discrete) %>%
  filter(tiedot %in% c("muuttaessa_ei_tyollistynyt_prop", "muuttaessa_tyollistynyt_prop")) %>%
  ggplot(aes(x = viim_tyott_kesto__t0_discrete, y = value, fill = tiedot)) +
  geom_col()

table %>% select(-n, -muuttanut, -tyollistynyt, -muuttaessa_tyollistynyt) %>%
  gather(tiedot, value, -viim_tyott_kesto__t0_discrete) %>%
  filter(tiedot %in% c("tyollistynyt_kotikuntaan_prop", "muuttaessa_tyollistynyt_prop")) %>%
  ggplot(aes(x = viim_tyott_kesto__t0_discrete, y = value, fill = tiedot)) +
  geom_col()

table %>% mutate(liikkuva = tyollistynyt - tyollistynyt_kotikuntaan) %>%
          mutate(liikkuva_prop = liikkuva /n) %>%
          select(viim_tyott_kesto__t0_discrete, n, liikkuva_prop, tyollistynyt_kotikuntaan_prop) %>%
          gather(tiedot, value, -viim_tyott_kesto__t0_discrete,-n) %>%
          ggplot(aes(x = viim_tyott_kesto__t0_discrete, y = value, fill = tiedot)) +
                 geom_col()

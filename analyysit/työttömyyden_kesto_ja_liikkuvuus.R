library(tidyverse)

source("R/set.R")
set_proj()

data <- readRDS("data/nov1/tyottomyyden_kesto_ja_muuttaminen2012.rds") %>%
        select(viim_tyott_kesto_t0_discrete, n, tyollistynyt, muuttanut_ja_tyollistynyt_sk, muuttanut_sk, liikkunut_sk) %>%
        mutate(tyollistynyt_kotiseutukuntaan = tyollistynyt - liikkunut_sk,
               liikkunut_sk_muuttaen = muuttanut_ja_tyollistynyt_sk,
               liikkunut_sk_pendeloiden = liikkunut_sk - muuttanut_ja_tyollistynyt_sk) %>%
        mutate(liikkunut_sk_muuttaen_prop = liikkunut_sk_muuttaen / n,
               liikkunut_sk_pendeloiden_prop = liikkunut_sk_pendeloiden / n,
               tyollistynyt_kotiseutukuntaan_prop = tyollistynyt_kotiseutukuntaan / n)

liikkunut_prop <- data %>% mutate(liikkunut_prop = liikkunut_sk / tyollistynyt) %>%
  select(viim_tyott_kesto_t0_discrete, liikkunut_prop)

data %>% select(viim_tyott_kesto_t0_discrete,
                liikkunut_sk_muuttaen_prop,
                liikkunut_sk_pendeloiden_prop,
                tyollistynyt_kotiseutukuntaan_prop) %>%
         gather(tiedot, value, -viim_tyott_kesto_t0_discrete) %>%
         ggplot(aes(x = viim_tyott_kesto_t0_discrete, y = value, fill = tiedot)) +
                geom_bar(stat = "identity") +
                labs(y = "Osuus yhtä pitkään työttömänä olleista",
                     x = "Työttömyyden kesto, päiviä",
                     fill = NULL) +
                scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Blues")[2:4],
                                  labels = c("Työllistynyt ja muuttanut", "Työllistynyt ja pendelöi", "Työllistynyt kotiseutukuntaan")) +
                theme(axis.text = element_text(size = 7),
                      axis.title = element_text(size = 11),
                      legend.text = element_text(size = 12)) +
         geom_line(data = liikkunut_prop,
                   aes(x = viim_tyott_kesto_t0_discrete, y = liikkunut_prop, group = 1),
                   inherit.aes = FALSE,
                   color = "red",
                   size = 0.8)

ggsave("analyysit/Liikkuvuusvalinnat/kuvaajat/liikkuvuus_ja_tyottomyyden_kesto.png",
       width = 8, height = 4)


###################

data <- readRDS("data/nov1/tyottomyyden_kesto_ja_muuttaminen2014.rds") %>%
        dplyr::select(n, muuttanut_sk, muuttanut_ja_tyollistynyt_sk, tyollistynyt, liikkunut_sk, -viim_tyott_kesto_t0_discrete) %>%
        summarize_all(list(sum)) %>%
        mutate(tyollistynyt_kotiseutukuntaan = tyollistynyt - liikkunut_sk) %>%
        mutate(tyollistyneiden_osuus_muuttaneista = muuttanut_ja_tyollistynyt_sk / muuttanut_sk,
               tyollistyneiden_osuus = tyollistynyt / n,
               tyollistyneiden_osuus_paikallaan_pysyvista = (tyollistynyt_kotiseutukuntaan + liikkunut_sk - muuttanut_ja_tyollistynyt_sk) / (n - muuttanut_sk),
               toiseen_seutukuntaan_tyollistyneiden_osuus = liikkunut_sk / n,
               kotiseutukuntaan_tyollistyneiden_osuus = tyollistynyt_kotiseutukuntaan / n) %>%
  dplyr::select(tyollistyneiden_osuus_muuttaneista, tyollistyneiden_osuus, tyollistyneiden_osuus_paikallaan_pysyvista,
                toiseen_seutukuntaan_tyollistyneiden_osuus, kotiseutukuntaan_tyollistyneiden_osuus)%>%
       gather(tiedot, value)

data <- readRDS("data/nov1/tyottomyyden_kesto_ja_muuttaminen2015.rds") %>%
        group_by(viim_tyott_kesto_t0_discrete) %>%
  dplyr::select(n, muuttanut_sk, muuttanut_ja_tyollistynyt_sk, tyollistynyt, liikkunut_sk, viim_tyott_kesto_t0_discrete) %>%
  mutate(tyollistynyt_kotiseutukuntaan = tyollistynyt - liikkunut_sk) %>%
  mutate(tyollistyneiden_osuus_muuttaneista = muuttanut_ja_tyollistynyt_sk / muuttanut_sk,
         tyollistyneiden_osuus = tyollistynyt / n,
         tyollistyneiden_osuus_ei_muuttaneista = (tyollistynyt_kotiseutukuntaan + liikkunut_sk - muuttanut_sk) / (n - muuttanut_sk),
         toiseen_seutukuntaan_tyollistyneiden_osuus = liikkunut_sk / n,
         kotiseutukuntaan_tyollistyneiden_osuus = tyollistynyt_kotiseutukuntaan / n) %>%
  dplyr::select(tyollistyneiden_osuus_muuttaneista, tyollistyneiden_osuus, tyollistyneiden_osuus_ei_muuttaneista,
                toiseen_seutukuntaan_tyollistyneiden_osuus, kotiseutukuntaan_tyollistyneiden_osuus, viim_tyott_kesto_t0_discrete)


# Työttömyys / vakanssi ja muuttamisen suunta

library(tidyverse)

tyomarkkinadata <- readRDS("data/avoimet_tyopaikat_tyonhakijat.rds")
muuttodata <- readRDS("data/muuttodata.rds") %>%
              select(Tulokunta, Lahtokunta, Vuosi, values) %>%
              filter(Vuosi > 2005) %>%
              rename(vuosi = Vuosi)

tyomarkkinadata <- tyomarkkinadata %>%
        mutate(vakanssiaste = Avoimet_tyopaikat / Tyovoima) %>%
        mutate(tyottomyysaste = Tyottomien_osuus / 100) %>%
        mutate(vuosi = as.double(substring(Kuukausi, 1,4))) %>%
        group_by(vuosi, Kunta) %>%
        summarize(tyottomyysaste = mean(tyottomyysaste, na.rm = TRUE),
                  vakanssiaste = mean(vakanssiaste, na.rm  = TRUE))

data <- left_join(muuttodata, rename(tyomarkkinadata, Tulokunta = Kunta), by = c("Tulokunta", "vuosi")) %>%
        rename(tyottomyysaste_lahto = tyottomyysaste, vakanssiaste_lahto = vakanssiaste)
data <- left_join(data, rename(tyomarkkinadata, Lahtokunta = Kunta), by = c("Lahtokunta", "vuosi")) %>%
        rename(tyottomyysaste_tulo = tyottomyysaste, vakanssiaste_tulo = vakanssiaste)

data <- filter(data, Tulokunta != Lahtokunta)

data <- data %>%
        mutate(tyottomyysasteero = tyottomyysaste_tulo - tyottomyysaste_lahto,
               vakanssiasteero = vakanssiaste_tulo - vakanssiaste_lahto)

data %>% ggplot(aes(x = tyottomyysasteero, y = values)) +
         geom_point()

Sys.sleep(5)
5+2
Sys.sleep(2)

Sys.sleep(4)
5+5

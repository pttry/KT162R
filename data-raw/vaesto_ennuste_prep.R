# get vaestoennuste data

library(pxweb)
library(tidyverse)
library(statfitools)


# d <- pxweb_interactive("statfi")




# Download data
pop_for_dat0 <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaenn/statfin_vaenn_pxt_128v.px",
            query = list("Alue"=c("*"),
                         "Vuosi"=c("*"),
                         "Sukupuoli"=c("SSS"),
                         "Ikä"=c("*"),
                         "Tiedot"=c("vaesto_e19")))

# Huom!
# ikä yhteensä = 999

pop_fore_dat <- pop_for_dat0 %>%
  as.data.frame(column.name.type = "code", variable.value.type = "code") %>%
  clean_times() %>%
  clean_names(to_lower = TRUE) %>%
  mutate(alue = as_factor(gsub("KU", "", alue)),
         ika = as.numeric(as.character(recode(ika, "100-" = "100", "SSS" = "999")))) %>%
  select(-sukupuoli)

usethis::use_data(pop_fore_dat, overwrite = TRUE)

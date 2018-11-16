
# Etäkäytöstä tuotujen tietojen lukeminen

# Työperäinen muutto

tyo_muutto_atyyppi <- read.csv2("data-raw/muutto_atyyppi_ulos.csv") %>%
  mutate(aluetyyppi = factor(atyyppi,
                             c("pk", "yokaup", "tkakeskus", "kaup", "kaupms", "ydinms", "ms"),
                             c("PK-seutu", "Muut yliopistokaupungit", "Muut TKA keskukset",
                               "Muut kaupungit", "Kaupunkien läh. maaseutu",
                               "Ydinmaaseutu", "Harvaan asuttu maaseutu"))) %>%
  mutate(tyo_muutto_netto = tyo_muutto_tulo - tyo_muutto_lahto,
         tyopaikki_muutto_netto = tyopaikka_muutto_tulo - tyopaikka_muutto_lahto,
         tyollistyy_muutto_netto = tyollistyy_muutto_tulo - tyollistyy_muutto_lahto,
         tmuutto_netto = amuutto_tulo - amuutto_lahto) %>%
  rename(time = vuosi,
         tmuutto_tulo = amuutto_tulo,
         tmuutto_lahto = amuutto_lahto) %>%
  select(-atyyppi)


tyo_muutto_ku <- read.csv2("data-raw/muutto_ku_ulos.csv")


use_data(tyo_muutto_atyyppi, tyo_muutto_ku, overwrite = TRUE)

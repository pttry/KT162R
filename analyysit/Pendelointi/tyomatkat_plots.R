library(tidyverse)

toimialanames = c("A" = "Maa-, metsä-, ja kalatalous (A)",
                  "B" = "Kaivostoiminta louhinta (B)",
                  "C" = "Teollisuus (C)",
                  "D" = "Sähkö-, kaasu- ja lämpöhuolto, jäähdytys (D)",
                  "E" = "Vesi-, viemäri-, jätevesi- ja jätehuolto; puhtaanapito (E)",
                  "F" = "Rakentaminen (F)",
                  "G" = "Tukku- ja vähittäiskauppa: moottoriajoneuvojen korjaus (G)",
                  "H" = "Kuljetus ja varastointi (H)",
                  "I" = "Majoitus- ja ravitsemustoiminta (I)",
                  "J" = "Informaatio ja viestintä (J)",
                  "K" = "Rahoitus- ja vakuutustoiminta (K)",
                  "L" = "Kiinteistöalan toiminta (L)",
                  "M" = "Ammatillinen, tieteellinen ja tekninen toiminta (M)",
                  "N" = "Hallinto- ja tukipalvelutoiminta (N)",
                  "O" = "Julkinen hallinto, maanpuolustus, sosiaalivakuutus (O)",
                  "P" = "Koulutus (P)",
                  "Q" = "Terveys- ja sosiaalipalvelut (Q)",
                  "R" = "Taiteet, viihde ja virkistys (R)",
                  "S" = "Muu palvelutoiminta (S)",
                  "T" = "Kotitalouksien toiminta työnantajana (T)",
                  "U" = "Kansainvälisten organisaatioiden toiminta (U)",
                  "X" = "Toimiala tuntematon (X)")

############ Toimiala, nonconditional, palkansaajat ###############################################

data <- readRDS("data/oct2/salaried_toimiala.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1))

levels(data$toimiala) <- c("C", "X", "A", "B", "D", "E", "F", "G", "H", "I", "J", "K",
                           "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U")

data <- filter(data, toimiala != "X")

data$toimiala <- gdata::drop.levels(data$toimiala)

data$toimiala <- factor(data$toimiala, levels(data$toimiala)[length(levels(data$toimiala)):1])

data$toimiala <- plyr::revalue(data$toimiala, replace = toimialanames)

plot_toimiala_salaried <- data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Työmatka, mediaani ja keskiarvo, km")


############## Toimiala, nonconditional, yrittajat ###############################################

data <- readRDS("data/oct2/self_employed_toimiala.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka_suurempi_kuin_nolla, digits = 1),
         median_tyomatka = round(median_tyomatka_suurempi_kuin_nolla, digits = 1))

levels(data$toimiala) <-c("M", "N", "J", "B", "L", "P", "H", "A", "I", "S", "K", "F", "D", "R", "C", "Q", "G", "E")

data <- filter(data, toimiala != "X")

data$toimiala <- gdata::drop.levels(data$toimiala)

data$toimiala <- plyr::revalue(data$toimiala, replace = toimialanames)

plot_toimiala_selfemployed <- data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

plot_toimiala <- ggarrange(plot_toimiala_salaried, plot_toimiala_selfemployed,
                           ncol = 2, )

########## Toimiala, unconditional, both ######################################

data1 <- readRDS("data/oct2/salaried_toimiala.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1))

levels(data1$toimiala) <- c("C", "X", "A", "B", "D", "E", "F", "G", "H", "I", "J", "K",
                           "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U")

data1 <- filter(data1, toimiala != "X")
data1$toimiala <- gdata::drop.levels(data1$toimiala)
data1$toimiala <- factor(data1$toimiala, levels(data1$toimiala)[length(levels(data1$toimiala)):1])
data1$toimiala <- plyr::revalue(data1$toimiala, replace = toimialanames)
data1$amas <- "Palkansaajat"

data2 <- readRDS("data/oct2/self_employed_toimiala.rds") %>%
  select(toimiala, mean_tyomatka, median_tyomatka, n) %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1))

levels(data2$toimiala) <-c("M", "N", "J", "B", "L", "P", "H", "A", "I", "S", "K", "F", "D", "R", "C", "Q", "G", "E")

data2 <- filter(data2, toimiala != "X")
data2$toimiala <- gdata::drop.levels(data2$toimiala)
data2$toimiala <- factor(data2$toimiala, levels(data2$toimiala)[length(levels(data2$toimiala)):1])
data2$toimiala <- plyr::revalue(data2$toimiala, replace = toimialanames)
data2$amas <- "Yrittäjät"

data <- rbind(data1, data2)

plot_toimiala <- data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  facet_wrap( ~ amas, nrow = 2) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(size = 14, family = "sans")) +
  labs(x = NULL,
       y = "Työmatka, mediaani ja keskiarvo, km")

ggsave("analyysit/Pendelointi/tyomatkat_toimialoittain.png", plot_toimiala,
       width = 10, height = 16)

################ Ammatit, unconditional, both ##################################
data1 <- readRDS("data/oct2/salaried_ammatti.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1)) %>%
  filter(!is.na(ammattikoodi_k))
data1$amas <- "Palkansaajat"

data2 <- readRDS("data/oct2/self_employed_ammatti.rds") %>%
  select(ammattikoodi_k, mean_tyomatka, median_tyomatka, n) %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1)) %>%
  filter(!is.na(ammattikoodi_k))
data2$amas <- "Yrittäjät"

data <- rbind(data1, data2)

plot_ammatti <- data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(ammattikoodi_k)) %>%
  ggplot(aes(y = mean_tyomatka, x = ammattikoodi_k)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = ammattikoodi_k,
                   yend = mean_tyomatka,
                   xend = ammattikoodi_k),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = ammattikoodi_k), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = ammattikoodi_k), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = ammattikoodi_k, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = ammattikoodi_k, label = median_tyomatka), color = "white", size = 3) +
  facet_wrap( ~ amas, nrow = 2) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 12, family = "sans"),
        axis.title.x = element_text(size = 12, family = "sans")) +
  labs(x = NULL,
       y = "Työmatka, mediaani ja keskiarvo, km")

ggsave("analyysit/Pendelointi/tyomatkat_ammateittain.png", plot_ammatti,
       width = 8, height = 8)

############ Ammatit, unconditionals, palkansaajat ##############################


data <- readRDS("data/oct2/salaried_ammatti.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka, digits = 1),
         median_tyomatka = round(median_tyomatka, digits = 1)) %>%
  filter(!is.na(ammattikoodi_k))

data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(ammattikoodi_k)) %>%
  ggplot(aes(y = mean_tyomatka, x = ammattikoodi_k)) +
  geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
  geom_segment(aes(y = 0,
                   x = ammattikoodi_k,
                   yend = mean_tyomatka,
                   xend = ammattikoodi_k),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = ammattikoodi_k), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = ammattikoodi_k), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = ammattikoodi_k, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = ammattikoodi_k, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

ggsave("analyysit/Pendelointi/regressiotulokset/unconditional_ammatti_palkansaajat.png",
       width = 7, height = 4)

############## Toimiala, nonconditional, yrittajat ###############################################

data <- readRDS("data/oct2/self_employed_toimiala.rds") %>%
  mutate(mean_tyomatka = round(mean_tyomatka_suurempi_kuin_nolla, digits = 1),
         median_tyomatka = round(median_tyomatka_suurempi_kuin_nolla, digits = 1))

levels(data$toimiala) <-c("M", "N", "J", "B", "L", "P", "H", "A", "I", "S", "K", "F", "D", "R", "C", "Q", "G", "E")

data <- filter(data, toimiala != "X")

data$toimiala <- gdata::drop.levels(data$toimiala)

data$toimiala <- plyr::revalue(data$toimiala, replace = toimialanames)

data %>%
  mutate(median_tyomatka = round(median_tyomatka, digits = 2),
         mean_tyomatka = round(mean_tyomatka, digits = 2)) %>%
  filter(!is.na(toimiala)) %>%
  ggplot(aes(y = mean_tyomatka, x = toimiala)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) +
  geom_segment(aes(y = 0,
                   x = toimiala,
                   yend = mean_tyomatka,
                   xend = toimiala),
               color = "#0ABBEC",
               size = 3) +
  geom_point(aes(y = mean_tyomatka, x = toimiala), stat = "identity", color = "#006FB9", size = 10) +
  geom_point(aes(y = median_tyomatka, x = toimiala), color = "blue", size = 10) +
  geom_text(aes(y = mean_tyomatka, x = toimiala, label = mean_tyomatka), color = "white", size = 3) +
  geom_text(aes(y = median_tyomatka, x = toimiala, label = median_tyomatka), color = "white", size = 3) +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_text(size = 10, family = "sans")) +
  labs(x = NULL,
       y = "Asuin- ja työpaikan välinen etäisyys, km")

ggsave("analyysit/Pendelointi/regressiotulokset/unconditional_toimiala_yrittajat.png",
       width = 7, height = 7)

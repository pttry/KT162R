# Muuttokertymäkartta

# Hae ja muokkaa data

   data("dat_muuttotiedot_kunnittain")
   reg_keytable <- readRDS("data/reg_keytable.rds")
   kuntakoodit <- select(reg_keytable, Knro, Kunta) %>%
                  mutate(Kunta = as.character(Kunta))
   df_kunnat <- readRDS("data/spdf.rds")

# Yhdistä luvia ja juankoski lisäämällä ne dataan

   Juankoski <- dat_muuttotiedot_kunnittain %>% filter(Alue == "Kuopio")
   Juankoski$Alue <- "Juankoski"
   Luvia <- dat_muuttotiedot_kunnittain %>% filter(Alue == "Eurajoki")
   Luvia$Alue <- "Luvia"

   dat_muuttotiedot_kunnittain <- rbind(dat_muuttotiedot_kunnittain, Juankoski, Luvia)


   df <- dat_muuttotiedot_kunnittain %>%
            filter(Vuosi %in% 1990:2017) %>%
            group_by(Alue) %>%
            filter(Tiedot == "nettomuutto") %>%
            spread(Tiedot, values) %>%
            summarise(muutos = sum(nettomuutto)/mean(vakiluku)) %>%
            ungroup() %>%
            mutate(Kunta = Alue) %>%
            select(Kunta, muutos) %>%
            right_join(rename(kuntakoodit, NATCODE = "Knro"), by = "Kunta") %>%
            left_join(distinct(df_kunnat, id, NATCODE), by = "NATCODE")

# Piirrä kartta
   ggplot(
     df,
     aes(
       fill = muutos
     )
   ) +
     geom_map(aes(map_id = id), map = df_kunnat) +
     coord_fixed(0.9) +
     xlim(0, 8e+05) +
     ylim(6600000, 7800000) +
     scale_fill_gradient2(
       high = "red4",
       mid = "white",
       midpoint = 0,
       low = "darkgreen",
       name = "Muutos"
     ) +
     scale_size_manual(values = c(0.2, 1), guide = "none") +
     scale_colour_manual(values = c("grey80", "grey20"), guide = "none") +
     theme(
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       axis.title = element_blank(),
       panel.grid.major = element_blank()
     ) +
     labs(caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT")



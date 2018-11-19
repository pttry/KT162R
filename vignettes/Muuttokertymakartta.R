# Muuttokertymäkartta

# Hae ja muokkaa data
   dat_muuttotiedot_kunnittain <- readRDS("data/dat_muuttotiedot_kunnittain.rds")
   reg_keytable <- readRDS("data/reg_keytable.rds")
   kuntakoodit <- select(reg_keytable, Knro, Kunta) %>%
                  mutate(Kunta = as.character(Kunta))
   df_kunnat <- readRDS("data/spdf.rds")

# Jotain menee pieleen kuntien yhdistämisessä, tekee valkoisia läikkiä karttaan.

  # df_kunnat$NATCODE <- as.double(as.integer(df_kunnat$NATCODE))
  #  kuntakoodit$Knro <- as.double(as.integer(kuntakoodit$Knro))
#
# # Yhdistä Luvia Eurajokeen ja Juankoski Kuopioon, muuttamalla Luvian ja Juankosken kuntakoodit
   # df_kunnat$NATCODE <- fct_collapse(df_kunnat$NATCODE, "297" = c("174", "297"))
   # df_kunnat$NATCODE <- fct_collapse(df_kunnat$NATCODE, "051" = c("442", "051"))
   # kuntakoodit$Knro <- fct_collapse(kuntakoodit$Knro, "297" = c("174", "297"))
   # kuntakoodit$Knro <- fct_collapse(kuntakoodit$Knro, "051" = c("442", "051"))
   # kuntakoodit$Kunta[kuntakoodit$Kunta == "Juankoski"] <- "Kuopio"
   # kuntakoodit$Kunta[kuntakoodit$Kunta == "Luvia"] <- "Eurajoki"

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


# The geom_polygon-cowplot way

   df <- dat_muuttotiedot_kunnittain %>%
     filter(Vuosi %in% 2008:2017) %>%
     group_by(Alue) %>%
     filter(Tiedot == "nettomuutto") %>%
     spread(Tiedot, values) %>%
     summarise(muutos = sum(nettomuutto)) %>%
     ungroup() %>%
     mutate(Kunta = Alue) %>%
     select(Kunta, muutos) %>%
     right_join(rename(kuntakoodit, NATCODE = "Knro"), by = "Kunta") %>%
     left_join(distinct(df_kunnat, id, NATCODE), by = "NATCODE")

   unset_ptt()
   library(cowplot)
   df <- df %>% left_join(df_kunnat, df, by = "NATCODE")

   df_muuttovoitolliset <- filter(df, muutos >= 0)
   df_muuttotappiolliset <- filter(df, muutos < 0)

   # Tiedosto "nettomuuttokertym?karttadata.Rda" luotu scriptiss? "Muuttokertym?karttadatan luonti"
   # kansiossas "Kuvaajien R koodit/Datan siistint?"

   muuttovoitto <- ggplot() + geom_polygon(data = df_muuttovoitolliset,
                                           aes(x = long, y = lat,fill = muutos, group = group)) +
     scale_fill_gradientn(colours = colorRampPalette(c("darkgreen", "lawngreen"))(100),
                          name = "Muuttovoitto") +
     coord_fixed(0.9) +
     xlim(0, 8e+05) +
     ylim(6600000,7800000) +
     theme(axis.text = element_blank(),
           axis.line = element_blank(),
           axis.ticks = element_blank(),
           panel.border = element_blank(),
           panel.grid = element_blank(),
           axis.title = element_blank(),
           panel.grid.major = element_blank(),
           legend.position = c(0.97,0.6)) +
     labs(caption = "Lähde: PTT, Tilastokeskus",
          title = "Nettomuuttokertymä kunnittain 2008-2017")

   muuttotappio <- ggplot() + geom_polygon(data = df_muuttotappiolliset,
                                           aes(x = long, y = lat,fill = muutos, group = group)) +
     scale_fill_gradientn(colours = colorRampPalette(c("red4", "lightcoral"))(100),
                          name = "Muuttotappio") +
     coord_fixed(0.9) +
     xlim(0, 8e+05) +
     ylim(6600000,7800000) +
     theme(axis.text = element_blank(),
           axis.line = element_blank(),
           axis.ticks = element_blank(),
           panel.border = element_blank(),
           panel.grid = element_blank(),
           axis.title = element_blank(),
           panel.grid.major = element_blank(),
           legend.justification = "bottom",
           legend.position = c(1.14,0.20)) +
     labs(caption = "Lähde: PTT, Tilastokeskus",
          title = "Nettomuuttokertymä kunnittain 2008-2017")
   # Yhdist? muuttotappio- ja muuttovoittokartat

   ggdraw() + draw_plot(muuttotappio, x = -0.1, y = 0) +
     draw_plot(muuttovoitto, x = -0.1, y = 0)


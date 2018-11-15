##################### MUUTTOKERTYMÄKARTTADATA #############################################

# Load data
  dat_muuttotiedot_kunnittain <- readRDS("data/dat_muuttotiedot_kunnittain.rds")

mydata <- dat_muuttotiedot_kunnittain %>%
          filter(Tiedot == "nettomuutto") %>%
          group_by(alue) %>%
          summarize(nettomuuttokertymä = sum(values))

names(mydata) <- c("Kunta", "Nettomuuttokertymä")
keytable <- sf_get_reg_keytable()
kuntakoodit <- select(keytable, Knro, Kunta)
my_data <- right_join(mydata, kuntakoodit, by = "Kunta")
names(my_data) <- c("Kunta", "Nettomuuttokertymä", "NATCODE")
my_data <- select(my_data, Nettomuuttokertymä, NATCODE)

# Hae karttapohja
   df_kunnat <- readRDS("data/spdf.rds")

# Changing Luvia into Eurajoki and Juankoski into Kuopio
# Yhdist? Luvia Eurajokeen ja Juankoski Kuopioon, muuttamalla Luvian ja Juankosken kuntakoodit

df_kunnat$NATCODE[df_kunnat$NATCODE == 174] <- 297
df_kunnat$NATCODE[df_kunnat$NATCODE == 442] <- 151

# Lis?? muuttuja, joka indikoi, onko nettomuutto positiivinen vai negatiivinen

df_kunnat2 <- df_kunnat %>% left_join(my_data, df_kunnat, by = "NATCODE")
df_kunnat2 <- df_kunnat2 %>% mutate(Positiivinen_nettomuutto = ifelse(Nettomuuttokertymä > 0, TRUE, FALSE))

# Tallenna

setwd(wd)
save(df_kunnat2, file = "nettomuuttokertymäkarttadata.Rda")



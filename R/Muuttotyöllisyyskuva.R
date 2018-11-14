library(pxweb)
library(statfitools)
library(ggptt)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(grid)

data0 <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vamuu/statfin_vamuu_pxt_004.px",
                 dims = list(Kuukausi = c('*'),
                             Tiedot = c('04')),
                 clean = TRUE)

data <- data0 %>% mutate(Vuosi = substring(Kuukausi, 0,4)) %>%
  mutate(Kuukausi = substring(Kuukausi, 6,7)) %>%
  mutate(time = as.Date(paste(Vuosi, Kuukausi, "01", sep="-"))) %>% 
  select(values, time) %>%
  mutate(values_sa = sa_series(values, time),
         values_trend = trend_series(values, time)) %>%
  gather(value_type, value, c("values", "values_sa", "values_trend"))

data %>% filter(time >= as.Date("2000-01-01")) %>%
  ggplot(aes(x = time, y = value, col = value_type)) +
  geom_line() +
  theme_ptt() +
  scale_color_discrete(labels = c("Kuntien väliset muutot",
                                  "Kuntien väliset muutot, sa",
                                  "Kuntien väliset muutot, trend")) +
  ylab("Kuntien välisiä muuttoja") + 
  xlab(NULL) + 
  theme(legend.position = "bottom", legend.justification = "left") +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Kuntien väliset muutot kuukausittain")

tvt <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyti/statfin_tyti_pxt_005.px",
                 dims = list(Tiedot = c('Tyolliset', 'Tyotunnit'),
                             Toimiala = c('SS'),
                             Vuosi = c('*'),
                             Ajanjakso = c('*')),
                 clean = TRUE)

tvt <- clean_times(tvt, sub_year_col = "Ajanjakso", agg_time = "Vuosikeskiarvo") %>%
  mutate(value_type = ifelse(grepl("työtunnit", Tiedot), "työtunnit", "työllisten_määrä")) %>%
  select(-Tiedot, -Toimiala) %>%
  mutate(values = 1000*values) %>%
  spread(value_type, values) %>%
  mutate(työllisten_määrä_trend = trend_series(työllisten_määrä, time),
         työtunnit_trend = trend_series(työtunnit, time)) %>%
  gather(value_type, values, 2:5)

p1 <- data %>%  filter(time >= as.Date("2005-01-01")) %>%
  filter(value_type == "values_trend") %>%
  ggplot(aes(x = time, y = value, col = value_type)) +
  geom_line() +
  theme_ptt() +
  ylab("Kuntien välisiä muuttoja") +
  xlab(NULL) +    
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +          
  theme(legend.position = "bottom", legend.justification = "left",
        legend.title = element_blank()) + 
  scale_color_discrete(labels = "Kuntien välisiä muuttoja") +
  ggtitle("Kuntien välinen muutoliike ja työllisyys")


p2 <- tvt %>% filter(value_type == "työllisten_määrä_trend") %>%
  ggplot(aes(x = time, y = values, col = value_type)) +
  geom_line() +
  scale_y_continuous(position = "right", limits = c(2350000,2550000)) +
  ylab("Työllisten määrä") +
  xlab(NULL) +
  theme_ptt() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom", 
        legend.justification = "right",
        legend.title = element_blank()) +
  scale_color_discrete(labels = "Työllisten määrä") +
  scale_colour_manual(values = ggptt_palettes$ptt[1])



ggdraw() + draw_plot(p1) + draw_plot(p2, x = 0.06)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

p1 <- data %>%  filter(time >= as.Date("2005-01-01")) %>%
  filter(value_type == "values_trend") %>%
  ggplot(aes(x = time, y = value, col = value_type)) +
  geom_line() +
  theme_ptt() +
  ylab("Kuntien välisiä muuttoja") +
  xlab(NULL) +                     
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position = "bottom", legend.justification = "left",
        legend.title = element_blank()) + 
  scale_color_discrete(labels = "Kuntien välisiä muuttoja") +
  ggtitle("Kuntien välinen muutoliike ja työtunnit")


p2 <- tvt %>% filter(value_type == "työtunnit_trend") %>%
  ggplot(aes(x = time, y = values, col = value_type)) +
  geom_line() +
  scale_y_continuous(position = "right") +
  ylab("Työllisten määrä") +
  xlab(NULL) +
  theme_ptt() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom", 
        legend.justification = "right",
        legend.title = element_blank()) +
  scale_color_discrete(labels = "Työllisten määrä") +
  scale_colour_manual(values = ggptt_palettes$ptt[1])

ggdraw() + draw_plot(p1) + draw_plot(p2, x = 0.06)

p1 <- tvt %>%  filter(time >= as.Date("2005-01-01"),
                      value_type == "työllisten_määrä_trend") %>%
  ggplot(aes(x = time, y = values/1000000)) + 
  geom_line() +
  theme_ptt() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(limits = c(as.Date("2005-01-01"), as.Date("2018-07-01")), 
               date_breaks = "1 year", labels = no_century) +
  ggtitle("Työllisten määrä, milj., trendi") +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(labels = deci_comma)

p2 <- tvt %>%  filter(time >= as.Date("2005-01-01"),
                      value_type == "työtunnit_trend") %>%
  ggplot(aes(x = time, y = values/1000000)) + 
  geom_line() +
  theme_ptt() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(limits = c(as.Date("2005-01-01"), as.Date("2018-07-01")), 
               date_breaks = "1 year", labels = no_century) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Työtunnit, milj., trendi") 

p3 <- data %>% filter(time >= as.Date("2005-01-01"),
                      value_type == "values_trend") %>%
  ggplot(aes(x = time, y = value)) +
  geom_line() + 
  theme_ptt() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(limits = c(as.Date("2005-01-01"), as.Date("2018-07-01")), 
               date_breaks = "1 year", labels = no_century) +
  labs(title = "Kuntien välinen muuttoliike, muuttoja kuukaudessa, trendi",
       caption = "Lähde: Tilastokeskus, PTT")

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))

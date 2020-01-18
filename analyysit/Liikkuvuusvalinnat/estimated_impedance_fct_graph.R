library(ggplot2)

d <- 30:500
gamma <- -2.12
beta <- 0.005

tanner_fct <- function(d) {
  d^gamma * exp(d * beta)
}

data <- data.frame(d = d, impedance = tanner_fct(d))

ggplot(data, aes(x = d, y = impedance)) +
  geom_line(color = "forestgreen") +
  theme_light() +
  labs( y = "f", x = "d (km)")

ggsave("C:/Users/juhoa/Google Drive/Projects/New job, migrate or commute/Presentation/Graphs/impedance_fct_graph.png",
       width = 4 ,height = 2)

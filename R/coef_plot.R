require(tidyverse)

coef_plot <- function(data) {

   data %>% ggplot(aes(y = coefficient, x = var, label = coefficient)) +
            geom_segment(aes(y = 0,
                             x = var,
                             yend = coefficient,
                             xend = var),
                         color = "#0ABBEC",
                         size = 3) +
           geom_errorbar(aes(x = var, ymin = coefficient - 2*data$se, ymax = coefficient + 2*data$se),
                         color = "red", size = 1.2, linetype = 2) +
           geom_point(stat = "identity", color = "#006FB9", size = 15) +
           geom_text(color = "white", size = 4) +
           coord_flip() +
           theme_light() +
           geom_hline(yintercept = 0, linetype = 4, color = "black", size = 1.2) +
           theme(text = element_text(size = 20, family = "sans")) +
           labs(x = NULL,
                y = "Marginaalivaikutus")
}

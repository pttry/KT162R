#' Set theme and knitr options
#'
#'
set_proj <- function(){
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 6,
    fig.height = 5.2
  )

  ggptt::set_ptt(15, "sans")

  ggplot2::theme_update(
    legend.text = ggplot2::element_text(size = 14),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = 10, face = "plain", colour = "grey40"))
}

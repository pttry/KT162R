#' relabellin funtion for age groups
#'
#' @param x a age vector
#'
#' @export

group_age9 <- function(x){
  x <- suppressWarnings(readr::parse_number(x))
  y <- dplyr::case_when(
    x < 18 ~ "- 17",
    x < 25 ~ "18 - 24",
    x < 35 ~ "25 - 34",
    x < 45 ~ "35 - 44",
    x < 55 ~ "45 - 54",
    x < 65 ~ "55 - 64",
    x < 75 ~ "65 - 74",
    x < 101 ~ "75 -",
    is.na(x) ~ "Yhteensä"
  )
  y
}



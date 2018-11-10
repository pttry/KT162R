# Funktio, joka lisää halutun kuntaluokituksen dataan

kuntaluokka <- function(kunta, luokittelu, vuosi = 2017) 
{  
  vuosi <- as.character(vuosi)
  url = paste("http://www.tilastokeskus.fi/static/media/uploads/meta/luokitukset/kooste_",
              vuosi,
              "_kaikki_kielet.xlsx", sep = "")
  
  # if(!("kuntaluokitukset" %in% ls(globalenv()))) {
  #    if("kuntaluokitukset.Rda" %in% dir()) {
  #          load("kuntaluokitukset.Rda") 
  #  } else {
  kuntaluokitukset <- statfitools::sf_get_reg_keytable(url = url)
  keskuskunnat <- statfitools::keskuskunnat
  kuntaluokitukset <- dplyr::left_join(kuntaluokitukset, keskuskunnat, by = "Kunta")
  kuntaluokitukset$Keskusryhma[is.na(kuntaluokitukset$Keskusryhma)] <- "muu"
  # }
  #}
  
  if(!(luokittelu %in% names(kuntaluokitukset))) {
    stop("Desired luokittelu not in kuntaluokitukset.")
  }
  if(any(!(kunta %in% kuntaluokitukset$Kunta))) {
    not_found <- kunta[!(kunta %in% kuntaluokitukset$Kunta)]
    error_msg <- paste("The element(s)", 
                       paste(not_found, collapse = ", "),
                       "in input kunta not recognized.")
    stop(error_msg)
  }
  
  output <- sapply(kunta, 
                   function(y) do.call(`$`, list(kuntaluokitukset, luokittelu))[kuntaluokitukset$Kunta == y])
  names(output) <- NULL
  unlist(output)
}
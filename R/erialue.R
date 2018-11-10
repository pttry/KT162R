# Luo funktio, joka määrittää onko muutto maakunnan tai seutukunnan välinen
# Huom! kuntaluokka-funktio tulee olla saatavissa

is.erialue <- function(kunta1, kunta2, alue) {
  
  !kuntaluokka(kunta1, alue) == kuntaluokka(kunta2, alue)
  
}
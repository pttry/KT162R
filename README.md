# KT162R


## Analyysi-paketin rakenne:

### Käytettävät paketit

* DESCRIPTION -tiedoston Imports tai Depends kenttään.

### Data

* Datan haku ja muokkaus-koodit data-raw -kansiossa. Luodaan: usethis::use_data_raw()
* Data tallennetaan paketin käyttöön: usethis::use_data(dat, overwrite = TRUE)

### Analyysi

* Rmd tiedostoissa vignettes-kansiossa. Luodaan usethis::use_vignette("aihe").
* Alussa täytyy olla devtools::load_all()

### Apufunktio

* Analyysissä tai datan muokkauksessa käytettävät funktiot R-kansioon

### Analyysin seuranta ja todo

* Tähän README -tiedostoon (käytännössä jää aina tekemättä)

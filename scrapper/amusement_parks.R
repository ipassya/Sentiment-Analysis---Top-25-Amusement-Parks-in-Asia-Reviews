library(readxl)
get_amusement_parks <- function() {
  amusementParks <- read_excel('dataset/top-25-amusement-parks-asia.xlsx', sheet = 'Sheet1')
  link <- amusementParks$link
  name <- amusementParks$name
  # mendapatkan dan menyetel atribut "nama" dari vektor (termasuk daftar) atau daftar pasangan.
  names(link) <- name
  return(link)
}
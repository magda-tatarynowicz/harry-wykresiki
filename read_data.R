library(readr)
library(stringr)


read_text <- function(path, part){
  text <- read_file(paste0(path, part, ".txt"))
  text <- tolower(text)
  text <- gsub('\\r\\n', ' ', text)
  text <- gsub('[^0-9a-z ]', ' ', text)
  text <- gsub('  *', ' ', text)
  text
}

get_movies <- function() {
  movies <- NULL
  for (i in 1:8) {
    text <- read_text("movies_transcript/", i)
    movies <- rbind(movies, data.frame(part = i, text = text))
  }
  movies
} 

# zmieniłem u siebie nazwy książek na cyfry 1-7, żeby było łatwiej wczytywać. Żeby zadziałało musisz też u siebie zmienić
get_books <- function() {
  books <- NULL
  for (i in 1:7) {
    text <- read_text("books/", i)
    books <- rbind(books, data.frame(part = i, text = text))
  }
  books
}

get_spells <- function() {
  movies <- get_movies()
  books <- get_books()
  spells <- read.csv("zaklecia.txt", sep = "\t", stringsAsFactors = FALSE)[,1:3]
  
  incantations <- tolower(spells$Incantation)
  
  for (i in 1:8) {
    spells_colnames <- colnames(spells)
    spells <- cbind(spells, str_count(movies[i,2], incantations))
    colnames(spells) <- c(spells_colnames, paste0("movie", i))
  }
  
  for (i in 1:7) {
    spells_colnames <- colnames(spells)
    spells <- cbind(spells, str_count(books[i,2], incantations))
    colnames(spells) <- c(spells_colnames, paste0("book", i))
  }
  
  spells <- spells[rowSums(spells[,-(1:3)]) > 0,]
  
  spells
}

spells <- get_spells()

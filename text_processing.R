library(dplyr)
library(readr)

read_text_frame <- function(title){
  text <- read_file(title)
  text_split <- unlist(strsplit(text, "\n|\t|\r"))
  text_frame <- data.frame(line = text_split)
  text_frame <- data.frame(line = as.character(text_frame[!apply(text_frame, 1, function(x) grepl("^\\s*$", x) | x == '----------'),]), stringsAsFactors=FALSE)
  text <- do.call(rbind.data.frame, apply(text_frame, 1, function(x) unlist(strsplit(x, ":"))))
  text <- text[,-3]
  colnames(text) <- c("person", "line")
  text$person <- as.character(text$person)
  text$line <- as.character(text$line)
  
  for(i in 1:nrow(text)) {
    row <- text[i,]
    if(row[1] == row[2]){
      text[i,1] <- ""
    }
    text[i,2] <- gsub('[.,!?]', '', text[i,2])
  }
  
  # Jak chcemy mieć jedną wypowiedź na wiersz to trzeba dwie linijki poniżej usunąć
  # Teraz zostawiam słowo na wiersz
  s <- strsplit(text$line, split = " ")
  text <- data.frame(person = rep(text$person, sapply(s, length)), line = unlist(s))
  
  text
  }

part1 <- read_text_frame("the Philosophers Stone.txt")
part2 <- read_text_frame("the Chamber of Secrets.txt")

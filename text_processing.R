library(dplyr)
library(readr)

read_text_frame <- function(title){
  text <- read_file(title)
  text_split <- unlist(strsplit(text, "\n|\t|\r"))
  text_frame <- data.frame(line = text_split)
  text_frame <- data.frame(line = as.character(text_frame[!apply(text_frame, 1, function(x) grepl("^\\s*$", x) | x == '----------'),]), stringsAsFactors=FALSE)
  text <- do.call(rbind.data.frame, apply(text_frame, 1, function(x) unlist(strsplit(x, ":"))))
  text <- text[,-3]
  colnames(text) <- c("person", "word")
  text$person <- as.character(text$person)
  text$word <- as.character(text$word)
  
  for(i in 1:nrow(text)) {
    row <- text[i,]
    if(row[1] == row[2]){
      text[i,1] <- ""
    }
    text[i,2] <- tolower(gsub('[.,!?]', '', text[i,2]))
  }
  
  # Jak chcemy mieć jedną wypowiedź na wiersz to trzeba dwie linijki poniżej usunąć
  # Teraz zostawiam słowo na wiersz
  s <- strsplit(text$word, split = " ")
  text <- data.frame(person = rep(text$person, sapply(s, length)), word = unlist(s))
  
  text %>% filter(word != "" & person != "" & !grepl("Scene", person) & person != "LOCATION")
}

read_text_frame_no_colons <- function(title) {
  text <- read_file(title)
  text_split <- unlist(strsplit(text, "\r\n"))
  text_frame <- NULL
  row <- NULL
  for (i in 1:length(text_split)) {
    line <- text_split[i]
    if(line!="" & grepl("^[A-Z\\. ]*-?[A-Z\\. ]*$", line)) {
      row$person = line
    }
    else  {
      if(line == "") {
        if(!is.null(row$person) & !is.null(row$line)) {
          text_frame <- rbind(text_frame, row)
        }
        row <- NULL
      }
      else {
        row$line = ifelse(is.null(row$line), line, paste(row$line, line))
      }
    }
  }
  
  rownames(text_frame) <- NULL
  text <- as.data.frame(text_frame, stringsAsFactors=FALSE)
  text$person <- as.character(text$person)
  text$line <- as.character(text$line)
  
  for(i in 1:nrow(text)) {
    row <- text[i,]
    if(row[1] == row[2]){
      text[i,1] <- ""
    }
    text[i,2] <- tolower(gsub('[.,!?]', '', text[i,2]))
  }

  s <- strsplit(text$line, split = " ")
  text <- data.frame(person = rep(text$person, sapply(s, length)), word = unlist(s))
  
  text
}

read_plain_text <- function(title){
  text <- read_file(title)
  text_split <- unlist(strsplit(text, "\n|\t|\r"))
  text_frame <- data.frame(line = text_split)
  text_frame <- data.frame(line = as.character(text_frame[!apply(text_frame, 1, function(x) grepl("^\\s*$", x) | x == '----------'),]), stringsAsFactors=FALSE)
  
  for(i in 1:nrow(text_frame)) {
    text_frame[i,1] <- tolower(gsub('[.,!?-]', '', text_frame[i,1]))
  }
  
  s <- strsplit(text_frame$line, split = " ")
  text <- data.frame(word = unlist(s))
  text %>% filter(word != "")
}

part1 <- read_text_frame("the Philosophers Stone.txt")
part2 <- read_text_frame("the Chamber of Secrets.txt")
part3 <- read_plain_text("the Prisoner of Azkaban transcript.txt")
part4 <- read_text_frame_no_colons("the Goblet of Fire.txt")
part5 <- read_plain_text("the Order of the Phoenix transcript.txt")
part6 <- read_plain_text("the Half Blood Prince transcript.txt")
part7 <- read_plain_text("the Deathly Hallows 1 transcript.txt")
part8 <- read_plain_text("the Deathly Hallows 2 transcript.txt")

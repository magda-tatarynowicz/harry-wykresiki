library(dplyr)
library(rCharts)

count_negative_words <- function(set, negative, partNum) {
  res <- table(as.character(set$word[set$word %in% negative]))
  res <- as.data.frame(res[order(res, decreasing = TRUE)])
  colnames(res) <- c("word", "count")
  rest <- setdiff(negative, res$word)
  res <- rbind(res, data.frame(word=rest, count = rep(0,length(rest))))
  res$part <- rep(partNum, nrow(res))
  res$percentage <- 100*res$count/ nrow(set)
  res
}

spells <- read.csv("zaklecia.txt", sep = "\t", stringsAsFactors = FALSE)[,1:3]

# Poprawka na szybko dla zaklęć ze spacją. Trzeba zmienić później.
spells$Incantation <- unlist(lapply(spells$Incantation, function(spell) parts <- strsplit(spell,' ')[[1]][1]))
incorrectIncantations <- c("Switching", "Ear", "Ears", "Cave", "Unbreakable",
                           "Permanent", "Gripping", "Flame", "Horcrux")
spells <- spells[!(spells$Incantation %in% incorrectIncantations),]

spells$Incantation <- tolower(spells$Incantation)

negative_book1 <- count_negative_words(book1, spells$Incantation, 1)
negative_book2 <- count_negative_words(book2, spells$Incantation, 2)
negative_book3 <- count_negative_words(book3, spells$Incantation, 3)
negative_book4 <- count_negative_words(book4, spells$Incantation, 4)
negative_book5 <- count_negative_words(book5, spells$Incantation, 5)
negative_book6 <- count_negative_words(book6, spells$Incantation, 6)
negative_book7 <- count_negative_words(book7, spells$Incantation, 7)

negative_book <- rbind(negative_book1, negative_book2, negative_book3, negative_book4,
                       negative_book5, negative_book6, negative_book7)

not_empty <- as.character((aggregate(count ~ word, negative_book, sum) %>% filter(count>0))$word)
negative_book$word <- as.character(negative_book$word)
negative_book <- negative_book[negative_book$word %in% not_empty,]

p = nPlot(count ~ part, group =  "word", data = negative_book, type = 'stackedAreaChart')
p$chart(useInteractiveGuideline=TRUE)
p$set(width = 900, height = 800)
p


negative1 <- count_negative_words(part1, spells$Incantation, 1)
negative2 <- count_negative_words(part2, spells$Incantation, 2)
negative3 <- count_negative_words(part3, spells$Incantation, 3)
negative4 <- count_negative_words(part4, spells$Incantation, 4)
negative5 <- count_negative_words(part5, spells$Incantation, 5)
negative6 <- count_negative_words(part6, spells$Incantation, 6)
negative7 <- count_negative_words(part7, spells$Incantation, 7)
negative8 <- count_negative_words(part8, spells$Incantation, 8)

negative <- rbind(negative1, negative2, negative3, negative4,
                       negative5, negative6, negative7, negative8)

not_empty <- as.character((aggregate(count ~ word, negative, sum) %>% filter(count>0))$word)
negative$word <- as.character(negative$word)
negative <- negative[negative$word %in% not_empty,]

p = nPlot(count ~ part, group =  "word", data = negative, type = 'stackedAreaChart')
p$chart(useInteractiveGuideline=TRUE)
p$set(width = 900, height = 800)
p

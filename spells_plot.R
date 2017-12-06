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

spells <- read.csv("zaklecia.txt", sep = "\t")
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

negative_book <- negative_book %>% filter(count != 0)

p = nPlot(percentage ~ part, group =  "word", data = negative_book, type = 'stackedAreaChart')
p$chart(useInteractiveGuideline=TRUE)
p

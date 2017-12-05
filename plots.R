library(ggplot2)
library(plotly)
library(dplyr)
library(rCharts)
library(scales)

library(tidytext)
data(stop_words)

word_counts <- function(set, id){
  threshold <- 5
  word_count <- set %>% group_by(person) %>% summarise(count = n()) %>% arrange(desc(count))
  word_count["percent"] <- word_count$count / sum(word_count$count) * 100
  other_sum <- sum((word_count %>% filter(percent < threshold))$count)
  other_percent <- sum((word_count %>% filter(percent < threshold))$percent)
  word_count <- word_count[word_count$percent >= threshold, ]
  x <- data.frame("other",other_sum, other_percent)
  names(x)<-c("person","count","percent")
  newdf <- rbind(word_count, x)
  newdf["id"] <- rep(id, nrow(newdf))
  newdf
}

word_count1 <- word_counts(part1, 1)
word_count2 <- word_counts(part2, 2)
word_count4 <- word_counts(part4, 3)
word_count <- rbind(word_count1, word_count2, word_count4)
word_count$person <- tolower(word_count$person)

p1 <- ggplot() + geom_bar(aes(y = percent, x = id, fill = person), data = word_count, stat="identity") + scale_colour_brewer(palette = "Paired")
p1
ggplotly(p1)


process_sentiment <- function(set, id, scale_max){
  sentiment <- set %>% inner_join(get_sentiments("afinn"))
  df <- data.frame(score = cumsum(sentiment$score), index = seq_along(sentiment$score), Part = id, word = sentiment$word)
  df["index_scaled"] <- rescale(df$index, to = c(0, scale_max))
  df
}

movie_scale <- 800
sentiment1 <- process_sentiment(part1, "1", movie_scale)
sentiment2 <- process_sentiment(part2, "2", movie_scale)
sentiment3 <- process_sentiment(part3, "3", movie_scale)
sentiment4 <- process_sentiment(part4, "4", movie_scale)
sentiment5 <- process_sentiment(part5, "5", movie_scale)
sentiment6 <- process_sentiment(part6, "6", movie_scale)
sentiment7 <- process_sentiment(part7, "7", movie_scale)
sentiment8 <- process_sentiment(part8, "8", movie_scale)

sentiment <- rbind(sentiment1, sentiment2, sentiment3, sentiment4,
                   sentiment5, sentiment6, sentiment7, sentiment8)

p2_movie <- ggplot() + geom_line(aes(y = score, x = index_scaled, color = Part, label = word), data = sentiment) +
  scale_colour_brewer(palette = "Paired") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
p2_movie
ggplotly(p2_movie, tooltip = c("Part", "word"))

book_scale <- 10000
sentiment_book1 <- process_sentiment(book1, "1", book_scale)
sentiment_book2 <- process_sentiment(book2, "2", book_scale)
sentiment_book3 <- process_sentiment(book3, "3", book_scale)
sentiment_book4 <- process_sentiment(book4, "4", book_scale)
sentiment_book5 <- process_sentiment(book5, "5", book_scale)
sentiment_book6 <- process_sentiment(book6, "6", book_scale)
sentiment_book7 <- process_sentiment(book7, "7", book_scale)

sentiment_book <- rbind(sentiment_book1, sentiment_book2, sentiment_book3, sentiment_book4,
                        sentiment_book5, sentiment_book6, sentiment_book7)

p2_book <- ggplot() + geom_line(aes(y = score, x = index_scaled, color = Part, label = word), data = sentiment_book) + 
  scale_colour_brewer(palette = "Paired") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
p2_book
ggplotly(p2_book, tooltip = c("Part", "word"))


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

negativeWords <- c("kill", "blood", "bloody", "dead", 
                   "death", "died", "killed", "crucio", 
                   "avada", "imperio", "torture", 
                   "pain", "mudblood", "hate", 
                   "murder", "murderer", "killing", 
                   "bleeding", "sectumsempra", "damn",
                   "bitch", "git", "die", "hell", 
                   "harm", "hell", "panic", "afraid",
                   "evil", "worry", "attack")

negative1 <- count_negative_words(part1, negativeWords, 1)
negative2 <- count_negative_words(part2, negativeWords, 2)
negative3 <- count_negative_words(part3, negativeWords, 3)
negative4 <- count_negative_words(part4, negativeWords, 4)
negative5 <- count_negative_words(part5, negativeWords, 5)
negative6 <- count_negative_words(part6, negativeWords, 6)
negative7 <- count_negative_words(part7, negativeWords, 7)
negative8 <- count_negative_words(part8, negativeWords, 8)

negative <- rbind(negative1, negative2, negative3, negative4,
                  negative5, negative6, negative7, negative8)


p = nPlot(percentage ~ part, group =  "word", data = negative, type = 'stackedAreaChart')
p$chart(useInteractiveGuideline=TRUE)
p


negative_book1 <- count_negative_words(book1, negativeWords, 1)
negative_book2 <- count_negative_words(book2, negativeWords, 2)
negative_book3 <- count_negative_words(book3, negativeWords, 3)
negative_book4 <- count_negative_words(book4, negativeWords, 4)
negative_book5 <- count_negative_words(book5, negativeWords, 5)
negative_book6 <- count_negative_words(book6, negativeWords, 6)
negative_book7 <- count_negative_words(book7, negativeWords, 7)

negative_book <- rbind(negative_book1, negative_book2, negative_book3, negative_book4,
                  negative_book5, negative_book6, negative_book7)


p = nPlot(percentage ~ part, group =  "word", data = negative_book, type = 'stackedAreaChart')
p$chart(useInteractiveGuideline=TRUE)
p

# 
# sentiment1 <- part1 %>% inner_join(get_sentiments("nrc")) %>% group_by(sentiment) %>% summarize(count = n()) %>% arrange(desc(count))
# sentiment4 <- part4 %>% inner_join(get_sentiments("nrc")) %>% group_by(sentiment) %>% summarize(count = n()) %>% arrange(desc(count))
# sentiment4 <- part4 %>% inner_join(get_sentiments("afinn")) %>% group_by(score) %>% summarize(count = n()) %>% arrange(desc(count))
# 
# sentiment_group1 <- part1 %>% inner_join(get_sentiments("nrc")) %>% group_by(person, sentiment) %>% summarize(count = n()) %>% arrange(desc(count))
# sentiment_group4 <- part4 %>% inner_join(get_sentiments("nrc")) %>% group_by(person, sentiment) %>% summarize(count = n()) %>% arrange(desc(count))


# common_words1 <- part1 %>% anti_join(stop_words) %>% group_by(person, word) %>% summarise(count = n()) %>% arrange(desc(count))
# common_words_ungroup1 <- part1 %>% anti_join(stop_words) %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))
# common_words_ungroup4 <- part4 %>% anti_join(stop_words) %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))
# common_words2 <- part2 %>% anti_join(stop_words) %>% group_by(person, word) %>% summarise(count = n()) %>% arrange(desc(count))
# common_words4 <- part4 %>% anti_join(stop_words) %>% group_by(person, word) %>% summarise(count = n()) %>% arrange(desc(count))

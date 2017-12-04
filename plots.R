library(ggplot2)
library(plotly)
library(dplyr)

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


process_sentiment <- function(set, id){
  sentiment <- set %>% inner_join(get_sentiments("afinn"))
  data.frame(score = cumsum(sentiment$score), index = seq_along(sentiment$score), id = id)
}

sentiment1 <- process_sentiment(part1, "1")
sentiment2 <- process_sentiment(part2, "2")
sentiment4 <- process_sentiment(part4, "4")

sentiment <- rbind(sentiment1, sentiment2, sentiment4)

p2 <- ggplot() + geom_line(aes(y = score, x = index, color = id), data = sentiment)
p2
ggplotly(p2)
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

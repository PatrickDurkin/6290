install.packages("quanteda")
library(pillar)
library(dplyr)
library(R.utils)
library(stringr)
library(tm)
library(quanteda)
library('tidytext')
library('topicmodels')
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(DT)
library(sentimentr)
library(textdata)
library(SnowballC)
library(ggplot2)

data <- read.csv(file = 'C:/Users/yaoli/Desktop/amazon-scraper-master/Washing Machine.csv')
head(data)


summary(data)

data$rate <- as.numeric(substr(data$rating, 1,3))
tapply(data$rate, data$ASIN, mean)


data$content <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", data$content)
data$content <- gsub("[[:punct:]]","", data$content)
data$content <- gsub("\\d+","", data$content)
data$content <- gsub("\\s+"," ", data$content)

toks1 <- tokens(char_tolower(data$content))
stopwords.0 <- gsub("'", "", stopwords("english"))
toks2 <- tokens_remove(toks1, stopwords.0)

toks.uni.3 <- tokens_ngrams(toks2, 1)
dfm.uni <- dfm(toks.uni.3)
freq.all.uni <- textstat_frequency(dfm.uni)
freq.all.uni

## bigrams coverage
########################
toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

freq.all.bi <- textstat_frequency(dfm.bi)
freq.all.bi

## trigrams coverage
########################
toks3 <- tokens_ngrams(toks2, 3)
dfm.tri <- dfm(toks3)

freq.all.tri <- textstat_frequency(dfm.tri)
freq.all.tri

words <- data %>%
  select(c("ASIN", "date_info", "name", "rate", "content")) %>%
  unnest_tokens(word, content)

datatable(head(words))

afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
data.afinn <- words %>%
  inner_join(afinn, by = "word")
head(data.afinn)

word_summary <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(data$rate),score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

ggplot(data = data, mapping = aes(x = rating, fill = as.factor(rate))) +
  geom_bar() +
  labs(x = "Star rating", y = "Frequency", fill = "Star rating")


---
title: "Amazon Reviews Sentiment Analysis"
author: "Linyi Yao, huaiqian yan, Patrick Durkin"
date: "3 5 2021"
output: html_notebook
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r cars}
library(tidyverse)
library(rvest)
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(quanteda)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(NLP)
```

```{r}
scrape_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text() -> review_title
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text
  
  # Number of stars in review
  doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         page = page_num) %>% return()
}
```

Here we are scraping product pages till 50th page and creating a dataframe with bind rows. The function is returning data as lists so we need to append them together.

```{r pressure}
ASIN <- 'B0775MV9K2' # Specify ASIN
page_range <- 1:200 # Let's say we want to scrape pages 1 to 50
# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!
match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))
lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(3) # Take a three second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> amazon_reviews_list
amazon_reviews_I8 <- amazon_reviews_list %>% 
  bind_rows()
datatable(amazon_reviews_I8, class = 'cell-border stripe')
```

########TF-idf
amazon_reviews_I8$rate <- as.numeric(substr(amazon_reviews_I8$review_star, 1,3))
head(amazon_reviews_I8$sentiment)

for (i in 1: nrow(amazon_reviews_I8)){
  if (amazon_reviews_I8$rate[i] >=3){
    amazon_reviews_I8$sentiment[i] <- "positive"
  }
  else{
    amazon_reviews_I8$sentiment[i] <- "negative"
  }
}
amazon_reviews_I8<- amazon_reviews_I8[!(is.na(amazon_reviews_I8$review_text) | amazon_reviews_I8$review_text==""), ]
#head(amazon_review)
dim(amazon_reviews_I8)

data.meta<- data.frame(amazon_reviews_I8$review_text, amazon_reviews_I8$sentiment)
head(data.meta,1)
dim(data.meta)


#term frequncy
vs.text <- VectorSource(data.meta$amazon_reviews_I8.review_text)
vcorpus.review <- VCorpus(vs.text)

vcorpus.review.0 <- tm_map(vcorpus.review, content_transformer(tolower))
vcorpus.review.1 <- tm_map(vcorpus.review.0, removeNumbers)
vcorpus.review.2 <- tm_map(vcorpus.review.1, removeWords, stopwords("english"))
vcorpus.review.3 <- tm_map(vcorpus.review.2, removePunctuation)
vcorpus.review.4 <- tm_map(vcorpus.review.3, stripWhitespace)
vcorpus.review.5 <- tm_map(vcorpus.review.4, stemDocument)
####


tdm.review <- TermDocumentMatrix(vcorpus.review.5)

count.words.show <- colSums(as.matrix(tdm.review))
count.words.tf <- rowSums(as.matrix(tdm.review))
count.words.tf <- subset(count.words.tf,count.words.tf>5)
count.words.tf.sorted <- sort(count.words.tf, decreasing=TRUE)

#based on positive
data.meta.positive <- subset(data.meta,data.meta$amazon_reviews_I8.sentiment == 'positive')
data.text.positive <- data.meta.positive$amazon_reviews_I8.review_text


vs.text <- VectorSource(data.text.positive)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

par(mfrow=c(1,2))
inspect(review_corpus[1])
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm, 20)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

inspect(review_dtm_tfidf[1,1:10])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

data.meta.positive = cbind(data.meta.positive, as.matrix(review_dtm_tfidf))
data.meta.positive$amazon_review.sentiment = as.factor(data.meta.positive$amazon_review.sentiment)

datatable(data.meta.positive)
write.csv(data.meta.positive,"data.positive.csv",row.names=FALSE)


#based on negative
data.meta.negative <- subset(data.meta,data.meta$amazon_reviews_I8.sentiment == 'negative')
data.text.negative <- data.meta.positive$amazon_reviews_I8.review_text
datatable(data.meta.negative)

vs.text <- VectorSource(data.text.negative)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

par(mfrow=c(1,2))
inspect(review_corpus[1])
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm, 20)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

inspect(review_dtm_tfidf[1,1:10])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

data.meta.negative = cbind(data.meta.negative, as.matrix(review_dtm_tfidf))
data.meta.negative$amazon_reviews_I8.sentiment = as.factor(data.meta.negative$amazon_reviews_I8.sentiment)

datatable(data.meta.negative)
write.csv(data.meta.negative,"data.negative.csv",row.names=FALSE)


#bigram ranking
vs.text <- VectorSource(data.meta$amazon_reviews_I8.review_text)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

review_dtm <- DocumentTermMatrix(review_corpus)
key_words <-c(findFreqTerms(review_dtm, 500))
length(key_words)
toks1 <- tokens(char_tolower(data.meta$amazon_reviews_I8.review_text))
stopwords.0 <- gsub("'", "", stopwords("english")) ## We had removed punctuation before
toks2 <- tokens_remove(toks1, stopwords.0)

toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

toks.p <- toks3[grepl("positive", data.meta[,2])]
toks.n <- toks3[!grepl("positive", data.meta[,2])]
freq.all.bi <- textstat_frequency(dfm.bi)
all.bigrams <- unlist(freq.all.bi[,1])

bigram <- function(term.special){
  list.special <- all.bigrams[grepl(term.special, all.bigrams)]
  
  dfm.bi.token <- dfm_select(dfm.bi, list.special)
  
  set.p <- colSums(dfm.bi.token[grepl("positive", data.meta[,2]),])
  set.n <- colSums(dfm.bi.token[!grepl("positive", data.meta[,2]),])
  
  set.p <- set.p[order(set.p, decreasing=TRUE)]
  set.n <- set.n[order(set.n, decreasing=TRUE)]
  
  set.answer <- list(set.p[1:10], set.n[1:10])
  return(set.answer)
}
output.1 <- bigram(key_words[6])


save.me.1 <- data.frame(names(output.1[[1]]), output.1[[1]], names(output.1[[2]]), output.1[[2]])


positive <- c(save.me.1[,1])
positive_frequency <-c(save.me.1[,2])

negative <- c(save.me.1[,3])
negative_frequency <-c(save.me.1[,4])
df <- data.frame(positive, positive_frequency,negative,negative_frequency)
datatable(df)


##########X
ASIN <- 'B07C357FSJ' # Specify ASIN
page_range <- 1:200 # Let's say we want to scrape pages 1 to 50
# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!
match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))
lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(3) # Take a three second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> amazon_reviews_list
amazon_reviews_X <- amazon_reviews_list %>% 
  bind_rows()
datatable(amazon_reviews_X, class = 'cell-border stripe')
```

########TF-idf
amazon_reviews_X$rate <- as.numeric(substr(amazon_reviews_X$review_star, 1,3))
head(amazon_reviews_X$sentiment)

for (i in 1: nrow(amazon_reviews_X)){
  if (amazon_reviews_X$rate[i] >=3){
    amazon_reviews_X$sentiment[i] <- "positive"
  }
  else{
    amazon_reviews_X$sentiment[i] <- "negative"
  }
}
amazon_reviews_X<- amazon_reviews_X[!(is.na(amazon_reviews_X$review_text) | amazon_reviews_X$review_text==""), ]
#head(amazon_review)
dim(amazon_reviews_X)

data.meta<- data.frame(amazon_reviews_X$review_text, amazon_reviews_X$sentiment)
head(data.meta,1)
dim(data.meta)


#term frequncy
vs.text <- VectorSource(data.meta$amazon_reviews_X.review_text)
vcorpus.review <- VCorpus(vs.text)

vcorpus.review.0 <- tm_map(vcorpus.review, content_transformer(tolower))
vcorpus.review.1 <- tm_map(vcorpus.review.0, removeNumbers)
vcorpus.review.2 <- tm_map(vcorpus.review.1, removeWords, stopwords("english"))
vcorpus.review.3 <- tm_map(vcorpus.review.2, removePunctuation)
vcorpus.review.4 <- tm_map(vcorpus.review.3, stripWhitespace)
vcorpus.review.5 <- tm_map(vcorpus.review.4, stemDocument)
####


tdm.review <- TermDocumentMatrix(vcorpus.review.5)

count.words.show <- colSums(as.matrix(tdm.review))
count.words.tf <- rowSums(as.matrix(tdm.review))
count.words.tf <- subset(count.words.tf,count.words.tf>5)
count.words.tf.sorted <- sort(count.words.tf, decreasing=TRUE)

#based on positive
data.meta.positive <- subset(data.meta,data.meta$amazon_reviews_X.sentiment == 'positive')
data.text.positive <- data.meta.positive$amazon_reviews_X.review_text


vs.text <- VectorSource(data.text.positive)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

par(mfrow=c(1,2))
inspect(review_corpus[1])
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm, 20)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

inspect(review_dtm_tfidf[1,1:10])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

data.meta.positive = cbind(data.meta.positive, as.matrix(review_dtm_tfidf))
data.meta.positive$amazon_review.sentiment = as.factor(data.meta.positive$amazon_review.sentiment)

datatable(data.meta.positive)
write.csv(data.meta.positive,"data.X.positive.csv",row.names=FALSE)


#based on negative
data.meta.negative <- subset(data.meta,data.meta$amazon_reviews_X.sentiment == 'negative')
data.text.negative <- data.meta.positive$amazon_reviews_X.review_text
datatable(data.meta.negative)

vs.text <- VectorSource(data.text.negative)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

par(mfrow=c(1,2))
inspect(review_corpus[1])
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm, 20)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

inspect(review_dtm_tfidf[1,1:10])

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

data.meta.negative = cbind(data.meta.negative, as.matrix(review_dtm_tfidf))
data.meta.negative$amazon_reviews_X.sentiment = as.factor(data.meta.negative$amazon_reviews_X.sentiment)

datatable(data.meta.negative)
write.csv(data.meta.negative,"data.X.negative.csv",row.names=FALSE)


#bigram ranking
vs.text <- VectorSource(data.meta$amazon_reviews_X.review_text)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

review_dtm <- DocumentTermMatrix(review_corpus)
key_words <-c(findFreqTerms(review_dtm, 500))
length(key_words)
toks1 <- tokens(char_tolower(data.meta$amazon_reviews_X.review_text))
stopwords.0 <- gsub("'", "", stopwords("english")) ## We had removed punctuation before
toks2 <- tokens_remove(toks1, stopwords.0)

toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

toks.p <- toks3[grepl("positive", data.meta[,2])]
toks.n <- toks3[!grepl("positive", data.meta[,2])]
freq.all.bi <- textstat_frequency(dfm.bi)
all.bigrams <- unlist(freq.all.bi[,1])

bigram <- function(term.special){
  list.special <- all.bigrams[grepl(term.special, all.bigrams)]
  
  dfm.bi.token <- dfm_select(dfm.bi, list.special)
  
  set.p <- colSums(dfm.bi.token[grepl("positive", data.meta[,2]),])
  set.n <- colSums(dfm.bi.token[!grepl("positive", data.meta[,2]),])
  
  set.p <- set.p[order(set.p, decreasing=TRUE)]
  set.n <- set.n[order(set.n, decreasing=TRUE)]
  
  set.answer <- list(set.p[1:10], set.n[1:10])
  return(set.answer)
}
output.1 <- bigram(key_words[3])


save.me.1 <- data.frame(names(output.1[[1]]), output.1[[1]], names(output.1[[2]]), output.1[[2]])


positive <- c(save.me.1[,1])
positive_frequency <-c(save.me.1[,2])

negative <- c(save.me.1[,3])
negative_frequency <-c(save.me.1[,4])
df <- data.frame(positive, positive_frequency,negative,negative_frequency)
datatable(df)



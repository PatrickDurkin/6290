##############################################
## Amazon Review PreProc
##############################################

pdtest<- myfiles[2,]

pdtest


library(tidyverse)
library(rvest)
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(udpipe)
library('textdata')

require(quanteda)
require(quanteda.corpora)
require(seededlda)
require(lubridate)


data-hook="review-date" class="a-size-base a-color-secondary review-date"


---
  title: "Amazon Reviews Sentiment Analysis"
author: "Huaiqian Yan"
date: "21 4 2021"
output: html_notebook
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


```{r cars}
library(tidyverse)
library(rvest)
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(udpipe)
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
  
  
  # Date of a review
  doc %>%
    html_nodes("[data-hook='review-date']") %>%
    html_text() -> review_date
  
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         review_date,
         ASIN,
         page = page_num) %>% return()
}
```

Here we are scraping product pages till 50th page and creating a dataframe with bind rows. The function is returning data as lists so we need to append them together.

```{r pressure}

ASIN <- 'B00L1G6GCS' # Specify ASIN
page_range <- 1:50 # Let's say we want to scrape pages 1 to 50

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

amazon_reviews_df <- amazon_reviews_list %>% 
  bind_rows()

print(amazon_reviews_df[3,2])


datatable(amazon_reviews_df, class = 'cell-border stripe')



Now we need to do a bit of manipulation here as in oreder to calculate the means we need to have the rating as a single number.

```{r}
df <-amazon_reviews_df %>%
  separate(review_star, c("review", "max review"), sep = "out"  )

df$review <- as.numeric(df$review)

#drop the max review column
df<- df[,-4]

#grab all but page column
data <- df %>%
  select(1:5)

datatable(data)

## transform date column to date format
data$review_date<- gsub("Reviewed in the United States on", '', data$review_date)
data$review_date<- gsub(",",'',data$review_date)
data$review_date<- as.Date(data$review_date, format = ' %B %d %Y')







datetest<- data$review_date[1]
datetest2<- gsub("Reviewed in the United States on", '', datetest)
datetest3<- gsub(",",'',datetest2)
datetest4<- as.Date(datetest3, format = ' %B %d %Y')
datetest4

splitpd<- strsplit(datetest, split = ' ')

splitpd<- unlist(splitpd[1])

datepd <- (splitpd[7:9])
datepd<- gsub('"','',datepd)
```

In order to begin analyzing the sentiment of each review, we look at the individual sentiment of each word. Thus, we filter the reviews text to remove any punctuation and stop words then create an individual row of each word.

```{r}
words <- data %>%
  select(c("review_title", "review_text", "review")) %>%
  unnest_tokens(word, review_text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

datatable(words)
```

There are different lexicons available in the tidytext to be used like Afinn , Bing & NRC. Afinn asigns a score from -5 to 5 for each word. Bing coverts thoese scores to positive and negative sentiment.

```{r}
afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))

data.afinn <- words %>%
  inner_join(afinn, by = "word")
datatable(data.afinn)
```

```{r}
bing <- get_sentiments("bing") %>% mutate(word = wordStem(word))

data.afinn <- words %>%
  inner_join(bing, by = "word")
datatable(data.afinn)
```

We choose to use afinn lexicon here and get mean ratings and the word count.

```{r}
afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
data.afinn <- words %>%
  inner_join(afinn, by = "word")

word_summary <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))

datatable(head(word_summary))
```

Plot most common words per mean rating review and sentiment.

```{r}
ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + 
  geom_text(aes(label = word, color = count_word), position= position_jitter()) + 
  #scale_color_gradient(low = "lightblue", high = "darkblue") + 
  coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

# ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + 
#   geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + 
#   scale_color_gradient(low = "lightblue", high = "darkblue") + 
#   coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

```

```{r}
wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Dark2"))
```

Define the positive words showing up in good reviews.
```{r}
good_reviews <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(mean_rating)

wordcloud(words = good_reviews$word, freq = good_reviews$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Dark2"))
```

Define the negative words showing up in good reviews.
```{r}
bad_reviews <- data.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(review), score = max(value), count_word = n()) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)

wordcloud(words = bad_reviews$word, freq = bad_reviews$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(n = 8 ,name = "Dark2"))
```


```{r}
review_summary <- data.afinn %>%
  group_by(review_title) %>%
  summarise(mean_rating = mean(review), sentiment = mean(value))
datatable(review_summary)
```

```{r}
review_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Positive Review/Postive Sentiment",
                              mean_rating <= x_mid & sentiment > y_mid  ~ "Negative Review/Positive Sentiment",
                              mean_rating <= x_mid & sentiment <= y_mid ~ "Negative Review/Negative Sentiment",
                              TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
  ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.5) + 
  geom_vline(xintercept=x_mid, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("green", "red", "red","green")) +
  ggtitle("How buyers rated and comment the purchase") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Positive Review/Postive Sentiment") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Negative Review/Positive Sentiment") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Negative Review/Negative Sentiment") +
  geom_point()
```

However these are just individual words. We need relationships. We need to understand the intent. The challenge is to visualize this stuff as many unique sequences as they are written individual users with different feelings and intents. However, we can definitely spot interesting phrases that can be used for ad copies, updating product desriptions or removing fears by improving service.

```{r}
review_ngrams <- amazon_reviews_df %>%
  unnest_tokens(ngram, "review_text", token = "ngrams", n = 3) %>%
  group_by(ngram) %>%
  count(ngram)

separate_ngram_sequence <- review_ngrams %>%
  separate(ngram, c("word1", "word2", "word3"),
           sep = " ")

removed_stop_words <- separate_ngram_sequence %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

amazon_reviews_intent <- removed_stop_words %>%
  unite(ngram, word1, word2, word3, sep = " ")

datatable(amazon_reviews_intent)
```

Pick up adjectives showing up in the reviews and its frequency. (udpipe package)


```{r}
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(model)

textanalysis <- udpipe_annotate(udmodel_english, amazon_reviews_df$review_text)
# data frame for the output
textframe <- data.frame(textanalysis)

adj <- subset(textframe, upos %in% c("ADJ"))
adj <- txt_freq(adj$token)
adj$key <- factor(adj$key, levels = rev(adj$key))

adj %>%
  top_n(freq) %>%
  ggplot(aes(key, freq)) + geom_bar(stat = "identity") + coord_flip()
```


Automated keywords extraction using RAKE that stands for Rapid Automated Keywords Extraction.

It is one of the most popular(unsupervised) algorithms for extracting keywords in information retrieval. It look for keywords by looking a contiguous sequence of words which do not contain irrelevant words.

```{r}
rake <- keywords_rake(x = textframe, term = "lemma", group = "doc_id", relevant =  textframe$upos %in% c("NOUN", "ADJ"))
rake$key <- factor(rake$keyword, levels = rev(rake$keyword))

rake %>%
  top_n(rake, n = 20) %>%
  ggplot(aes(key, rake)) + geom_bar(stat = "identity") + coord_flip()
```





##############
```

##Quanteda LDA and adding topic column to data
text.shows <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", data$review_text)
text.shows <- gsub("^.+TITLE.END..\\s+", "", text.shows)
text.shows <- gsub("[[:punct:]]", "", text.shows)
text.shows <- gsub("\\d+", "", text.shows)
text.shows <- gsub("\\s+", " ", text.shows)



toks1 <- tokens(char_tolower(text.shows))
stopwords.0 <- gsub("'", "", stopwords("english"))
toks2 <- tokens_remove(toks1, stopwords.0)



toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

toks_news <- tokens(toks3, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("en"), "*-time", "updated-*", "*_quality", "bst"))
dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.5, termfreq_type = "quantile",
           max_docfreq = 0.8, docfreq_type = "prop")

tmod_lda <- textmodel_lda(dfmat_news, k = 4)
terms(tmod_lda, 10)


dfmat_news$topic <- topics(tmod_lda)
bigram.doctopics <- data.frame(dfmat_news@Dimnames$docs, dfmat_news$topic)

data$topics <- bigram.doctopics[,2]





########TF-idf
amazon_reviews_df$rate <- as.numeric(substr(amazon_reviews_df$review_star, 1,3))
head(amazon_reviews_df$sentiment)

for (i in 1: nrow(amazon_reviews_df)){
  if (amazon_reviews_df$rate[i] >=4){
    amazon_reviews_df$sentiment[i] <- "positive"
  }
  else{
    amazon_reviews_df$sentiment[i] <- "negative"
  }
}
amazon_reviews_df<- amazon_reviews_df[!(is.na(amazon_reviews_df$review_text) | amazon_reviews_df$review_text==""), ]
#head(amazon_review)
dim(amazon_reviews_df)

data.meta<- data.frame(amazon_reviews_df$review_text, amazon_reviews_df$sentiment)
head(data.meta,1)
dim(data.meta)


#term frequncy
vs.text <- VectorSource(data.meta$amazon_reviews_df.review_text)
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
data.meta.positive <- subset(data.meta,data.meta$amazon_reviews_df.sentiment == 'positive')
data.text.positive <- data.meta.positive$amazon_reviews_df.review_text


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
data.meta.negative <- subset(data.meta,data.meta$amazon_reviews_df.sentiment == 'negative')
data.text.negative <- data.meta.positive$amazon_reviews_df.review_text
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
data.meta.negative$amazon_reviews_df.sentiment = as.factor(data.meta.negative$amazon_reviews_df.sentiment)

datatable(data.meta.negative)
write.csv(data.meta.negative,"data.negative.csv",row.names=FALSE)


#bigram ranking
vs.text <- VectorSource(data.meta$amazon_reviews_df.review_text)
review_corpus <- VCorpus(vs.text)

review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, stripWhitespace)
review_corpus <- tm_map(review_corpus, stemDocument)

review_dtm <- DocumentTermMatrix(review_corpus)
key_words <-c(findFreqTerms(review_dtm, 90))
length(key_words)
toks1 <- tokens(char_tolower(data.meta$amazon_reviews_df.review_text))
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
output.1 <- bigram(key_words[1])
output.2 <- bigram(key_words[2])
output.3 <- bigram(key_words[3])
output.4 <- bigram(key_words[4])
output.5 <- bigram(key_words[5])

save.me.1 <- data.frame(names(output.1[[1]]), output.1[[1]], names(output.1[[2]]), output.1[[2]])
save.me.2 <- data.frame(names(output.2[[1]]), output.2[[1]], names(output.2[[2]]), output.2[[2]])
save.me.3 <- data.frame(names(output.3[[1]]), output.3[[1]], names(output.3[[2]]), output.3[[2]])
save.me.4 <- data.frame(names(output.4[[1]]), output.4[[1]], names(output.4[[2]]), output.4[[2]])
save.me.5 <- data.frame(names(output.5[[1]]), output.5[[1]], names(output.5[[2]]), output.5[[2]])


save.me.5[,4]


positive <- c(save.me.5[,1])
positive_frequency <-c(save.me.5[,2])

negative <- c(save.me.5[,3])
negative_frequency <-c(save.me.5[,4])
df <- data.frame(positive, positive_frequency,negative,negative_frequency)

























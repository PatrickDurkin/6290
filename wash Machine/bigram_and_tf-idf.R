library(SnowballC)
library(wordcloud)
install.packages('wordcloud')

setwd("C:/Users/yaoli/Desktop")
amazon_review <- read.csv("C:/Users/yaoli/Desktop/amazon-scraper-master/Washing Machine.csv", stringsAsFactors=FALSE)

amazon_review$rate <- as.numeric(substr(amazon_review$rating, 1,3))
head(amazon_review$rate)

for (i in 1: nrow(amazon_review)){
  if (amazon_review$rate[i] >=4){
    amazon_review$sentiment[i] <- "positive"
  }
  else{
    amazon_review$sentiment[i] <- "negtive"
  }
}
amazon_review<- amazon_review[!(is.na(amazon_review$content) | amazon_review$content==""), ]
#head(amazon_review)
dim(amazon_review)

data.meta<- data.frame(amazon_review$content, amazon_review$sentiment)
head(data.meta,1)
dim(data.meta)

data.meta.positive <- subset(data.meta,data.meta$amazon_review.sentiment == 'positive')
data.text.positive <- data.meta.positive$amazon_review.content
dim(data.meta.positive)

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

head(data.meta.positive)
write.csv(data.meta.positive,"data.positive.csv",row.names=FALSE)



data.meta.negtive <- subset(data.meta,data.meta$amazon_review.sentiment == 'negtive')
data.text.negtive <- data.meta.negtive$amazon_review.content
dim(data.meta.negtive)

vs.text <- VectorSource(data.text.negtive)
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

data.meta.negtive = cbind(data.meta.negtive, as.matrix(review_dtm_tfidf))
data.meta.negtive$amazon_review.sentiment = as.factor(data.meta.negtive$amazon_review.sentiment)

head(data.meta.negtive)
write.csv(data.meta.negtive,"data.negtive.csv",row.names=FALSE)



vs.text <- VectorSource(data.meta$amazon_review.content)
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
toks1 <- tokens(char_tolower(data.meta$amazon_review.content))
stopwords.0 <- gsub("'", "", stopwords("english")) ## We had removed punctuation before
toks2 <- tokens_remove(toks1, stopwords.0)

toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

toks.p <- toks3[grepl("positive", data.meta[,2])]
toks.n <- toks3[!grepl("positive", data.meta[,2])]
freq.all.bi <- textstat_frequency(dfm.bi)
all.bigrams <- unlist(freq.all.bi[,1])

do.exam.q4.1 <- function(term.special){
  list.special <- all.bigrams[grepl(term.special, all.bigrams)]
  
  dfm.bi.token <- dfm_select(dfm.bi, list.special)
  
  set.p <- colSums(dfm.bi.token[grepl("positive", data.meta[,2]),])
  set.n <- colSums(dfm.bi.token[!grepl("positive", data.meta[,2]),])
  
  set.p <- set.p[order(set.p, decreasing=TRUE)]
  set.n <- set.n[order(set.n, decreasing=TRUE)]
  
  set.answer <- list(set.p[1:10], set.n[1:10])
  return(set.answer)
}
output.1 <- do.exam.q4.1(key_words[1])
output.2 <- do.exam.q4.1(key_words[2])
output.3 <- do.exam.q4.1(key_words[3])
output.4 <- do.exam.q4.1(key_words[4])
output.5 <- do.exam.q4.1(key_words[5])

save.me.1 <- data.frame(names(output.1[[1]]), output.1[[1]], names(output.1[[2]]), output.1[[2]])
save.me.2 <- data.frame(names(output.2[[1]]), output.2[[1]], names(output.2[[2]]), output.2[[2]])
save.me.3 <- data.frame(names(output.3[[1]]), output.3[[1]], names(output.3[[2]]), output.3[[2]])
save.me.4 <- data.frame(names(output.4[[1]]), output.4[[1]], names(output.4[[2]]), output.4[[2]])
save.me.5 <- data.frame(names(output.5[[1]]), output.5[[1]], names(output.5[[2]]), output.5[[2]])













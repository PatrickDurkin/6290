library(tm)
library(stringr)
library(R.utils)
library(quanteda)
library(tidyr)
df %>% drop_na


amazon_review <- read.csv("C:/Users/yaoli/Desktop/amazon-scraper-master/Washing Machine.csv", stringsAsFactors=FALSE)
head(amazon_review[,5],1)

## Let's get some metadata from the first column

data <- amazon_review$content
data <-data[rowSums(is.na(data)) != ncol(data)]
## Some cleaning

data <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", data)
data <- gsub("[[:punct:]]", "", data)
data <- gsub("\\d+", "", data)
data <- gsub("\\s+", " ", data)

## Removing stopwords

toks1 <- tokens(char_tolower(data))
stopwords.0 <- gsub("'", "", stopwords("english")) ## We had removed punctuation before
toks2 <- tokens_remove(toks1, stopwords.0)

########################
## bigrams coverage
########################
toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

freq.all.bi <- textstat_frequency(dfm.bi)

vs.text <- VectorSource(toks2)
vcorpus.tv <- VCorpus(vs.text)

vcorpus.tv.0 <- tm_map(vcorpus.tv, content_transformer(tolower))
vcorpus.tv.1 <- tm_map(vcorpus.tv.0, removeNumbers)
vcorpus.tv.2 <- tm_map(vcorpus.tv.1, removeWords, stopwords("english"))
vcorpus.tv.3 <- tm_map(vcorpus.tv.2, removePunctuation)
vcorpus.tv.4 <- tm_map(vcorpus.tv.3, stripWhitespace)
vcorpus.tv.5 <- tm_map(vcorpus.tv.4, stemDocument)

tdm.tv.0 <- TermDocumentMatrix(vcorpus.tv.4)
tdm.tv.1 <- TermDocumentMatrix(vcorpus.tv.5)

count.words.show <- colSums(as.matrix(tdm.tv.0))
count.words.tf <- rowSums(as.matrix(tdm.tv.0))
count.words.tf.sorted <- sort(count.words.tf, decreasing=TRUE)

count.words.show.1 <- colSums(as.matrix(tdm.tv.1))
count.words.tf.1 <- rowSums(as.matrix(tdm.tv.1))
count.words.tf.sorted.1 <- sort(count.words.tf.1, decreasing=TRUE)

##
sum(count.words.tf==1)
sum(count.words.tf.1==1)

head(count.words.tf.1)
library(NLP) 

bigram.tokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
}

tdm.tv.9 <- TermDocumentMatrix(vcorpus.tv.4, control = list(tokenize=bigram.tokenizer))
tdm.tv.9

tdm.tv.8 <- TermDocumentMatrix(vcorpus.tv.5, control = list(tokenize=bigram.tokenizer))
tdm.tv.8

findMostFreqTerms(tdm.tv.9)

all.bigrams <- rownames(tdm.tv.9)


## Four different tdms

tdm.tv.0 #unigrams
tdm.tv.1 #stemmed
tdm.tv.9 #bigrams
tdm.tv.8 #bigrams stemmed

## How do we find document frequencies (vs term frequencies)?
count.words.show.1 <- colSums(as.matrix(tdm.tv.1))
count.words.show.9 <- colSums(as.matrix(tdm.tv.9))

sum(count.words.show.9>50)


## Take a given term, how do we get document frequency?

no.shows <- length(data)
tv.df.1 <- apply(as.matrix(tdm.tv.1), 1, function(x){ return(sum(x>0))})
tv.df.9 <- apply(as.matrix(tdm.tv.9), 1, function(x){ return(sum(x>0))})
tv.idf.9 <- log(no.shows/tv.df.9)

tdm.reduced.tv.9 <- removeSparseTerms(tdm.tv.9, 0.99)

tdm.matrix.tv.9 <- as.matrix(tdm.reduced.tv.9)

tdm.matrix.pc.tv.9 <- matrix(0, dim(tdm.matrix.tv.9)[1], dim(tdm.matrix.tv.9)[2])


for(j in 1:no.shows){
  cat("Iteration", j, "out of", no.shows, "\n")
  tdm.matrix.pc.tv.9[,j] <- tdm.matrix.tv.9[,j]/count.words.show.9[j]
}

colSums(tdm.matrix.pc.tv.9)

all.bigrams.reduced <- rownames(tdm.reduced.tv.9)


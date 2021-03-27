#################################
install.packages("pillar")
library(pillar)
install.packages('dplyr')
library(dplyr)
library(R.utils)
library(stringr)
library(tm)
library(quanteda)
library('tidytext')
library('topicmodels')
library(ggplot2)
library(tidyr)

##################################

setwd('~/Desktop/NLP/TermProject')

hfreviews<- read.csv('~/Desktop/NLP/TermProject/hfreviews.csv', header = FALSE)

dim(hfreviews)
head(hfreviews)


review.text<- hfreviews[2:nrow(hfreviews),4]

length(review.text)
tail(review.text)
head(review.text)
review.text <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", review.text)
review.text <- gsub("[[:punct:]]","", review.text)
review.text <- gsub("\\d+","", review.text)
review.text <- gsub("\\s+"," ", review.text)



## Removing stopwords

toks1 <- tokens(char_tolower(review.text))
stopwords.0 <- gsub("'", "", stopwords("english"))
toks2 <- tokens_remove(toks1, stopwords.0)


time1 <- Sys.time()

########################
## unigrams coverage
########################
toks.uni.3 <- tokens_ngrams(toks2, 1)
dfm.uni <- dfm(toks.uni.3)
freq.all.uni <- textstat_frequency(dfm.uni)

########################
## bigrams coverage
########################
toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

freq.all.bi <- textstat_frequency(dfm.bi)

########################
## trigrams coverage
########################
toks3 <- tokens_ngrams(toks2, 3)
dfm.tri <- dfm(toks3)

freq.all.tri <- textstat_frequency(dfm.tri)

time2 <- Sys.time()
time2-time1

no.unique.words.uni <- ntype(dfm.uni)
no.unique.words.bi <- ntype(dfm.bi)
no.unique.words.tri <- ntype(dfm.tri)



summary(no.unique.words.uni)
summary(no.unique.words.bi)
summary(no.unique.words.tri)




all.unis <- sum(freq.all.uni[,2])
all.bis  <- sum(freq.all.bi[,2])
all.tris <- sum(freq.all.tri[,2])

save.coverage <- matrix(0, 10, 3)

for(k in 7:16){
    save.coverage[k-6, 1] <- sum(freq.all.uni[1:(2^k), 2])/all.unis
    save.coverage[k-6, 2] <- sum( freq.all.bi[1:(2^k), 2])/all.bis
    save.coverage[k-6, 3] <- sum(freq.all.tri[1:(2^k), 2])/all.tris
    }

save.coverage



hf_lda<- LDA(dfm.uni, k= 2, control = list(seed=1234))

raw.sum=apply(dfm.uni,1,FUN=sum)
dfm.uni=dfm.uni[raw.sum!=0,]


hf_lda<- LDA(dfm.uni, k= 2, control = list(seed=1234))
hf_lda


ap_topics <- tidy(hf_lda, matrix = "beta")
ap_topics


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
  
  

beta_spread <- ap_topics %>%=
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
  top_n(15, log_ratio)

beta_spread


max(beta_spread[,4])


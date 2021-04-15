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

all.files<- dir(recursive=TRUE)
all.files<- all.files[grepl('csv',all.files)]




# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

head(myfiles)

myfiles[1,]

myfiles[1,1]

### getting date column format into month year and turning stars column into numeric value
myfiles[,1] <- gsub('^(.+?)\\d','',myfiles[,1])
myfiles[,5]<- as.numeric(substr(myfiles[,5],1,3))

review.text<- myfiles[,4]

str(review.text)
install.packages('NLP')
library(NLP)
review.text<- myfiles[,4]


length(review.text)
?sample

review.sample<- sample(review.text, length(review.text)*0.05)

length(review.sample)


ar.text <- VectorSource(review.text)
review.corpus <- VCorpus(ar.text)
inspect(review.corpus[[1]])

ar.sample<- VectorSource(review.sample)
sample.corpus<- VCorpus(ar.sample)






sample.corpus <- tm_map(sample.corpus, removeWords, stopwords("english"))
#remove whitespace
sample.corpus <- tm_map(sample.corpus, removePunctuation)
#Strip digits
sample.corpus <- tm_map(sample.corpus, removeNumbers)
#remove stopwords
sample.corpus <- tm_map(sample.corpus, stripWhitespace)
#Stem document
sample.corpus <- tm_map(sample.corpus, stemDocument)

inspect(pharma.vcorpus)
inspect(pharma.vcorpus[[1]])

#Good practice to check every now and then
writeLines(as.character(sample.corpus[[30]]))




sample.dtm <- DocumentTermMatrix(sample.corpus)

#collapse matrix by summing over columns
sample.freq <- colSums(as.matrix(sample.dtm))

#length should be total number of terms
length(sample.freq)

#create sort order (descending)
sample.ord <- order(sample.freq,decreasing=TRUE)

#List all terms in decreasing order of pharma.freq and write to disk
sample.freq[sample.ord][1:50]

inspect(sample.dtm)



write.csv(sample.freq[sample.ord], "word_sample.freq.csv")

###################################################################
## LDA run (it could be time consuming)
###################################################################

time1 <- Sys.time()
lda.pharma <- LDA(sample.dtm, control = list(alpha = 0.1), k = 5)
time3 <- Sys.time()
time3-time1 ## 3 minutes for 1,000

###################################################################
## LDA output
###################################################################

raw.sum=apply(sample.dtm,1,FUN=sum) #sum by raw each raw of the table
sample.dtm<- sample.dtm[raw.sum!=0,]


terms(lda.pharma,20)

#########


colnames(myfiles)

myfiles[,5]<- as.numeric(substr(myfiles[,5],1,3))


myfiles[1,]

plot(as.factor(myfiles[,5],)
unique(myfiles[,5])
library(ggplot2)
install.packages(ggpubr)

ggplot(myfiles, aes(myfiles[,5])) +
  geom_bar(fill = "#0073C2FF") +
  ggtitle("Frequency of Stars") +
  xlab("Stars") + ylab("Frequency")
 


##############################

text.shows <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", review.sample)
text.shows <- gsub("^.+TITLE.END..\\s+", "", text.shows)
text.shows <- gsub("[[:punct:]]", "", text.shows)
text.shows <- gsub("\\d+", "", text.shows)
text.shows <- gsub("\\s+", " ", text.shows)



toks1 <- tokens(char_tolower(text.shows))
stopwords.0 <- gsub("'", "", stopwords("english"))
toks2 <- tokens_remove(toks1, stopwords.0)





toks3 <- tokens_ngrams(toks2, 2)
dfm.bi <- dfm(toks3)

freq.all.bi <- textstat_frequency(dfm.bi)


dfm.bi


all.bis  <- sum(freq.all.bi[,2])


time1 <- Sys.time()
lda.pharma <- LDA(dfm.bi, control = list(alpha = 0.1), k = 5)
time3 <- Sys.time()
time3-time1 ## 3 minutes for 1,000

###################################################################
## LDA output
###################################################################

raw.sum=apply(sample.dtm,1,FUN=sum) #sum by raw each raw of the table
sample.dtm<- sample.dtm[raw.sum!=0,]


terms(lda.pharma,20)


#collapse matrix by summing over columns
sample.freq <- colSums(as.matrix(dfm.bi))

#length should be total number of terms
length(sample.freq)

#create sort order (descending)
sample.ord <- order(sample.freq,decreasing=TRUE)

#List all terms in decreasing order of pharma.freq and write to disk
sample.freq[sample.ord][1:50]

inspect(sample.dtm)



ldamodel<- textmodel_lda(dfm.bi,5)
terms(ldamodel, 10)


require(quanteda)
require(quanteda.corpora)
require(seededlda)
require(lubridate)

install.packages('quanteda.corpora')
install.packages('seededlda')
install.packages('lubridate')

toks_news <- tokens(toks3, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
toks_news <- tokens_remove(toks_news, pattern = c(stopwords("en"), "*-time", "updated-*", "quality", "bst"))
dfmat_news <- dfm(toks_news) %>% 
              dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile",
                       max_docfreq = 0.015, docfreq_type = "prop")

tmod_lda <- textmodel_lda(dfmat_news, k = 4)
terms(tmod_lda, 10)


myfiles[,5] <- as.numeric(myfiles[,5])



tmod_lda



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




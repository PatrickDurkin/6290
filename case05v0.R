###################################################################
## Skeleton for case03
###################################################################

###################################################################
## The following is a nifty way to check if a package is
## installed and if not install it.
## Better protocol than just commeting out the
## install.packages commands.
###################################################################
## Install and Load Packages ##
rm(list = ls())
packages = c("R.utils", "stringr", "xtable")
for(i in 1:length(packages)){
  if(!(packages[i] %in% as.character(installed.packages()[,1]))){
    for(j in 1:length(packages)){
      install.packages(packages[j])
    }
  }
}

###################################################################
## Load libraries
###################################################################

library(R.utils)
library(stringr)

###################################################################
## Set Working Directory.
## This you need to change to your own directory address.
###################################################################

setwd('~/Desktop/NLP/Case05.2')

###################################################################
## Loading index file created from case02
## data.edgar.8K.1994.2020.csv
## has the 8-Ks

###################################################################

dataedgar_df <- read.csv('~/Desktop/NLP/Case05.2/data.edgar.10K.1994.2020.csv', stringsAsFactors=FALSE)

dim(dataedgar_df)
head(dataedgar_df)
tail(dataedgar_df)

####################################################################
## Pick 2008 Q1
####################################################################

date.yeardg <- as.numeric(substr(dataedgar_df[,4], 1, 4))
date.monthdg <- as.numeric(substr(dataedgar_df[,4], 6, 7))
date.daydg <- as.numeric(substr(dataedgar_df[,4], 9, 10))

summary(as.factor(date.yeardg))
summary(as.factor(date.monthdg))

flag.case <- date.yeardg==2008 & (date.monthdg>=1 & date.monthdg<=3)
sum(flag.case)

data10ks <- dataedgar_df[flag.case,]
head(data10ks)
dim(data10ks)

###############################
## Let's grab N random 8Ks
###############################
ene <- 1000

time1 <- Sys.time()
set.seed(7)
random_sample_8ks <- sample(dim(data10ks)[1], ene)

#### Recycling code...
secweb <- "https://www.sec.gov/Archives/";

for(i in 1:ene){
    cat("Iteration", i, "out of ", ene, "\n")
    webaddress <- data10ks[random_sample_8ks[i], 5]
    trashme <- unlist(str_split(webaddress, "/")) ## Bad habits are hard to break...
    
    ciksave <- trashme[3] ## Saving CIK
    filename <- trashme[4] ## filename

    crawlme <- paste(secweb, webaddress, sep="")

    ## Let's find a home for the downloaded file
    ## I use my suggested subdirectory tree structure,
    ## grabbing the first four digits of the CIK and formatting
    ## accordingly.

    cik.directories <- floor(as.numeric(ciksave)/1000)
    #pads front of number with 0s
    cik.directories <- sprintf("%04d", cik.directories)
    save.subdir <- paste(cik.directories, "/", ciksave, sep="")
    dir.create(save.subdir, recursive=TRUE)    

    filename.clean <- paste(save.subdir, "/", filename, sep="") ## Nice place where to store the file
    
    ## Download file to folder
    download.file(crawlme, filename.clean)
}
time2 <- Sys.time()
time2-time1 ## 8 minutes for 1,000

######################################
## Estimating hard space needs
######################################

all.files <- dir(recursive=TRUE)
all.files <- all.files[grepl("txt", all.files)]

file.info.case <- file.info(all.files)
head(file.info.case)

sum(file.info.case[,1])
mean(file.info.case[,1])

mean(file.info.case[,1])*dim(data10ks)[1] ## Decent estimate of the space needed to store all 10Ks from Q1 2008

########################################################################################
## Note that the above estimate is about 12 Gbs (uncompressed).
## I will do 1,000 to leave space for videos....
########################################################################################

############################################################################
## Let's create an index file that has
## some metadata from the header of the
## 10-K filing.
##
## I am going to grab the
## 1 - PUBLIC DOCUMENT COUNT
## 2 - FILED AS OF DATE
## 3 - ACCEPTANCE-DATETIME
## 4 - COMPANY CONFORMED NAME
## 5 - STANDARD INDUSTRIAL CLASSIFICATION
## 6 - CITY
## 7 - STATE
############################################################################

time1 <- Sys.time()

filereading <- readLines(all.files[1])

head(filereading, 20)
TenK.timestamp <- rep(0, ene)
TenK.doc.count <- rep(0, ene)
TenK.file.date <- rep(0, ene)
TenK.sic <- rep(0, ene)
TenK.city <- rep(0, ene)
TenK.state <- rep(0, ene)
TenK.name <- rep(0, ene)

for(i in 1:ene){
#for(i in 1:10){
    cat("Iteration", i, "out of", ene, "\n")
    filereading <- readLines(all.files[i])
 
    ## Timestamp
    flag01 <- grepl("ACCEPTANCE-DATETIME", filereading)
    timestamp01 <- filereading[flag01]
    timestamp02 <- unlist(str_split(timestamp01, ">"))
    timestamp03 <- timestamp02[2]
    time01 <- substr(timestamp03, 9, 10)
    time02 <- substr(timestamp03, 11, 12)
    time03 <- substr(timestamp03, 13, 14)
    time04 <- as.numeric(time01)+as.numeric(time02)/60 + as.numeric(time03)/3600
  
    TenK.timestamp[i] <- time04

    ## Number of docs
    flag01 <- grepl("PUBLIC DOCUMENT COUNT", filereading)
    docs.info <- filereading[flag01]
    docs.info <- gsub("PUBLIC DOCUMENT COUNT:\\s+", "", docs.info)
    TenK.doc.count[i] <- docs.info

    ## File date
    flag01 <- grepl("FILED AS OF DATE", filereading)
    date.info <- filereading[flag01]
    date.info <- gsub("FILED AS OF DATE:\\s+", "", date.info)
    TenK.file.date[i] <- date.info
    
    ## Company name
    flag01 <- grepl("COMPANY CONFORMED NAME", filereading)
    name.info <- filereading[flag01]
    name.info <- gsub("\\s+COMPANY CONFORMED NAME:\\s+", "", name.info)
    TenK.name[i] <- name.info

    ## SIC
    flag01 <- grepl("STANDARD INDUSTRIAL CLASSIFICATION:", filereading)
    sic.info <- filereading[flag01]
    sic.info.1 <- paste(sic.info, collapse=";") ## Paste all lines with data using ";"s
    sic.info.2 <- gsub("\\s+STANDARD INDUSTRIAL CLASSIFICATION:\\s+", "", sic.info.1)
    TenK.sic[i] <- sic.info.2
    
    ## City
    flag01 <- grepl("CITY:", filereading)
    city.info <- filereading[flag01]
    city.info.1 <- paste(city.info, collapse=";") ## Paste all lines with data using ";"s
    city.info.2 <- gsub("\\s+CITY:\\s+", "", city.info.1)
    TenK.city[i] <- city.info.2
    
    ## State
    flag01 <- grepl("STATE:", filereading)
    state.info <- filereading[flag01]
    state.info.1 <- paste(state.info, collapse=";") ## Paste all lines with data using ";"s
    state.info.2 <- gsub("\\s+STATE:\\s+", "", state.info.1)
    TenK.state[i] <- state.info.2
}

time2 <- Sys.time()
time2-time1 ## 2.5 minutes for 1,000

###########################################################
## Let's just save the index file
###########################################################

file.out <- "index.10k.1000.20210222.csv"

TenK.indexfile <- data.frame(TenK.name, TenK.file.date, TenK.timestamp,
                             TenK.doc.count, TenK.sic, TenK.city, TenK.state,
                             all.files[1:1000])

head(TenK.indexfile)

write.csv(TenK.indexfile, file=file.out)

###########################################################
## Recycling code.
## A few modifications from the previous cleanFun.
## Main gsub from
## https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
###########################################################
cleanFun <- function(htmlString) {
    temp1 <- gsub("<DOCUMENT>", "TAGDOCUMENTXYZ2020", htmlString)
    temp1 <- gsub("<TEXT>", "TAGTEXTXYZ2020", temp1)
    temp1 <- gsub("<.*?>", "", temp1)
    temp1 <- gsub("&#\\d{2,4};", " ", temp1)
    temp1 <- gsub("&nbsp;", " ", temp1)
    temp1 <- gsub(" ", " ", temp1)
    temp1 <- replace_non_ascii(temp1)
    temp1 <- gsub("\\t", " ", temp1)
    temp1 <- gsub("\\s+", " ", temp1)
    temp1 <- gsub("^\\s+", "", temp1)
    temp1 <- gsub("\\s+$", "", temp1)
    temp1 <- temp1[!temp1==""]
  return(temp1)
}

###########################################################
## Let's read one filing.
## The key piece of the code below is a hack from:
## https://www.r-bloggers.com/htmltotext-extracting-text-from-html-via-xpath/
## Just dealing with html conventions.
## It saves the 10-K without the html code.
###########################################################
library(RCurl)
library(XML)

tryme <- 473 ## Trying one single filing
filing10k <- readLines(all.files[tryme]) 

doc <- htmlParse(filing10k, asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
annual.text <- paste(plain.text, collapse = " ")
annual.text <- gsub("\\s+", " ", annual.text)

write.csv(annual.text, "trashme", row.names=FALSE) ## For inspection, it is supposed to contain the whole 10-K stripped of html stuff



##########################################################

tryme <- 411 ## Trying one single filing
filing10k <- readLines(all.files[tryme]) 

TenK.indexfile[tryme,]

## lets NOT read the non 10K <DOCUMENT>
##only read the 10k piece

flag.doc<- grepl('<DOCUMENT>',filing10k)

list.lines<- 1:length(filing10k)

start.documents<- list.lines[flag.doc]


## Grab the 10k
##need to account for filings that only have one document
if(length(start.documents) > 1){
	lines.10k<- start.documents[1]:(start.documents[2]-1)
} else{
	lines.10k<- start.documents[1]:length(filing10k)
	}


text.10k<- filing10k[lines.10k]

doc <- htmlParse(text.10k, asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
annual.text <- paste(plain.text, collapse = " ")
annual.text <- gsub("\\s+", " ", annual.text)

write.csv(annual.text, "trashme", row.names=FALSE) ## For inspection, it is supposed to contain the whole 10-K stripped of html stuff








#grabbing all exhibits
save.exhibits<- rep(list(''), length(start.documents))

for(k in 1:(length(start.documents)-1)){
	lines.10k<- start.documents[k]:(start.documents[k+1]-1)
	save.exhibits[[k]]<- filing10k[lines.10k]
}

head(save.exhibits[[1]])



setwd('~/Desktop/NLP/Case05.2')
getwd()
library(tm)
library(RCurl)
library(XML)
all.files <- dir(recursive = TRUE)
all.files <- all.files[grepl("txt", all.files)]

ene<- 1000
master.documents<- rep(list(''), ene)

time1<- Sys.time()
for(i in 1:ene){
	cat("Iteration", i, "out of", ene, "\n")
	filing10k <- readLines(all.files[i])
	flag.doc<- grepl('<DOCUMENT>',filing10k)
	list.lines<- 1:length(filing10k)
	start.documents<- list.lines[flag.doc]
	save.exhibits<- rep(list(''), length(start.documents))
	
	if(length(start.documents) > 1){
		for(k in 1:(length(start.documents)-1)){
			lines.10k<- start.documents[k]:(start.documents[k+1]-1)
			save.exhibits[[k]]<- filing10k[lines.10k]
		}
	}else{
		lines.10k<- start.documents[1]:length(filing10k)
		save.exhibits[[1]]<- filing10k[lines.10k]
	}
	master.documents[[i]]<- save.exhibits
}
time2<- Sys.time()

time2-time1
######################################################
time1<- Sys.time()
for(i in 1:ene){
	cat("Iteration", i, "out of", ene, "\n")
	pdtext<- master.documents[[i]][[1]]
	doc2 <- htmlParse(pdtext, asText=TRUE)
	plain.text <- xpathSApply(doc2, "//text()[not(ancestor::script)]		[not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
	annual.text <- paste(plain.text, collapse = " ")
	annual.text <- gsub("\\s+", " ", annual.text)
	write.csv(annual.text, paste('trashme',i), row.names=FALSE) ## For inspection, it is supposed 		to contain the 10k document section of the 10k stripped of html stuff
	master.documents[[i]][[1]]<- annual.text
}

time2<- Sys.time()

time2-time1
######################################################

docs<- rep('',ene/4)
for(i in 1:ene/4){
	docs[i] <- master.documents[[i]][[1]]
}

time1<- Sys.time()
test.corpus<- VCorpus(VectorSource(docs))
test.corpus<- tm_map(test.corpus, content_transformer(tolower))
test.corpus<- tm_map(test.corpus, removeWords, stopwords('english'))
test.corpus<- tm_map(test.corpus, stemDocument)
time2<- Sys.time()

time2-time1


test.tdm<- TermDocumentMatrix(test.corpus)






inspect(test.tdm)























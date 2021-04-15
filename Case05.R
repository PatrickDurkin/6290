#### CASE 5 ####

work_dir<- '~/Desktop/NLP/nyt_bonds_1950'
getwd()
setwd(work_dir)

###for some reason a couple of the files arent reading in correctly, try moving to files to own folder and re trying
allfiles<- dir(recursive=TRUE)
allfiles<- allfiles[!grepl('clean',allfiles)]
allfiles<- allfiles[grepl('bonds',allfiles)]
allfiles<- allfiles[grepl('.txt',allfiles)]


dates.first<- gsub('.+_(.+)_.+','\\1',allfiles)

dates.year<- gsub('.+_(\\d+)-(\\d+)-(\\d+)_.+','\\3',allfiles)
dates.month<- gsub('.+_(\\d+)-(\\d+)-(\\d+)_.+','\\1',allfiles)
dates.day<- gsub('.+_(\\d+)-(\\d+)-(\\d+)_.+','\\2',allfiles)

dates.dg<- (1900 +as.numeric(dates.year))*10000 + as.numeric(dates.month)*100+as.numeric(dates.day)

dates.1<- as.numeric(paste(dates.year,dates.month,dates.day,sep=''))

i<-10

for(i in 1:length(allfiles)){
	cat('Iteration',i,'out of',length(allfiles),'\n')
	bondfile<- readLines(allfiles[i])
	#periods between lower case and digit make into space
	bondtext<- gsub('([a-z])\\.+(\\d)','\\1 \\2',bondfile)
	#try to grab maturity dates
	bond.maturities<- gsub('.+(\\d{4}-\\d+)\\b.+','\\1', bondtext)
	bond.
	
	
	##cleaning strips
	for(j in 1:length(bondfile)){
		bondfile[j]<- gsub(
		
		
		
####################### 

setwd('~/Desktop/NLP/Case05')


yeardg <- 2008
qtrno <- 1

filename <- paste("mastersec", yeardg, "QTR", qtrno, ".dat.gz", sep="")
    
## Create Full URL where data is held
crawlme <- paste(secweb, yeardg, "/QTR", qtrno, "/", "master.gz", sep="")
    
## Download file to folder
download.file(crawlme, filename)
    
## Unzip file
gunzip(filename)

## Create string that is the name of the unzipped file ##
filenamey  <- paste("mastersec", yeardg, "QTR", qtrno, ".dat", sep="")

dataedgar<- readLines('~/Desktop/NLP/Case05/master (1)')
head(dataedgar,15)
dataedgar<- dataedgar[12:length(dataedgar)]
head(dataedgar)


split_func <- function(x){
  trash <- str_split(x, "\\|")
  trashb <- unlist(trash)
  return(trashb)
}

## apply split_func to matrix of data ##
dataedgar_matrix <- apply(as.matrix(dataedgar), 1, split_func)

## transpose matrix ##
dataedgar_matrix <- t(dataedgar_matrix)

## Turn matrix into data frame ##
dataedgar_df <- as.data.frame(dataedgar_matrix)

## Name columns in Data Frame ##
names(dataedgar_df) <- c("CIK", "Company", "Form", "Date", "Path")

## Change class of each column in Data Frame ##
dataedgar_df$CIK = as.numeric(as.character(dataedgar_df$CIK))
dataedgar_df$Company = as.character(dataedgar_df$Company)
dataedgar_df$Form = as.character(dataedgar_df$Form)
dataedgar_df$Date = as.Date(dataedgar_df$Date)
dataedgar_df$Path = as.character(dataedgar_df$Path)


head(dataedgar_df)



secweb <- "https://www.sec.gov/Archives/";

## Doing first 200...

for(i in 1:2){
    cat("Iteration", i, "out of 100\n")
    webaddress <- dataedgar_df[i, 5]
    trashme <- unlist(str_split(webaddress, "/"))
    
    ciksave <- trashme[3]
    filename <- trashme[4]

    ## Create directory structure
    ciksave.formatted <- sprintf("%07d", as.numeric(ciksave))

    name.directory.1 <- substr(ciksave.formatted, 1, 4)
    name.directory.2 <- paste(name.directory.1, "/", ciksave.formatted, sep="")
    command.create.dir.1 <- paste("mkdir", name.directory.1)
    command.create.dir.2 <- paste("mkdir", name.directory.2)
    system(command.create.dir.1)
    system(command.create.dir.2)

    filename.with.directory <- paste(name.directory.2, "/", filename, sep="")
    
    crawlme <- paste(secweb, webaddress, sep="")
    
    ## Download file to folder
    download.file(crawlme, filename.with.directory)
}




























	
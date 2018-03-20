################################################################################
################################################################################
### Title: Exploratory Analysis of Switfkey Data
### Date: 2018/03/15
### Description:
### Author: Alvaro Ortiz <ortiz.fernandez.alvaro@gmail.com>
################################################################################
################################################################################

#required packages
library(tm)
library(SnowballC)
library(quanteda)
library(readtext)

#set the working directory and load the text file
setwd("C:/datos/Coursera/capstone/")
blogs <- readLines("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
twitter <- readLines("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
news <- readLines("Coursera-SwiftKey/final/en_US/en_US.news.txt")

#finding the longest line
output <- sapply(twitter,nchar)
head(sort(output,decreasing = TRUE))
output <- sapply(blogs,nchar)
head(sort(output,decreasing = TRUE))
output <- sapply(news,nchar)
head(sort(output,decreasing = TRUE))

#anohte rway to connect to the file
con <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt",'r')
raw.text <- readLines(con, 5)
close(con)

#calculate the love/hate ratio
love.index <- length(grep('love', twitter))
hate.index <- length(grep('hate', twitter))
love.index/hate.index

##CORPUS CREATION
#create the corpus from the raw text file
corpus <- Corpus(VectorSource((blogs)))

#apply the most common transformations to a text corpus
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
my_dfm <- DocumentTermMatrix(Corpus(VectorSource((corpus))))
inspect(frequencies[1000:1005,505:515]) #inspecting the frequency matrix
findFreqTerms(frequencies, lowfreq = 20) #ranking of the wrods used more than 20 times
sparse <- removeSparseTerms(frequencies, 0.995) #keep terms that appear in more than 0.5% of documents
tweetsSparse <- as.data.frame(as.matrix(sparse)) #convert to DF
colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) 

#------------------------------------------------------------
#quanteda
my_corpus <- corpus(blogs)
#n2_grams <- dfm(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, ngrams=2)
#n2_grams <- tokens(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, ngrams=2)
ngram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE, ngrams = 1:4)
ngram <- tolower(ngram)
my_dfm <- dfm(ngram)
freq <- topfeatures(my_dfm, nfeat(my_dfm))
summary(freq)
ndoc(my_dfm)*0.005

#--------------------------------------------------------------
#exploratory analysis
#--------------------------------------------------------------
#Some words are more frequent than others
#what are the distributions of word frequencies? 
setwd("C:/datos/Coursera/capstone/")
blogs <- readLines("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
my_corpus <- corpus(blogs)
unigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE)
unigram <- tokens_tolower(unigram)
my_dfm <- dfm(unigram)
rm(blogs)
rm(my_corpus)
head(my_dfm, 5, nf = 5)
#set.seed(31416)
#dfm_rnd <- dfm_sample(my_dfm, 5)
freqs <- docfreq(my_dfm) #number of docs a feature appears in 
#freqs2<- textstat_frequency(dfm_rnd) #general frequency information
plot(sort(log10(freqs$frequency), decreasing = TRUE))
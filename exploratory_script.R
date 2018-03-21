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
#exploratory analysis - creating unigrams dfm and freq objects
#--------------------------------------------------------------
#Some words are more frequent than others
#what are the distributions of word frequencies? 
  
  setwd("C:/datos/Coursera/capstone/final/")
  blogs <- readLines("en_US/en_US.blogs.txt")
  my_corpus <- corpus(blogs)
  unigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE)
  unigram <- tokens_tolower(unigram)
  dfm_blogs_unigram <- dfm(unigram)
  rm(blogs)
  rm(my_corpus)
  head(dfm_blogs_unigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #blogs.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_blogs_unigram <- textstat_frequency(dfm_blogs_unigram) #general frequency information
  write.table(freqs_blogs_unigram, file = "freqs_blogs_unigram")
  save("dfm_blogs_unigram", file = "dfm_blogs_unigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_blogs_unigram)
  rm(freqs_blogs_unigram)
  
  
  #now the same for news
  news <- readLines("en_US/en_US.news.txt")
  my_corpus <- corpus(news)
  unigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE)
  unigram <- tokens_tolower(unigram)
  dfm_news_unigram <- dfm(unigram)
  rm(news)
  rm(my_corpus)
  head(dfm_news_unigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #news.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_news_unigram <- textstat_frequency(dfm_news_unigram) #general frequency information
  write.table(freqs_news_unigram, file = "freqs_news_unigram")
  save("dfm_news_unigram", file = "dfm_news_unigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_news_unigram)
  rm(freqs_news_unigram)
  #plot(sort(log10(news.freqs$frequency), decreasing = TRUE))
  
  #now the same for tweets
  twitter <- readLines("en_US/en_US.twitter.txt")
  my_corpus <- corpus(twitter)
  unigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE)
  unigram <- tokens_tolower(unigram)
  dfm_twitter_unigram <- dfm(unigram)
  rm(twitter)
  rm(my_corpus)
  head(dfm_twitter_unigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #news.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_twitter_unigram<- textstat_frequency(dfm_twitter_unigram) #general frequency information
  write.table(freqs_twitter_unigram, file = "freqs_twitter_unigram")
  save("dfm_twitter_unigram", file = "dfm_twitter_unigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_twitter_unigram)
  rm(freqs_twitter_unigram)
  #plot(sort(log10(twitter.freqs$frequency), decreasing = TRUE))
  
  
  #--------------------------------------------------------------
  #exploratory analysis - creating bigrams dfm and freq objects
  #--------------------------------------------------------------
  #Some words are more frequent than others
  #what are the distributions of word frequencies? 
  
  setwd("C:/datos/Coursera/capstone/final/")
  blogs <- readLines("en_US/en_US.blogs.txt")
  my_corpus <- corpus(blogs)
  bigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE, ngrams = 2)
  bigram <- tokens_tolower(bigram)
  dfm_blogs_bigram <- dfm(bigram)
  rm(blogs)
  rm(my_corpus)
  head(dfm_blogs_bigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #blogs.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_blogs_bigram <- textstat_frequency(dfm_blogs_bigram) #general frequency information
  write.table(freqs_blogs_bigram, file = "freqs_blogs_bigram")
  save("dfm_blogs_bigram", file = "dfm_blogs_bigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_blogs_bigram)
  rm(freqs_blogs_bigram)
  
  
  #now the same for news
  news <- readLines("en_US/en_US.news.txt")
  my_corpus <- corpus(news)
  bigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE, ngrams = 2)
  bigram <- tokens_tolower(bigram)
  dfm_news_bigram <- dfm(bigram)
  rm(news)
  rm(my_corpus)
  head(dfm_news_bigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #news.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_news_bigram <- textstat_frequency(dfm_news_bigram) #general frequency information
  write.table(freqs_news_bigram, file = "freqs_news_bigram")
  save("dfm_news_bigram", file = "dfm_news_bigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_news_bigram)
  rm(freqs_news_bigram)
  #plot(sort(log10(news.freqs$frequency), decreasing = TRUE))
  
  #now the same for tweets
  twitter <- readLines("en_US/en_US.twitter.txt")
  my_corpus <- corpus(twitter)
  bigram <- tokens(x=my_corpus, remove_punct = TRUE, remove_numbers = TRUE, ngrams = 2)
  bigram <- tokens_tolower(bigram)
  dfm_twitter_bigram <- dfm(bigram)
  rm(twitter)
  rm(my_corpus)
  head(dfm_twitter_bigram, 5, nf = 5)
  #set.seed(31416)
  #dfm_rnd <- dfm_sample(my_dfm, 5)
  #news.freqs <- docfreq(my_dfm) #number of docs a feature appears in 
  freqs_twitter_bigram<- textstat_frequency(dfm_twitter_bigram) #general frequency information
  write.table(freqs_twitter_bigram, file = "freqs_twitter_bigram")
  save("dfm_twitter_bigram", file = "dfm_twitter_bigram")
  #plot(sort(log10(freqs_blogs_unigram$frequency), decreasing = TRUE))
  rm(dfm_twitter_bigram)
  rm(freqs_twitter_bigram)
  #plot(sort(log10(twitter.freqs$frequency), decreasing = TRUE))
  ##Plots a wordcloud
  set.seed(100)
  textplot_wordcloud(my_dfm, min_count = 1, 
                     random_order = FALSE,rotation = .25,
                     color = RColorBrewer::brewer.pal(8,"Dark2"))
  

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
corpus <- Corpus(VectorSource((raw.text)))

#apply the most common transformations to a text corpus
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)

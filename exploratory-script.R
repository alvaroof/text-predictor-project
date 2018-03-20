#Load the neccesary libraries
require(quanteda)
require(readtext)
require(wordcloud)
##Read the en_US files
##  blogs <- readLines("C:/datos/Coursera/capstone/final/en_US/en_US.blogs.txt", n = 100)
##  news <- readLines("C:/datos/Coursera/capstone/final/en_US/en_US.news.txt")
##  twitter <- readLines("C:/datos/Coursera/capstone/final/en_US/en_US.twitter.txt")

my_corpus <- corpus(blogs)

##  texts <- readtext("C:/datos/Coursera/capstone/final/en_US/*.txt")
##  my_corpus <- corpus(texts)

##  kwic(my_copus)


#tokenize the corpus using the n-grams option - if there is additional need for improving the performance
#the option what="fasterword" or "fastestword" can be used.
n1_gram <- tokens(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
n2_gram <- tokens(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, ngrams = 2)
n3_gram <- tokens(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, ngrams = 3)

##create a document-feature matrix from the corpus. same options as in tokens() can be used
##  my_dfm <- dfm(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove = stopwords("english"))

my_dfm <- dfm(my_corpus, remove_numbers = TRUE, remove_punct = TRUE, ngrams = 3)
topfeatures(my_dfm,20) #20 top words

##Plots a wordcloud
set.seed(100)
textplot_wordcloud(my_dfm, min_count = 1, 
                   random_order = FALSE,rotation = .25,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))
# Load in Packages
library(wordcloud)
library(RWeka)
library(tm)
library(tidyverse)

############################################################
####################### Clean Corpus #######################
############################################################

# Write a function that cleans a corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en")))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

######################################################
########## Commonality and Comparison Plots ##########
######################################################
# Create function for commonality and comparison plots
commonality_comparison_plots <- function(text_a, name_a, text_b, name_b, type = "Commonality", n_words = 100){
  # Take in two text documents, clean it and turn it into a tdm
  all_text <- c(text_a, text_b)
  all_text_v <- VectorSource(all_text)
  all_corpus <- VCorpus(all_text_v)
  all_clean <- clean_corpus(all_corpus)
  all_tdm <- TermDocumentMatrix(all_clean)
  # Determine whether commonality or comparison plots
  if(type == "Commonality"){
    all_m <- as.matrix(all_tdm)
    commonality.cloud(all_m, max.words = n_words, colors = "steelblue1")
  } else {
    colnames(all_tdm)  <- c(name_a, name_b)
    all_m <- as.matrix(all_tdm)
    comparison.cloud(all_m, colors = c("orange", "blue"), max.words = n_words/2, title.bg.colors="white")
  }
}

# Obtain text from mission statements
# http://panmore.com/ford-motor-company-vision-statement-mission-statement#:~:text=Ford's%20corporate%20mission%20is%20%E2%80%9Cto,automobiles%20and%20the%20transportation%20sector.
ford_statements <- ford_statements 
# https://corporate.ferrari.com/en/about-us/ferrari-dna#:~:text=the%20world%20dream-,Mission,World%20of%20Dreams%20and%20Emotions%E2%80%9D.
ferrari_statements <- ferrari_statements   

# Create Cloud Plots
commonality_ford_ferrari <- commonality_comparison_plots(text_a = ford_statements, name_a = "Ford", text_b = ferrari_statements, name_b = "Ferrari", type = "Commonality", n_words = 100)
comparison_for_ferrari <- commonality_comparison_plots(text_a = ford_statements, name_a = "Ford", text_b = ferrari_statements, name_b = "Ferrari", type = "Comparison", n_words = 100)

# Corpus.R
# Creates and saves a corpus of unigrams, bigrams, trigrams, and quadgrams.
library(tm)
library(dplyr)
library(RWeka)
library(stringi)
library(magrittr)
library(SnowballC)
library(ggplot2)

set.seed(2972) # for repeatability

# Set working directory for Rich's machine.
setwd("D:/Data/kavanaugh_testimony")

con <- file("kav_opening_statement.txt", "rb")
lines <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

print("--- Build Vcorpus and cleansing ---")
data <- gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", lines)

corpus <- VCorpus(VectorSource(data))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, toSpace, "[^ a-zA-Z&-]|[&-]{2,}")
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "[^\x01-\x7F]") 

# First get the word counts/frequencies
counts <- function(t) {
  f <- sort(rowSums(as.matrix(t)), decreasing = TRUE)
  return(data.frame(word = names(f), freq = f))
}


# Next, tokenize
print("--- Create anagrams ---")
bi_gram <- function(z) NGramTokenizer(z, Weka_control(min = 2, max = 2))
tri_gram <- function(z) NGramTokenizer(z, Weka_control(min = 3, max = 3))

print("--- Generating nGrams & removing Sparse Terms (infrequently appearing terms) ---")
freq_uni <- counts(removeSparseTerms(TermDocumentMatrix(corpus), 0.99))
freq_bi <- counts(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = bi_gram)), 0.999))
freq_tri <- counts(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = tri_gram)), 0.999))
                 
tdm_unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.99)
unigram_levels <- unique(tdm_unigram$dimnames$Terms)
tdm_bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = bi_gram)), 0.99)
tdm_trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = tri_gram)), 0.999)

fft <- findFreqTerms(tdm_unigram, 2, 5)


print("--- Create the plot function --- ")
gimme_Plot <- function(data, label) {
  ggplot(data[1:40,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Count/Frequency") +
    theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("purple")) }
  
print("--- Now call the plots! ---")
g1 <- gimme_Plot(freq_bi, "Unigrams")
g2 <- gimme_Plot(freq_tri, "Trigrams")

print(g1)
print(g2)


library(sentimentr)
library(syuzhet)
sent_terms <- extract_sentiment_terms(lines)
sentby <- sentiment_by(lines)
s <- filter(sentby, sentby$word_count != 0)

sdf <- as.data.frame(s)
h <- ggplot(as.data.frame(sdf), aes(x = sdf$element_id , y= sdf$ave_sentiment, color = "blue")) + geom_bar(stat = "identity", fill = I("blue"))
print(h)

g <- plot(sent_df, transformation.function = syuzhet::get_dct_transform)
print(g)

# NRC Sentiment
nrc_data <- get_nrc_sentiment(lines)
nrc_values <- get_nrc_values(lines, language="english")
nrc_values
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.8, 
  las = 1, 
  main = "Emotions in Kavanaugh Opening Statement", xlab="Percentage"
)




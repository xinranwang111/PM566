library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(forcats)
library(textdata)

# Preset ----
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))

dir <- "/Users/xinranwang/Documents/Course/25Fall/PM566"
idir <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Data"
sdir <- "/Users/xinranwang/Documents/Course/25Fall/PM566/Scripts"

filetype <- "Class1"
#source(paste0(sdir, "/", filetype, ".func.R"))

odir <- paste0(dir, "/Output")
invisible(suppressMessages(suppressWarnings((dir.create(odir, recursive = TRUE)))))

infile <- "alice.rds"

#
alice <- readRDS(paste0(idir, "/", infile)) 
alice

# tokenize
alice |>
  unnest_tokens(token, text)

alice |>
  unnest_tokens(token, text) |>
  count(token)

alice |>
  unnest_tokens(token, text) |>
  count(token, sort = TRUE)

alice |>
  unnest_tokens(token, text) |>
  count(chapter, token)

alice |>
  unnest_tokens(token, text) |>
  group_by(chapter) |>
  count(token) |>
  top_n(10, n)

alice |>
  unnest_tokens(token, text) |>
  count(token) |>
  top_n(10, n) |>
  ggplot(aes(n, token)) +
  geom_col()

alice |>
  unnest_tokens(token, text) |>
  count(token) |>
  top_n(10, n) |>
  ggplot(aes(n, fct_reorder(token, n))) +
  geom_col()

# stop words, https://smltar.com/stopwords
    # word → the stop word itself
    # lexicon → the source of the stop word list
        # 	SMART: from the SMART IR system, widely used in classic IR experiments.
        # 	Snowball: from the Snowball stemmer project (used in NLP, more linguistically motivated).
        # 	Onix: from the Onix Text Retrieval Toolkit, broad and general-purpose stopword set.

stop_words

table(stop_words$lexicon)

stop_words |>
  filter(lexicon == "snowball") |>
  pull(word)
sort(table(stop_words$word), decreasing = TRUE)

# removing stop words
alice |>
  unnest_tokens(token, text) |>
  anti_join(stop_words, by = c("token" = "word")) |>
  count(token, sort = TRUE)

alice |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = c("word")) |>
  count(word, sort = TRUE) |>
  top_n(10, n) |>
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()

# Which words appear together?
    # ngrams are sets of n consecutive words and we can count these to see which words appear together most frequently.
    # ngrams with n = 1 are called “unigrams”: “which”, “words”, “appear”, “together”
    # ngrams with n = 2 are called “bigrams”: “which words”, “words appear”, “appear together”
    # ngrams with n = 3 are called “trigrams”: “which words appear”, “words appear together”

alice |>
  unnest_ngrams(ngram, text, n = 2)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  count(ngram, sort = TRUE)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2) |>
  filter(word1 == "alice")

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2) |>
  filter(word1 == "alice") |>
  count(word2, sort = TRUE)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2) |>
  filter(word2 == "alice") |>
  count(word1, sort = TRUE)

# TF-IDF
    # TF: Term frequency
    # IDF: Inverse document frequency
    # TF-IDF: product of TF and IDF
    # TF gives weight to terms that appear a lot, IDF gives weight to terms that appears in a few documents

alice |>
  unnest_tokens(text, text)

alice |>
  unnest_tokens(text, text) |>
  count(text, chapter)

alice |>
  unnest_tokens(text, text) |>
  count(text, chapter) |>
  bind_tf_idf(text, chapter, n)

alice |>
  unnest_tokens(text, text) |>
  count(text, chapter) |>
  bind_tf_idf(text, chapter, n) |>
  arrange(desc(tf_idf))

# Sentiment Analysis
get_sentiments('bing')
get_sentiments('afinn')
get_sentiments('nrc')

alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing"))

diff_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing")) |> 
  group_by(chapter) |> 
  summarise(sentiment = sum(sentiment == "positive") - sum(sentiment == "negative"))
diff_by_chap
barplot(diff_by_chap$sentiment, names.arg = diff_by_chap$chapter)

avg_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("afinn")) |> 
  group_by(chapter) |> 
  summarise(sentiment = mean(value))
barplot(avg_by_chap$sentiment, names.arg = avg_by_chap$chapter)

alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("nrc")) |> 
  group_by(chapter) |> 
  summarise(sentiment = names(which.max(table(sentiment))))
nrc_fun <- get_sentiments("nrc")
nrc_fun <- nrc_fun[!nrc_fun$sentiment %in% c("positive","negative"), ]

alice |>
  unnest_tokens(word, text) |>
  inner_join(nrc_fun) |> 
  group_by(chapter) |> 
  summarise(sentiment = names(which.max(table(sentiment))))





























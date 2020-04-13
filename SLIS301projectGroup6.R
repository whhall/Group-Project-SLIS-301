library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(tidytext)
library(janeaustenr)
library(stringr)
library(Hmisc)
library(fs)
library(textdata)
library(wordcloud)
library(reshape2)
require(wordcloud)
require(reshape2)
library(topicmodels)
require(topicmodels)
library(tm)
require(tm)

#Pick the file labeled Tweets-food.txt
InputFile_G6 <- file.choose()
openfile_G6 <- file(InputFile_G6, open="r")
tweets_G6 <- readLines(openfile_G6)
length(tweets_G6)

#change the data to a data frame
tweets_df_G6 <- data_frame(text = tweets_G6) %>% mutate(doc_number=row_number())

#tidy the data frame
tweets_df_tidy_G6 <- tweets_df_G6 %>% unnest_tokens(word,text)

#Remove stop words from the dataframe
tweets_df_tidy_Nostopwords_G6 <- tweets_df_tidy_G6 %>% anti_join(stop_words)

#Bar chart for words used over 50 times in the data frame
tweets_df_tidy_Nostopwords_G6 %>%
  count(word,sort = TRUE) %>%
  filter(n>50) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Count of positive and negative words
tweets_bing_words_counts_G6 <- tweets_df_tidy_G6 %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

#2 bar charts comparing the positive and negative seniments
tweets_bing_words_counts_G6 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = "Words") +
  coord_flip()

#Word cloud that groups positive(green) and negative(red) words
tweets_df_tidy_G6 %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word~sentiment, value.var = 'n', fill = 0) %>%
  comparison.cloud(colors = c("red", "green"), max.words = 100)



#Preparing data for topic modeling 
corpus_G6 <- Corpus(VectorSource(tweets_G6), readerControl = list(language="en"))
corpus_G6 <- tm_map(corpus_G6, function(x) iconv(enc2utf8(x), sub = 'byte'))
corpus_G6 <- tm_map(corpus_G6, content_transformer(tolower))
corpus_G6 <- tm_map(corpus_G6, function(x) iconv(enc2utf8(x), "latin1", "ASCII", sub = ""))
corpus_G6 <- tm_map(corpus_G6, removeWords, c("http", "https"))
dtm_G6 <- DocumentTermMatrix(corpus_G6, control = list(stemming = FALSE,
                                                           stopwords = TRUE,
                                                           minWordLength=3,
                                                           removeNumbers=TRUE,
                                                           removePunctuation=TRUE))
dtm_matrix_G6 <- as.matrix(dtm_G6)
dim(dtm_matrix_G6)

rowTotals_G6 <- apply(dtm_matrix_G6, 1, sum)
dtm_matrix_G6 <- dtm_matrix_G6[rowTotals_G6>2,]
dim(dtm_matrix_G6)
tweets_lda_G6 <- LDA(dtm_matrix_G6, k = 10)
tweets_topics_G6 <- tidy(tweets_lda_G6, matrix = "beta")
tweets_top_terms_G6 <- tweets_topics_G6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Display topic groups
tweets_top_terms_G6 %>%
  mutate(term= reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
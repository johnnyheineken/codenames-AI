library(RSQLite)
library(dplyr)
library(reshape2)
library("tm")
library(stringi)
library(pbapply)
library(parallel)
library(text2vec)
library(tokenizers)
library(XML)
library(httr)
library(rvest)

load("RDatafiles//words_codenames.RData")



#### Choose one of these: ####
#load("RDatafiles//reddit_coments_sample.RData")
load("RDatafiles//wiki_good_articles.RData")
#learning_text <- readLines("RDatafiles//text8", n = 1, warn = FALSE)
#learning_text <- c(readLines("RDatafiles//TheHobbit.txt",encoding="UTF-8"),
#                   readLines("RDatafiles//LotR_FotR.txt",encoding="UTF-8"),
#                   readLines("RDatafiles//LotR_TTT.txt",encoding="UTF-8"),
#                   readLines("RDatafiles//LotR_RotK.txt",encoding="UTF-8"))



#### Learning of the text ####
it<-learning_text %>%
  itoken(tokenizer = tokenize_words, 
         lowercase = TRUE, 
         strip_punctuation = TRUE)

vocab<- it %>%
  create_vocabulary(stopwords=stopwords()) %>%
  prune_vocabulary(term_count_min = 5)

vectorizer <- vocab %>%
  vocab_vectorizer(grow_dtm = FALSE,
                   skip_grams_window = 5)

tcm <- create_tcm(it, vectorizer)

glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 20)


wv<-glove$get_word_vectors()

save(wv, file="wordvectors_wiki")
# test from paper about word2vec by  Mikolov et al.
# Paris to France is same as Berlin to Germany. This is easily quantified with vectors. 
# vec(Paris)-vec(France) = vec(Berlin) - vec(Germany)

# Now, we can ask the algorithm which was obtained through textmining
# Given the relationship of Paris to France, what is the equivalent for germany?
# answer should be berlin, and we rearange previous equation to
# vec(Paris)-vec(France) + vec(Germany) = vec(Berlin)



cos_sim = sim2(x = wv, y = wv["paris", , drop=FALSE]-wv["france", , drop=FALSE]+wv["germany", , drop=FALSE], method = "cosine", norm = "l2")


#we had asked for "vec(Paris)-vec(France)+vec(Germany)". Let's look what are the results and what is the best match.
head(sort(cos_sim[,1], decreasing = TRUE), 10)

#It works with the testing dataset from wikipedia. i am not so sure whether it works if we take subreddit data.




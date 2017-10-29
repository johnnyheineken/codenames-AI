three_word_fun<-function(vec){
  stopifnot(dim(vec)==3)
  cos_sim = sim2(x = wv, y = wv[vec[1], , drop=FALSE]-wv[vec[2], , drop=FALSE]+wv[vec[3], , drop=FALSE], method = "cosine", norm = "l2")
  result<-head(sort(cos_sim[,1], decreasing = TRUE), 5)
  result<-result[!(names(result) %in% vec)]
  result<-result[1:3]
  result<-names(result)
  
  
  return(result)
}


test_list<-list(
  c("paris","france", "germany"),
  c("man","woman","queen"),
  c("athens", "greece", "norway"),
  c("switzerland", "swiss", "dutch"),
  c("lucky", "luckiest", "easiest"),
  c("merkel", "germany", "japan"),
  c("gambling", "cards", "alcohol"),
  c("tail", "human", "animal"),
  c("beer", "prague", "kiev")
)

result<-sapply(test_list, three_word_fun)

result<-as.data.frame(result,stringsAsFactors = F)



for (i in 1:length(test_list)){
  a<-unlist(test_list[i])
  names(result)[i]<-paste(paste(a[1],a[2], sep="-"), a[3], sep="+")
}

xtable(t(as.matrix(result)), caption="results from the test on the reddit dataset")



####### hints #####
load("~/RDatafiles/wordvectors_reddit.RData")
#load("~/RDatafiles/wordvectors_wiki.RData")


load("~/RDatafiles/words_codenames.RData")
ind<-sapply(words_codenames, match, table=rownames(wv[,,drop=F]))
codenames_wv<-wv[na.omit(ind),,drop=F]
w<-function(word) {return(codenames_wv[word, , drop=FALSE])}

#Creating list of games, so we can use same games for both algorithms.
game_list<-list(game(), game(), game(), game(), game(), game(), game(), game(), game(), game(), game(), game(), game())


mat<-matrix(ncol=3, nrow=length(game_list))

for (i in 1:length(game_list)){
  a<-both_hints(game_list[[i]], c(0.5, 0.3, 0.32, 0.34, 0.36))
  wrds<-a$chosen_words
  wrds<-paste(names(wrds), collapse = " ")
  h1<-unlist(a$`hint - 1st`)
  h1<-names(h1)[1:3]
  h1<-paste(h1, collapse = " ")
  h2<-unlist(a$`hint - 2nd`)
  h2<-names(h2)[1:3]
  h2<-paste(h2, collapse = " ")
  mat[i,1]<-wrds
  mat[i,2]<-h1
  mat[i,3]<-h2
}

xtable(mat, caption = c("Results from the choosing and hinting function", "reddit comments"))


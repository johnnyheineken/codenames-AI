# previous code must be run before or:
#load("~/RDatafiles/wordvectors_reddit.RData")
load("~/RDatafiles/wordvectors_wiki.RData")


load("~/RDatafiles/words_codenames.RData")
ind<-sapply(words_codenames, match, table=rownames(wv[,,drop=F]))
codenames_wv<-wv[na.omit(ind),,drop=F]

w<-function(word) {return(codenames_wv[word, , drop=FALSE])}



####game function####
#in this function, we randomly take 25 words from the deck of ~395 words. 8 of these are for the blue player, 7 for the red player, 
# 1 is the killer, which ends the game.

game<-function(){
  OnTheTable<-sample(x = rownames(codenames_wv), 25)
  blue<-sample(OnTheTable, 8)
  red<-sample(setdiff(OnTheTable, blue), 7)
  kill<-sample(setdiff(setdiff(OnTheTable,blue),red),1)
  
  #finding best match. We find the most similar words (with cosine) from the words of the blue player. 
  blue_list<-list()
  for (j in 1:length(blue)){
    
    a <- sim2(x=codenames_wv[blue,,drop=F], #computing cosine 
              y=w(blue[j]), 
              method="cosine", 
              norm="l2")
    blue_list[[length(blue_list)+1]] <- a[,1] %>% 
      sort(decreasing = TRUE) %>% 
      head(n=5)
    names(blue_list)[length(blue_list)] <-blue[j]
  }
  
  # we apply the same also for the red player.
  red_list<-list()
  for (j in 1:length(red)){
    
    a <- sim2(x=codenames_wv[red,,drop=F], 
              y=w(red[j]), 
              method="cosine", 
              norm="l2")
    red_list[[length(red_list)+1]] <- a[,1] %>% 
      sort(decreasing = TRUE) %>% 
      head(n=5)
    names(red_list)[length(red_list)] <-red[j]
  }
  game_result<-list()
  game_result[[1]]<-blue_list
  game_result[[2]]<-red_list
  game_result[[3]]<-OnTheTable
  game_result[[4]]<-kill
  names(game_result)<-c("blue", "red", "table", "killer")
  
  # we return list with most similar words of the blue and red player. 
  # then, we simply list all of the words "on the table" and killer
  class(game_result)<-"Game"
  return(game_result)
}



thresh_5<-0.36
thresh_4<-0.34
thresh_3<-0.32
thresh_2<-0.30
thresh_1<-0.5

threshold<-c(thresh_1, thresh_2, thresh_3, thresh_4, thresh_5)






#### first hinting function ####
#We take game object specified before. 

hint_word<-function(game, n, threshold, print_result=T){
  stopifnot(class(game) == "Game")
  stopifnot(dim(threshold) == n)
  
  a<-game$blue
  
  #we now look, whether any of our list of words is capable of surpassing given threshold
  while (n>1){
    if (sapply(a,unlist)[n,] %>% max > threshold[n]) {
      
      ind <-sapply(a,unlist)[n,] %>% 
        max %>% 
        match(table = sapply(a,unlist)[n,])
      b<-a[ind]
      names(b)<-NULL
      
      # main point - we make vector in a way that we simply add all of chosen vectors together. 
      # As we are doing cosine similarity, there is no need to scale the result vector
      vector<-b %>% 
        unlist %>% 
        names %>% 
        sapply(w) %>% 
        rowSums %>% 
        as.matrix %>%
        t
      
      # computing cosine similarity
      cos_sim <-sim2(x=wv, y=(vector), method="cosine", norm="l2")
      guess<-head(sort(cos_sim[,1], decreasing=T),15)
      chosen_words<-unlist(b)[1:n]
      
      result<-list(chosen_words, guess)
      names(result)<-c("chosen_words", "guess")
      
      return(result)
      
      
      break
    } else {
      n<-n-1}
  }
}

hint_word(game(), 5, threshold)

#### second hinting function ####

cos_sim_list <- function(word_name){
  a<-sim2(x=wv,y=w(word_name), method="cosine", norm="l2")
  b<-sort(a[,1], decreasing = T)[1:1000]
  return(b)
}



hint_word.2<-function(game, n, threshold){
  stopifnot(class(game) == "Game")
  stopifnot(dim(threshold) == n)
  blue<-game$blue
  
  #we now look, whether any of our list of words is capable of surpassing given threshold
  while (n>1){
    if (sapply(blue,unlist)[n,] %>% max > threshold[n]) {
      ind <-sapply(blue,unlist)[n,] %>% 
        max %>% 
        match(table = sapply(blue,unlist)[n,])
      b<-blue[ind]
      names(b)<-NULL
      b_names<-b %>%
        unlist %>%
        names 
      b_names<-b_names[1:n]
      
      most_similar_words<-lapply(b_names, FUN = cos_sim_list)
      
      intersect<-Reduce(intersect, lapply(most_similar_words, names))
      
      sum_of_words<-tapply(unlist(most_similar_words), names(unlist(most_similar_words)), sum)
      
      guess<-head(sort(sum_of_words, decreasing=TRUE)[intersect], 15)
      
      
      result<-list(b_names, guess, sum_of_words, intersect)
      names(result)<-c("chosen_words", "guess", "sum_of_words", "intersect")
      
      if (length(guess) != 0){
        return(result)
        break
      }
      
      
    }
    else{
      n<-n-1
      if (n==1) {print("Nothing found")}
    }
  }
  
}

g<-hint_word.2(game(), n = 5, threshold)

#These are the words that algorithm chose from the game that these will be the best to connect:
g$chosen_words

#These are possible guesses chosen by algorithm. 
g$guess

#### result of both guesses at the same time ####
both_hints<-function(game, threshold){
  stopifnot(class(game)=="Game")
  
  g1<-hint_word(game, n = 5, threshold)
  g2<-hint_word.2(game, n = 5, threshold)
  
  hint1<- g1$guess[!(names(g1$guess) %in% stopwords())] # removing stopwords and words already in the chosen words
  hint1<- hint1[!(names(hint1) %in% names(g1$chosen_words))]
  hint2<- g2$guess[!(names(g2$guess) %in% stopwords())]
  hint2<- hint2[!(names(hint2) %in% names(g1$chosen_words))]
  
  result<-list(g1$chosen_words, hint1, hint2)
  names(result)<-c("chosen_words", "hint - 1st", "hint - 2nd")
  return(result)
  
}



a<-both_hints(game(), c(0.5, 0.32, 0.34, 0.36, 0.38))
a

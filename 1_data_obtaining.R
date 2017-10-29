#### Explanation ####

#We used two main sources of data
#May 2015 reddit comments, available on kaggle.com (30GB sqlite database)
#Wikipedia articles (downloaded directly form wikipedia via wikipedia API)
#in the first case, it is useless to download whole database when correcting the code.
#in the second case, download of 25 thousand articles takes about 3 hours, so it is useless as well.
#Therefore,  this is why we divided the code in two parts. This part is not directly needed for the code to be working, 
#but it shows the complexity of the data obtaining.


#database of reddit comments is in the folder "datadrive" 
#(as well as 1 billion word dataset, but I havent tried to make it work, yet)

# all scripts are stored in the folder "scripts"

# directly loadable elements such as already processed reddit database dump or wikipedia extract 
# are stored in the folder RDatafiles.


#### Used libraries + explanation ####
library(RSQLite) #to manipulate with sql database
library(dplyr) #manipulating directly with sql database, also pipe operator %>%
library(reshape2) #table manipulation
library(httr) #for wikipedia downloading
library(rvest) # for scraping list of wikipedia links
library(stringr) # manipulation with text


#### Obtaining Reddit Comments ####
db <- src_sqlite('database.sqlite', create = F)

#obtaining list of subreddits. This requires sqlite database of May2015 reddit comments. it can be downloaded on kaggle.:
reddit_all<-tbl(db, "May2015")
subreddit_list<-reddit_all %>% 
  select(subreddit) %>% 
  distinct %>% 
  collect 
subreddit_list<-subreddit_list$subreddit



#okay, let's see distribution. How many comments are added in individual subreddits? 
for_plot<-reddit_all %>% 
  group_by(subreddit) %>% 
  summarise(n()) %>%
  collect
plot(sort(for_plot$`COUNT()`))

# there is a lot of almost inactive subbredits, we will omit them.
subreddit_list<-reddit_all %>% 
  group_by(subreddit) %>% 
  summarise(n()) %>% 
  filter(`COUNT()` > 1000) %>% # only subreddits where was more than 1000 added comments in the month of May.
  select(subreddit) %>%
  collect

subreddit_list<-subreddit_list$subreddit

#let's add some random subreddits. We do this only for memory reasons - handling more data would be very time and memory consuming.
chosen_subreddits<-sample(subreddit_list, 250)


#function for getting comments for chosen subbreddits. 
subreddit_comments<- function(subreddit){
  sub<- subreddit
  db <- src_sqlite('database.sqlite', create = F)
  db_subset <- db %>%
    tbl('May2015')
  
  db_subset <- db %>% 
    tbl('May2015') %>% 
    filter(subreddit == sub)
  df <- data.frame(db_subset)
  return(df$body)
}


# okay, we are ready to obtain comments of our chosen subreddits. 
learning_text<-c()
j<-1
for (i in chosen_subreddits){
  learning_text<-c(learning_text, subreddit_comments(i))
  print(i)
  print(j)
  j<-j+1
}

#let's save them
save(learning_text, "reddit_coments_sample.RData")

#### Obtaining Wikipedia's Good Articles ####
#I had found small dataset of few thousands wikipedia articles.
learning_text = readLines(text8_file, n = 1, warn = FALSE)

#It is around 100Mb big. We decided it is insufficient and decided that we need more articles. 

# There is nice page on the wikipedia - list of good articles. We decided to use that.

url<-read_html("https://en.wikipedia.org/wiki/Wikipedia:Good_articles/all")
links<-html_attr(html_nodes(url, "a"), "href") # obtaining list of all links listed on the page
links<-links[133:25435] 
links<-links[1:(length(links)-53)]
links<-links[grep("wiki", links)]

#the list of links is still in  format /wiki/[name_of_article]. Let's omit the start.
good_articles<-lapply(links, FUN = str_sub, start=7)

# originally, i intended to use package wikipediR. I was confused by the output of that package. Therefore, I wrote own function.

#cleaning text:
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# getting one article by name. But we have 25 thousands of them!
get_wiki_text<-function(wiki_article){
  url<-sprintf("https://en.wikipedia.org/w/api.php?action=query&prop=extracts&titles=%s&format=json", wiki_article)
  a<-url %>% 
    GET %>% 
    content
  
  b<-a$query$pages
  names(b) <- NULL
  b<- b %>% unlist(recursive = FALSE)
  result<-cleanFun(b$extract)
  return(result)
  
}


#function for obtaining text. If you run this, 
#you will see that there is quite nice time estimate of how long will the obtaining of the data take. 
#And also it will show you a plot every thousand articles!

get_learning_text<-function(sample_articles){
  
  # useful part, which actually does the work
  learning_text<-c()
  mov_avg<-c()
  j<-1
  for (i in sample_articles){
    try(
      learning_text<-c(learning_text, get_wiki_text(i))
    )
    
    
    if ((j/10)%%1==0){
      message(paste(j))
      if (j>11){
        difference<-Sys.time()-now
        estimate<-round(((length(sample_articles)-j)*difference)/600, digits = 1)
        message(sprintf("estimated time: %s minutes", estimate))
        mov_avg[length(mov_avg)+1]<-estimate
        if (length(mov_avg)>100) {
          ma<-mean(tail(mov_avg, 100))
          message(sprintf("Moving average:%s", ma))
        }
      }
      now<-Sys.time()
      
    }
    
    if ((j/1000)%%1==0){
      plot(mov_avg)
    }
    
    j<-j+1
    
  }
  return(learning_text)
}

learning_text<-get_learning_text(good_articles)




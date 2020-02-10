library("rtweet")
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)


consumer_key <- "pDcfg6NBQqVnjddH5EGCdg58V"
consumer_secret <- "dC0Qo36SxDcgLt7t6J7yo3EAV7gbdBBOkjYnFrG0ZBXdB12TN8"
access_token <- "75640650-oLJcfJ2lXU6vzchCfMgbSjws8llhdFB5fJrQvimdd"
access_token_secret <- "Nz3WtCuq8bOuvq6Bj7BIV4HJVqHFDAaetiwVXABs9TBNh"


## authenticate via web browser
token <- create_token(
  app = "tweetrmajos",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_token_secret
  )

## SEARCH KEYWORDS
ripMoi <- search_tweets(
  "#ripMoi",
  n = 18000,
  include_rts = FALSE
)

#Save the tweets
# sv <- save_as_csv(ripMoi,'ripMoi.csv')

#import dataset
ripData <- read_csv("ripMoi.csv")

#Extract needed fields
ripDataEx <- ripData %>% select('screen_name','text')

#Clean the text column by removing words that start with @ and # symbols since the like denote a person mentioned
# or a trending hashtag in this case most likely ripMoi
ripDataEx$text <- gsub("@\\w+ *", "", ripDataEx$text)
ripDataEx$text <- gsub("#\\w+ *", "", ripDataEx$text)
ripDataEx$text <- gsub("(http[^ ]*)|(www.[^ ]*)", "", ripDataEx$text)




# Tokenize words
 tokens <- ripDataEx %>% unnest_tokens(word,text)

 tokens %>%
   anti_join(stop_words) %>% 
   inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
   count(sentiment) %>% # count the # of positive & negative words
   spread(sentiment, n, fill = 0) %>% make data wide
   mutate(sentiment = positive - negative)

 
 #plot Negative vs Positive Sentiments
  NegVsPos <- ripDataEx %>% 
   unnest_tokens(word, text) %>% 
   anti_join(stop_words) %>% 
   inner_join(get_sentiments("bing")) %>% 
   group_by(sentiment) %>% 
   count(word) %>% 
   top_n(200) 
  
  
  
  
 #plot a word cloud
  NegVsPos %>% 
     acast(word~sentiment, value.var = "n", fill = 0) %>% 
     comparison.cloud(color = c("#1b2a49", "#00909e"),
                      max.words = 500)
  # Note the acast function is from the reshape2 package
  # Functions such as comparison.cloud() require you to turn the data frame into a matrix with reshape2’s acast()



  
  
tx <-ripDataEx %>% 
       unnest_tokens(word, text) %>% 
       anti_join(stop_words) %>% 
       inner_join(get_sentiments("bing")) %>% 
       group_by(sentiment,word) %>% 
       count(word) %>% 
       arrange(desc(n)) %>% 
       head(100)

    ggplot(tx,aes(x = reorder(word, n), y = n, fill = sentiment)) +
        geom_col(show.legend = TRUE) +
        ylab("") +
        xlab("") +
        coord_flip() +
        facet_wrap(~sentiment, ncol = 2, scales = "free_y")  +
        ggtitle("Negative Vs Positive Sentiments") +
        theme_minimal()




  #See how people responded to the news of the deceased
    ripData %>%
      mutate(day = date(created_at)) %>%
      count(day) %>%
      ggplot(aes(x = day, y = n,fill = day)) +
      geom_bar(stat = "identity") +
      ylab("") + xlab("") +
      ggtitle("Tweets per day")+
      theme_minimal()
  
  
    
    
    #Explore Unique words 
            ripDataEx %>%
            select(text) %>%
            unnest_tokens(words,text,token = "ngrams",n=5) %>% 
            count(words,sort = TRUE) %>%
            top_n(50) %>%
            mutate(words=reorder(words,n)) %>%
            ggplot(aes(x=words,y=n))+
            geom_col()+
            coord_flip()+
            theme_minimal()+
            xlab("Word count")+
            ylab("paird words")+
            ggtitle("Exploring words that occur together")
  
            
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



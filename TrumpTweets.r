# Download the necessary packages
install.packages("scales")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dslabs")
install.packages("tidytext")
install.packages("broom")

# Load the packages
library(tidytext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(dslabs)
library(broom)
set.seed(1)

#Get the data
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 


data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
?trump_tweets

trump_tweets %>% select(text) %>% head

#Count tweets on the basis of the source
colnames(trump_tweets)
trump_tweets %>% count(source) %>% arrange(desc(n))

#Remove some text from the source
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

#Assign trump tweets to a new var and use filter for gettind specific date range data
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-05-17") & 
           created_at < ymd("2016-12-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

head(campaign_tweets)

#Plot the graph

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "GMT"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (IST)",
       y = "% of tweets",
       color = "")

#Extract data using a tweet number 
x <- 3080
campaign_tweets$text[x]
campaign_tweets[x,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#Analyse the pattern
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[x,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Remov the links
campaign_tweets[x,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Count, how many times a word has been used.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

#Get top 10 word used and also remove unnessary data
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#Frequent words used via different devices and also get hashtags
device <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(other = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
  device %>% arrange(desc(other))
  device %>% arrange(other)

  device %>% filter(Android+iPhone > 100) %>%
    arrange(desc(other))
  device %>% filter(Android+iPhone > 100) %>%
    arrange(other)
  

  
  #Analyse the sentiment of the tweets
  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    select(word, sentiment)

  tweet_words %>% inner_join(nrc, by = "word") %>% 
    select(source, word, sentiment) %>% sample_n(10)  

  sentiment_counts <- tweet_words %>%
    left_join(nrc, by = "word") %>%
    count(source, sentiment) %>%
    spread(source, n) %>%
    mutate(sentiment = replace_na(sentiment, replace = "none"))
  sentiment_counts  

  tweet_words %>% group_by(source) %>% summarize(n = n())    

  sentiment_counts %>%
    mutate(Android = Android / (sum(Android) - Android) , 
           iPhone = iPhone / (sum(iPhone) - iPhone), 
           or = Android/iPhone) %>%
    arrange(desc(or))
  
  #Different sentiments in different devices with used rate
  log_or <- sentiment_counts %>%
    mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
            se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
            conf.low = log_or - qnorm(0.975)*se,
            conf.high = log_or + qnorm(0.975)*se) %>%
    arrange(desc(log_or))
  
  log_or
  
  #Plot the ScatterPlot with errorbar
  log_or %>%
    mutate(sentiment = reorder(sentiment, log_or),) %>%
    ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar() +
    geom_point(aes(sentiment, log_or)) +
    ylab("Log odds ratio for association between Android and sentiment") +
    coord_flip() 
  
  #Frequently used words in different devices with sentiment behind it
  device %>% inner_join(nrc) %>%
    filter(sentiment == "disgust" & Android + iPhone > 10) %>%
    arrange(desc(other))
  

  #Plot the data for each sentiment with the words used  
  device %>% inner_join(nrc, by = "word") %>%
    mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
    mutate(log_or = log(other)) %>%
    filter(Android + iPhone > 10 & abs(log_or)>1) %>%
    mutate(word = reorder(word, log_or)) %>%
    ggplot(aes(word, log_or, fill = log_or < 0)) +
    facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
    geom_bar(stat="identity", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
#KDD Summer 1 2016 Final Exam  - Social Minning with R - using Twitter feeds
#By Pooja Sharma 
#Student ID#: 800935487
#My Portfolio Link - https://poojaxme.wordpress.com/

#Q.Social mining with R.
 
#Reference Work :
#I took help from the following two blogs for my work - 
#1.R code help from https://cognitivedatascientist.com/2016/03/16/twitter-sentiment-analysis-in-r-14/#toc_0
#The above link mines the twitter feeds for Presdential Candidates in US 2016 elections.
#2.For better domain understanding and application, read the post at http://ukandeu.ac.uk/how-remain-and-leave-camps-use-hashtags/
#3.Generated a doughnut chart referring to the example at http://www.r-bloggers.com/doughnut-chart-in-r-with-googlevis/

#Summary:
#Analyzed the Twitter feeds of the Twitter users in London about how they
#tweeted on the EU referendum on UK membership.

#There are three parts in this code: 
#Part1  : Setting up the Twitter API secure tokens and access keys.
#Part2  : Extracted the tweets where location was equal to the geocode for London city
#         Hashtags I used - 'brexit','strongerin','leave','bremain','euref'
#Part3  : Generated a word cloud based on extracted tweets.
#Part4  : Generated a dough nut chart by segregating the tweets based on proexit and proremain UK supporters.

#########
#Part1  #
#########

rm(list=ls()) 
#Twitter API Setup  
#install.packages("twitteR")
#install.packages("ROAuth")

require(twitteR)    #provides an interface to the twitter web api.
require(ROAuth)

#Created a Twitter App "EUVoteMiner2016" at https://apps.twitter.com/ to generate the below credentials.
consumerKey    <- "HVinXlXRd8S7oBeOUtuyLnv6k"
consumerSecret <- "fIRFvRcMdx5oqbDmsJgsCiM6nHBUgOvGjysITzT4Y3afUMl2ZG"
accessToken    <- "2173824948-LoIZG3fKF4QEBU7lrEgx0rurglALqVBDVE9Gj2w"
accessSecret   <- "nRxUDUSHOfpg9hmMjDhcR55HvaZh9pzFkWENFkkPDpPB2"

# This function wraps the OAuth authentication handshake functions
# from the httr package for a twitteR session
setup_twitter_oauth(
  consumer_key = consumerKey,
  consumer_secret = consumerSecret, 
  access_token = accessToken, 
  access_secret = accessSecret)

library(dplyr)

#########
#Part2  #    Extracting Tweets
#########

#tweet2df function takes in 3 parameters - hashtag and dates.
#searchTwitter function searches Twitter based on the hashtag, geo location code and time parameters.
#strip_retweets function removes any retweets so that they do not skem our analysis.

tweet2df = function(topic, N, since=NULL, until=NULL) {
  t <- searchTwitter(topic, n=N, since=since, until=until, 
                     geocode='51.5,-127,2400km',           #geocode for London 
                     retryOnRateLimit=10, lang="en")
  # retain 'original' tweets only
  t <- strip_retweets(t, strip_manual = TRUE, strip_mt = TRUE)
  # convert to data frame
  df <- twListToDF(t)
  df$longitude <- as.numeric(df$longitude)
  df$latitude <- as.numeric(df$latitude)
  return(cbind(topic, df))
}

# search parameters 
days_ago <- 5:1
N <- 200

#Specifying Hashtags to be used for tweets.
topic <- c('brexit','strongerin','leave','bremain','euref')

# create data frame containing all search parameters
since <- format(Sys.Date()-days_ago)

#The expand.grid function below creates a dataframe for all possible combinations 
#of dates and hashtags(topic) which is later passed to the tweet2df function via a loop.
parms <- data.frame(expand.grid(since=since, N=N, topic=topic))
parms$until <- format(Sys.Date()-(days_ago-1))
i <- sapply(parms, is.factor)
parms[i] <- lapply(parms[i], as.character)

parms <- parms[parms$until != '2016-06-24',]
parms

#Extracts all timed tweets based on the dates specified in the parms data frame.
timedTweets <- bind_rows(lapply(1:nrow(parms), 
                                function(x) tweet2df(parms$topic[x], N=N, 
                                                     since=parms$since[x], 
                                                     until=parms$until[x]))) %>% 
  as.data.frame()

#Extracts all untimed Tweets, the dates passed are NULL.
untimedTweets <- bind_rows(lapply(1:length(topic), 
                                  function(x) tweet2df(topic[x], N=N))) %>% 
  as.data.frame()

# Combine into single data frame 
EUTweets <- rbind(timedTweets, untimedTweets)

#Store the tweets in a csv file - useful for future reference purposes.
write.csv(EUTweets, file = "EUTweets_all.csv",row.names=FALSE)

library(stringr)
#Spilt timestamp column to extract day field separated by space in the original data.
EUTweets$day <- str_split_fixed(EUTweets$created," ",2)[,1]


#########
#Part3  #    Generating Wordclouds from tweets data
#########


library(tm)

#Preparing data to generate word cloud
# create a vector source, which interprets each element of its argument as a document
v <- VectorSource(EUTweets$text)

# create an object of class 'corpus'; a collection of documents containing NL text
docs <- Corpus(v)

# convert corpus to plain text
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
docs <- tm_map(docs, content_transformer(function(x) tolower(x)))

docs <- tm_map(docs, removeWords, stopwords('en'))

# remove URLs
stripURL = function(x) {
  gsub("www[^[:space:]]+|htt[^[:space:]]+", "", x)
}
docs <- tm_map(docs, content_transformer(stripURL))


docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

EUTweets$tweet <- as.character(unlist(sapply(docs, `[`, "content")))
EUTweets$text <- NULL

# remove rows with duplicate tweets
EUTweets <- EUTweets %>% 
  group_by(topic) %>% 
  distinct(., tweet)

# function to remove topic from all tweets about that topic
removeTopic = function(topic, tweets) {
  words <- unlist(str_split(tolower(topic), boundary("word")))
  pattern <- paste(words,sep="",collapse = "|")
  out <- gsub(pattern, '', tweets)
  return(out)
}

# call function rowwise
EUTweets <- EUTweets %>% rowwise() %>%   mutate(tweet = removeTopic(topic, tweet)) %>%
  as.data.frame()

library(wordcloud)
col=brewer.pal(8, 'Set1')

topics <- unique(EUTweets$topic)

lapply(1:length(topics), function(x) {
  print(topics[x])
  dat2cloud <- subset(EUTweets, topic==topics[x])
  text2cloud <- dat2cloud$tweet
  corp <- Corpus(VectorSource(text2cloud))
  print(wordcloud(corp, max.words=75, random.color=F, 
                  random.order=F, colors=col))
}
)

#########
#Part4  #    Segrgating the tweets into proremain UK and proexit UK based on hashtags used 
#########    And generating a dough chart to assess sentiment of both supporters.

#Created a new support coulmn to indicate whether the tweet was proremain or proexit based on the hashtag used 

library(data.table)
EUTweets <- data.table(EUTweets)

EUTweets$Support[EUTweets$topic %like% 'strongerin']  <- 'pro remain'
EUTweets$Support[EUTweets$topic %like% 'bremain']     <- 'pro remain'

EUTweets$Support[EUTweets$topic %like% 'brexit']      <- 'pro exit'
EUTweets$Support[EUTweets$topic %like% 'leave']       <- 'pro exit'


#Generating a donut chart comparing the "Remain" and "Exit" supporters of EU in London based on twitter hashtags

#Plotting a donut chart 
dat <- data.frame(as.data.frame(table(EUTweets$Support)))
#install.packages("googleVis")
library(googleVis)
## Doughnut chart - a pie with a hole
doughnut <- gvisPieChart(dat, 
                         options=list(
                           width=500,
                           height=500,
                           slices="{0: {offset: 0.2},
                           1: {offset: 0.2},
                           2: {offset: 0.2}}",
                           title='EU Referendum 2016 - From Twitter Hashtags
                           (Support: Remain in EU/Leave EU)',
                           legend='bottom',
                           colors="['black','red']",
                           pieSliceText='label',
                           pieHole=0.5),
                         chartid="doughnut")
plot(doughnut) #The chart is created in html page.Please check browser window for the generated chart.

#The generated chart is also attached in the folder - doughnut.html - for a quick look.

#End


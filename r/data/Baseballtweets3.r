#############################################
# Chunk - 1 - Authenticate with twitter API
#############################################

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)

## Windows users need to get this file
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "VV0MDn7sTnXYy5HxTi8yltba8"
consumerSecret = "5FK2DBsqa4qgMVt5kV9YViN9lbEDs6LOWsWC4xfdvmBvSsc2Ji"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
6501811
save(Cred, file="twitter authentication.Rdata")
registerTwitterOAuth(Cred)

## Future use

load("twitter authentication.Rdata")
registerTwitterOAuth(Cred)

############################################################
# Chunk  - 2 - Twitter Scrape  #Rangers #Athletics #MLB  
############################################################

#Rangers.list <- searchTwitter('#Rangers', n=500, cainfo="cacert.pem")  
#Rangers.df = twListToDF(Rangers.list)  
#write.csv(Rangers.df, file='Z:/Clients/UCD/TURAS/App Development/#2 Geospatial Dashboard App/data/RangersTweets.csv', row.names=F)

FACTEST.list <- searchTwitter('#FACTEST', n=1000, cainfo="cacert.pem")
#FACTEST.list <- searchTwitter('#NTFM', n=100, geocode='53.3385,-6.2488,100km', cainfo="cacert.pem")
FACTEST.df = twListToDF(FACTEST.list)  
write.csv(FACTEST.df, file='Z:/Clients/UCD/TURAS/App Development/#2 Geospatial Dashboard App/data/FACTESTTweets.csv', row.names=F)

#MLB.list <- searchTwitter('#MLB', n=1000, cainfo="cacert.pem")  
#MLB.df = twListToDF(MLB.list)  
#write.csv(MLB.df, file='Z:\Clients\UCD\TURAS\App Development\#2 Geospatial Dashboard App\data\MLBTweets.csv', row.names=F)


###############################
#Chunk -3- Sentiment Function     
###############################

library (plyr)
library (stringr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)       
  
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub('[[:punct:]]', '', sentence)  
    
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    
    sentence = gsub('\\d+', '', sentence)  
    
    # and convert to lower case:  
    
    sentence = tolower(sentence)  
    
    # split into words. str_split is in the stringr package  
    
    word.list = str_split(sentence, '\\s+')  
    
    # sometimes a list() is one level of hierarchy too much  
    
    words = unlist(word.list)  
    
    # compare our words to the dictionaries of positive & negative terms  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:  
    
    pos.matches = !is.na(pos.matches)  
    
    neg.matches = !is.na(neg.matches)  
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 


############################################
#Chunk - 4 - Scoring Tweets & Adding a column      
############################################

#Load sentiment word lists
hu.liu.pos = scan('C:/temp/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/temp/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical')

#Import 3 csv
DatasetRangers <- read.csv("C:/temp/RangersTweets.csv")
DatasetRangers$text<-as.factor(DatasetRangers$text)

DatasetAthletics <- read.csv("C:/temp/AthleticsTweets.csv")
DatasetAthletics$text<-as.factor(DatasetAthletics$text)

DatasetMLB <- read.csv("C:/temp/MLBTweets.csv")
DatasetMLB$text<-as.factor(DatasetMLB$text)
 

#Score all tweets 
Rangers.scores = score.sentiment(DatasetRangers$text, pos.words,neg.words, .progress='text')
Athletics.scores = score.sentiment(DatasetAthletics$text, pos.words,neg.words, .progress='text')
MLB.scores = score.sentiment(DatasetMLB$text, pos.words,neg.words, .progress='text')

path<-"C:/temp/"
write.csv(Rangers.scores,file=paste(path,"RangersScores.csv",sep=""),row.names=TRUE)
write.csv(Athletics.scores,file=paste(path," AthleticsScores.csv",sep=""),row.names=TRUE)
write.csv(MLB.scores,file=paste(path,"MLBScores.csv",sep=""),row.names=TRUE)

Rangers.scores$Team = 'Rangers'
Athletics.scores$Team = 'Athletics'
MLB.scores$Team = 'MLB'

############################# 
#Chunk -5- Visualizing   	    
#############################

hist(Rangers.scores$score)
qplot(Rangers.scores$score)

hist(Athletics.scores$score)
qplot(Athletics.scores$score)

hist(MLB.scores$score)
qplot(MLB.scores$score)
 
#################################
#Chunk -6- Comparing 3 data sets	              
#################################

all.scores = rbind(Rangers.scores, Athletics.scores, MLB.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=Team), binwidth=1) +
  facet_grid(Team~.) + # make a separate plot for each hashtag
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

####################################################
#Chunk -7- Classification by emotions and polarity                
####################################################

library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Get the text
Rangers_txt = sapply(Rangers.list, function(x) x$getText())

# Prepare text for the analysis
Rangers_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Rangers_txt)
Rangers_txt = gsub("@\\w+", "", Rangers_txt)
Rangers_txt = gsub("[[:punct:]]", "", Rangers_txt)
Rangers_txt = gsub("[[:digit:]]", "", Rangers_txt)
Rangers_txt = gsub("http\\w+", "", Rangers_txt)
Rangers_txt = gsub("[ \t]{2,}", "", Rangers_txt)
Rangers_txt = gsub("^\\s+|\\s+$", "", Rangers_txt)

try.error = function(x)
  
{  
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
Rangers_txt = sapply(Rangers_txt, try.error)

# remove NAs in Rangers_txt
Rangers_txt = Rangers_txt[!is.na(Rangers_txt)]
names(Rangers_txt) = NULL

#classify emotion
class_emo = classify_emotion(Rangers_txt, algorithm="bayes", prior=1.0)
#get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(Rangers_txt, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=Rangers_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", 
       title = "Sentiment Analysis of Tweets about Rangers\n(classification by emotion)",
       plot.title = element_text(size=12))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets",
       title = "Sentiment Analysis of Tweets about Rangers\n(classification by polarity)",
       plot.title = element_text(size=12))
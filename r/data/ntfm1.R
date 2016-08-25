# Load libraries
library(twitteR)
library(RCurl) 
library(ROAuth)

# SSL Certificate
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# API URLs
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"

# API Keys from https://apps.twitter.com/app/new 
apiKey <- "VV0MDn7sTnXYy5HxTi8yltba8"
apiSecret <- "5FK2DBsqa4qgMVt5kV9YViN9lbEDs6LOWsWC4xfdvmBvSsc2Ji"

# Connect to Twitter to get credentials

twitCred <- OAuthFactory$new(
  consumerKey=apiKey,
  consumerSecret=apiSecret,
  requestURL=reqURL,
  accessURL=accessURL,
  authURL=authURL)

# Twitter Handshake - you will need to get the PIN after this
twitCred$handshake()
3601929
# Optionally save credentials for later
registerTwitterOAuth(twitCred)
save(list="twitCred", file="credentials")


# Set up the query
query <- "#factest, #ntfm"
query <- unlist(strsplit(query,","))
tweets = list()

# Loop through the keywords and store results

for(i in 1:length(query)){
  result<-searchTwitter(query[i],n=1500,geocode='53.334848,-6.251999,100km')
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}

# Create a placeholder for the file
file<-NULL

# Check if tweets.csv exists
if (file.exists("tweets.csv")){file<- read.csv("tweets.csv")}

# Merge the data in the file with our new tweets
df <- do.call("rbind", lapply(tweets, as.data.frame))
df<-rbind(df,file)

# Remove duplicates
df <- df[!duplicated(df[c("id")]),]

# Save
write.csv(df,file="Z:/Clients/UCD/TURAS/App Development/#2 Geospatial Dashboard App/data/tweetsFACTEST.csv",row.names=FALSE)

# Done!


###################################################
# Chunk - 1 - Authenticate with Twitter Search API
###################################################

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
#library(ggplot2)

## Windows users need to get this file
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

## JUMP TO FUTURE USE

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
8019887
save(Cred, file="twitter authentication.Rdata")
registerTwitterOAuth(Cred)

## Future use

load("twitter authentication.Rdata")
registerTwitterOAuth(Cred)

#########################################
# Chunk  - 2 - Query Twitter Search API 
#########################################

FACTEST.list <- searchTwitter('#TURASgi', n=100, cainfo="cacert.pem")
#FACTEST.list <- searchTwitter('#NTFM', n=100, geocode='53.3385,-6.2488,100km', cainfo="cacert.pem")
FACTEST.df = twListToDF(FACTEST.list)  
write.csv(FACTEST.df, file='Z:/Clients/35 - Turas UCD/TURAS/App Development/2 London Geospatial Dashboard App/r/data/TURASgi_Rscrape.csv', row.names=F)


#########################################
#########################################



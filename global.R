# for twitter API
#library(devtools)
#install_github("geoffjentry/twitteR")
library(httr)
library(httpuv)
library(twitteR)
# for word cloud
library(SnowballC)
library(tm)
library(wordcloud)
library(memoise)
# for data parsing
library(stringr)
library(plyr)


# load custom functions
source("R/cleanTweet.R")
source("R/sentiment_score.R")

# load opinion lexicon for sentiment analysis 
# from Hu & Liu [KDD-2004](http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
hu_liu_pos <- scan("data/opinion-lexicon-English/positive-words.txt", skip = 35, what = "character")
hu_liu_neg <- scan("data/opinion-lexicon-English/negative-words.txt", skip = 35, what = "character")
# add industry-specific terms...
pos_words <- c(hu_liu_pos, "highlife")
neg_words <- c(hu_liu_neg, "dammit", "recall", "safety", "crash", "accident", "malfunction")


# authorize twitter
consumer_key <- readLines("consumer_key.key")
consumer_secret <- readLines("consumer_secret.key")
access_token <- readLines("access_token.key")
access_secret <- readLines("access_secret.key")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# The list of valid car brands
brands <<- list("Acura" = "acura",
               "Aston Martin" = "astonmartin",
               "Audi" = "audi",
               "Bentley" = "bentley",
               "BMW" = "bmw",
               "Buick" = "buick",
               "Cadillac" = "cadillac",
               "Chevrolet" = "chevrolet",
               "Chrysler" = "chrysler",
               "Dodge" = "dodge",
               "Ferrari" = "ferrari",
               "Fiat" = "fiat",
               "Ford" = "ford",
               "GMC" = "GMC",
               "Honda" = "honda",
               "Hummer" = "hummer",
               "Hyundai" = "hyundai",
               "Infiniti" = "infiniti",
               "Isuzu" = "isuzu",
               "Jaguar" = "jaguar",
               "Jeep" = "jeep",
               "Kia" = "kia",
               "Lamborghini" = "lamborghini",
               "Land Rover" = "landrover",
               "Lexus" = "lexus",
               "Lincoln" = "lincolnmotorco",
               "Lotus" = "lotus",
               "Maserati" = "maserati",
               "Mazda" = "mazda",
               "McLaren" = "mclaren",
               "Mercedes-Benz" = "mercedesbenz",
               "Mini" = "mini",
               "Mitsubishi" = "mitsubishi",
               "Nissan" = "nissan",
               "Porsche" = "porsche",
               "Ram" = "ram",
               "Rolls-Royce" = "rollsroyce",
               "Saab" = "saab",
               "Saturn" = "saturn",
               "Scion" = "scion",
               "Subaru" = "subaru",
               "Suzuki" = "suzuki",
               "Tesla" = "tesla",
               "Toyota" = "toyota",
               "Volkswagen" = "volkswagen",
               "Volvo" = "volvo")


# Using "memoise" to get tweets and automatically cache the results
getTweets <- memoise(function(brand, n = 500) {
  library(twitteR)
  
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(brand %in% brands)) stop("Unknown brand")
  
  print(brand)
  TS <- paste0("@", brand, " OR ", "#", brand)
  # get tweets
  tweets <- suppressWarnings(searchTwitter(TS, n = n, since = format(Sys.Date()-7), lang="en"))
  # if search returns tweets, strip retweets and clean text
  if(length(tweets)>0) {
    tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
    # convert to data.frame
    tweetsdf <- twListToDF(tweets)
    # add brand and return
    tweetsdf <- data.frame(cbind(brand, tweetsdf))
    # ddply to clean tweet text
    tweetsdf <- ddply(tweetsdf, .(id), mutate, text_clean = cleanTweet(text, leaveout = brand))
    # drop 'text' because invalid characters cause problems with printing and such
    tweetsdf <- subset(tweetsdf, , - text)
  } else { # else return empty dataframe in same structure for use with plyr functions
    tweetsdf <- structure(list(brand = structure(integer(0), .Label = c(brand), class = "factor"),
                          #text = character(0), 
                          favorited = logical(0), 
                          favoriteCount = numeric(0), 
                          replyToSN = character(0), 
                          created = structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                          truncated = logical(0), replyToSID = character(0), 
                          id = character(0), 
                          replyToUID = character(0), 
                          statusSource = character(0), 
                          screenName = character(0), 
                          retweetCount = numeric(0), 
                          isRetweet = logical(0), 
                          retweeted = logical(0), 
                          longitude = character(0), 
                          latitude = character(0)), 
                          text_clean = character(0),
                     .Names = c("brand", "text", "favorited", "favoriteCount", "replyToSN", "created",  "truncated", "replyToSID", "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted", "longitude","latitude"), row.names = integer(0), class = "data.frame")
   }
  return(tweetsdf)
})

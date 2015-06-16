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


# authorize twitter
consumer_key <- readLines("consumer_key.key")
consumer_secret <- readLines("consumer_secret.key")
access_token <- readLines("access_token.key")
access_secret <- readLines("access_secret.key")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# The list of valid car makes
makes <<- list("Acura" = "acura",
               "Aston Martin" = "astonmartin",
               "Audi" = "audi",
               "Bentley" = "bentley",
               "BMW" = "bmw",
               "Buick" = "buick",
               "Cadillac" = "cadillac",
               "Chevrolet" = "chevrolet",
               "Chrylser" = "chrsyler",
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


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(make) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(make %in% makes))
    stop("Unknown make")
  
  # get data from Twitter
  tweetsearch <- paste0("@", make, " OR ", "#", make)
  tweets <- searchTwitter(tweetsearch, n=500, since = format(Sys.Date()-7), lang="en")
  
  # strip retweets
  tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
  
  # convert to data.frame
  tweetdf <- twListToDF(tweets)

  # clean tweet text
  tweetdf$text2 <- NA
  
  for(i in 1:nrow(tweetdf)) {
    thistweet <- unlist(str_split(tweetdf[i, 'text'], pattern = " "))
    # remove all non-alphanumeric characters
    thistweet <- str_replace_all(thistweet, "[^[:alnum:]]", " ")
    # lowercase and remove make as it swamps other words
    thistweet <- tolower(thistweet)
    thistweet <- thistweet[!grepl(make, thistweet)]
    # remove links
    thistweet <- thistweet[!grepl("http", thistweet)]

    tweetdf[i, "text2"] <- paste(thistweet, collapse = " ")
  }
  
  # turn into a text mining Corpus
  tweetCorpus <- Corpus(VectorSource(tweetdf$text2))
  tweetCorpus = tm_map(tweetCorpus, removePunctuation)
  tweetCorpus = tm_map(tweetCorpus, removeNumbers)
  tweetCorpus = tm_map(tweetCorpus, removeWords, stopwords("SMART"))
  
  myDTM <- TermDocumentMatrix(tweetCorpus,
                              control = list(minWordLength = 1))
  
  m <- as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})


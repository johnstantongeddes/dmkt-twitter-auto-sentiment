#' Get and clean tweets for a specific brand using the Twitter API
#' 
#' @param brand Brand or any other string to search Twitter for tweets.
#' @param n Number of tweets to return. Default 1000
#' 
#' @return out Dataframe with information on returned tweets
#' 
#' @examples 
#' getTweets("subaru", n=100)
#' @export 


getTweets <- function(brand, n = 1000) {
  library(twitteR)
  
  TS <- paste0("@", brand, " OR ", "#", brand)
  # get tweets
  tweets <- searchTwitter(TS, n = n, since = format(Sys.Date()-7), lang="en")
  # strip retweets
  if(length(tweets)>0) {
    tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
    # convert to data.frame
    tweetdf <- twListToDF(tweets)
    # add brand and return
    out <- cbind(brand, tweetdf)
  } else {
    out <- structure(list(brand = structure(integer(0), .Label = c(brand), class = "factor"),
                          text = character(0), 
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
                     .Names = c("brand", "text", "favorited", "favoriteCount", "replyToSN", "created",  "truncated", "replyToSID", "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted", "longitude","latitude"), row.names = integer(0), class = "data.frame")
  }
  
  return(out)
}
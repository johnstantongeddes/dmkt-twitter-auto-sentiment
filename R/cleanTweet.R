#' Make a word cloud from a list of tweets
#' 
#' @param tweets Vector of text of cleaned tweets from Twitter API
#' 
#' @return clean Cleaned text of tweet
#' 
#' @examples 
#' cleanTweet(tweet = "a %&messy #example @somebody Tweet with A Link http://somewhere", leaveout = c("a", "with"))
#' @export 
#' 
cleanTweet <- function(tweet, leaveout = "ignorethisstring") {
  library(stringr)
  
  cleaned_tweet <- unlist(str_split(tweet, pattern = " "))
  # remove all non-alphanumeric characters
  cleaned_tweet <- str_replace_all(cleaned_tweet, "[^[:alnum:]]", " ")
  # convert to lowercase 
  cleaned_tweet <- tolower(cleaned_tweet)
  # remove 'leaveout' terms
  cleaned_tweet <- cleaned_tweet[!grepl(paste(leaveout, collapse = '|'), cleaned_tweet)]
  # remove links
  cleaned_tweet <- cleaned_tweet[!grepl("http", cleaned_tweet)]
  # remove 'amp' which keeps showing up
  cleaned_tweet <- cleaned_tweet[!grepl("amp", cleaned_tweet)]
  # recombine and return
  paste(cleaned_tweet, collapse = " ")
}

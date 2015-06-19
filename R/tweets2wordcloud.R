#' Make a word cloud from a list of tweets
#' 
#' @param tweets Vector of text of cleaned tweets from Twitter API
#' 
#' @examples 
#' tweets2wordcloud(tweets)
#' @export 

tweets2wordcloud <- function(tweets) {
  library(tm)
  library(wordcloud)
  
  # check that tweets are cleaned
  if(TRUE %in% grepl("[[:punct:]]", tweets)) stop("Need to clean tweets first using cleanTweet()")
  if(TRUE %in% grepl("http", tweets)) stop("Need to clean tweets first using cleanTweet()")
  
  # turn tweets into a Corpus
  tweetCorpus <- Corpus(VectorSource(tweets))
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, tolower)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("SMART"))
  
  myDTM <- TermDocumentMatrix(tweetCorpus,
                              control = list(minWordLength = 2))
  
  m <- as.matrix(myDTM)
  m <- sort(rowSums(m), decreasing=TRUE)
  
  wordcloud(names(m), m, scale = c(3, 0.5), 
            min.freq = 2, max.words = 50,
            colors=brewer.pal(8, "RdYlBu"))
  
}
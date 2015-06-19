#' Score positive/negative sentiment of string of text 
#' 
#' @param text String of text to score sentiment
#' @param pos_words Vector of positive words to score
#' @param neg_words Vector of negative words to score
#' 
#' @return score Numeric sentiment score
#' 
#' @examples 
#' sentiment_score(text = "happy happy happy sad", pos_words = c("happy"), neg_words = "sad")
#' @export

# sentiment_score function
sentiment_score <- function(text, pos_words, neg_words) {
  library(stringr)
  # split text into words
  word_list <- str_split(text, '\\s+')
  # unlist
  words <- unlist(word_list)
  
  # compare rods to the dictionaries of positive and negative terms
  # return only those that are TRUE/FALSE
  pos_matches <- !is.na(match(words, pos_words))
  neg_matches <- !is.na(match(words, neg_words))
  
  # score. TRUE/FAlSE treated as 1/0 by sum()
  score <- sum(pos_matches) - sum(neg_matches)
  return(score)
}
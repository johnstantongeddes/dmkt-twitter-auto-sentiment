# Social messages posted to Twitter in last 7 days for each car brand using their hashtag
# e.g. #acura OR @acura 

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  getTwitterData <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing tweets...")
        getTweets(brand = input$selection, n=500)
      })
    })
  })
  
  getTermMatrix <- reactive({
    # get tweets
    tweetsdf <- getTwitterData()
    
    # turn tweets into a Corpus
    tweetCorpus <- Corpus(VectorSource(tweetsdf$text_clean))
    tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
    tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
    tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("SMART"))
    
    myDTM <- TermDocumentMatrix(tweetCorpus,
                                control = list(minWordLength = 1))
    
    m <- as.matrix(myDTM)
    m <- sort(rowSums(m), decreasing=TRUE)
    # drop top term which is (almost?) always the brand because the cleanTweet(leaveout= as.name(brand)) function isn't working...
    #m <- m[-1]
  })
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- getTermMatrix()
    wordcloud_rep(names(v), v, scale=c(3,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  # Sentiment score 
  output$sentiment_score <- renderUI({
    # get tweets
    tweetsdf <- getTwitterData()
    
    # calculate sentiment score for each tweet
    # identify very positive (sscore > 1) and negative (sscore < -1) tweets
    tweetsdf <- ddply(tweetsdf, .(id), mutate, 
                             sscore = sentiment_score(text_clean, pos_words = pos_words, neg_words = neg_words),
                             very_pos = as.numeric(sscore > 1),
                             very_neg = as.numeric(sscore < -1))
    
    # overall sentiment score by brand as ratio of very positive to very negative tweets
    pos_count <- sum(tweetsdf$very_pos)
    all_count <- sum(tweetsdf$very_pos) + sum(tweetsdf$very_neg)

    overall_sentiment_score <- round(100 * pos_count/all_count)
    
    # paste text to render in UI
    HTML(paste("Twitter Sentiment Score for the past week: ", overall_sentiment_score, '% positive', sep = ''))
    
  }) # output$sentiment_score
}


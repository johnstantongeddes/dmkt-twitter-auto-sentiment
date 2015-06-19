fluidPage(
  # Application title
  titlePanel("Twitter Car Brand Sentiment"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      p("Score sentiment and visualize the words in tweets that # or @ the selected car make in the past week. 
        The size of the word is proportional to the number of times it has been used. Enjoy!"),
      selectInput("selection", "Choose a car brand:",
                  choices = brands,
                  selected = "acura"),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum times word used:",
                  min = 1,  max = 20, value = 3),
      sliderInput("max",
                  "Maximum number of words:",
                  min = 1,  max = 100,  value = 50)
    ),
    
    # Show Word Cloud
    mainPanel(
      htmlOutput("sentiment_score"),
      br(),
      plotOutput("plot"),
      br(),
      includeMarkdown("acknowledgements.md")
    )
  )
)


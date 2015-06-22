fluidPage(
  # Application title
  titlePanel("Twitter Automotive Sentiment"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      p("Choose a car brand to get the sentiment score of tweets that # or @ the selected
brand in the past week, and visualize common words from those tweets. Enjoy!"),
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


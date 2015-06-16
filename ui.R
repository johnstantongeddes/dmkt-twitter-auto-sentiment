fluidPage(
  # Application title
  titlePanel("Twitter Auto Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      p("Visualize the words in tweets that # or @ the selected car make in the past week. 
        The size of the word is proportional to the number of times it has been used. Enjoy!"),
      selectInput("selection", "Choose a car make:",
                  choices = makes,
                  selected = "acura"),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum times word used:",
                  min = 1,  max = 20, value = 5),
      sliderInput("max",
                  "Maximum number of words:",
                  min = 1,  max = 100,  value = 50)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot"),
      includeMarkdown("acknowledgements.md")
    )
  )
)


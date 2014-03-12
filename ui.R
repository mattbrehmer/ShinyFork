#ShinyForks P4K review explorer
#by Matt Brehmer
#Updated Nov 10, 2013, moved to github repo Mar 12 2014
#ui.r

shinyUI(pageWithSidebar(
  
  headerPanel("ShinyFork"),
  
  sidebarPanel(
    
    helpText(
      p("Explore 14+ years of album reviews from ",a("Pitchfork.com", href="http://www.pitchfork.com/reviews/albums/", target="_blank"),".")
    ),
    
    wellPanel(
      
      uiOutput("year_range_slider"),
      uiOutput("score_range_slider"),
      checkboxInput("includeBNM", "Show 'Best New Music'",TRUE),
      checkboxInput("includeBNR", "Show 'Best New Reissues'",TRUE),
      checkboxInput("includeNA", "Show all other releases",TRUE),
      uiOutput("artist_search"),
      uiOutput("album_search"),
      uiOutput("label_search"),
      uiOutput("label_picker"),
      uiOutput("reviewer_picker")
    ),  
    downloadButton('downloadData', 'Download Data'),
    
    br(),
    
    br(),
    
    p("By ",a("Matt Brehmer", href="http://cs.ubc.ca/~brehmer", target="_blank")
    ),    
    p("Written in R using ",a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank")," and ",a("ggplot2", href="http://ggplot2.org/", target="_blank"),". See the ",a("gist source", href="https://gist.github.com/mattbrehmer/5645155", target="_blank"),"."
    ),
    
    p("Data collected using ",a("ScraperWiki",href="https://classic.scraperwiki.com/scrapers/pitchfork_review_data_1/", target="_blank"),"."
    ),    
    p("Twitter: ",
      a("@mattbrehmer", href="http://twitter.com/mattbrehmer", target="_blank")
    ),
    p("Contact: ",
      a( "mattbrehmer@gmail.com", href="mailto:mattbrehmer@gmail.com")
    )
    
  ),
  
  mainPanel(
    
    htmlOutput("notes"),
    
    h4("Distribution of Scores"),
    plotOutput("histPlot"),
    
    htmlOutput("medianNote"),
    
    h4("Timeline"),
    plotOutput("linePlot"),
    htmlOutput("trendNote"),
    
    h4("Summary"),
    verbatimTextOutput("summary"),
    
    h4("Reviews"),   
    wellPanel(
      numericInput("obs", "Number of reviews shown:", 25),
      selectInput("tableSortBy", "Sort table by:",
                  c("Artist" = "artist",
                    "Title" = "album",
                    "Label" = "label",
                    "Year" = "release_year",
                    "Reviewer" = "reviewer",
                    "Score" = "score",
                    "Accolade" = "accolade",
                    "Date" = "publish_date"), 
                  selected = "Date"),
      checkboxInput("isDecreasing", "Sort decreasing?",TRUE)
    ),
    tableOutput("table")
  )
))

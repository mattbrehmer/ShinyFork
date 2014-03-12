#ShinyForks P4K review explorer
#by Matt Brehmer
#Updated Nov 10, 2013, moved to github repo Mar 12 2014
#server.r

shinyServer(function(input, output) {
  
  Data <- reactive( {
    
    # subset the reviews to match user input
    reviews_sub <- reviews[in_range(reviews$release_year, input$year_range) &
                             in_range(reviews$score, input$score_range) &
                             name_match(reviews$artist, input$artist) &
                             name_match(reviews$album, input$album) &
                             name_match(reviews$label, input$labelSearch) &
                             label_match(reviews$label, input$label) &                             
                             reviewer_match(reviews$reviewer, input$reviewer) &
                             bnm_check(reviews$accolade, input$includeBNM) &
                             bnr_check(reviews$accolade, input$includeBNR) &
                             no_acc_check(reviews$accolade, input$includeNA), ]
    
    return(reviews_sub)
  })
  
  #specify the year range slider
  output$year_range_slider <- renderUI({
    
    sliderInput(inputId = "year_range",
                label = paste("Release year:"),
                min = min(reviews$release_year)-1, max = max(reviews$release_year)+1, 
                value = c(min(reviews$release_year), max(reviews$release_year)), 
                format="####")
  })
  
  #specify the score range slider
  output$score_range_slider <- renderUI({
    
    sliderInput(inputId = "score_range",
                label = paste("Score:"),
                min = min(reviews$score), max = max(reviews$score), 
                value = c(min(reviews$score), max(reviews$score)), 
                step = 0.1, 
                format="#.#")
  })
  
  #artist search text input
  output$artist_search <- renderUI({
    textInput("artist", "Artist search:", "")
  })
  
  #album title search text input
  output$album_search <- renderUI({
    textInput("album", "Album title search:", "")
  })
  
  #record label search text input
  output$label_search <- renderUI({
    textInput("labelSearch", "Record label search:", "")
  })
  
  #record label list selection, populated by top 50 labels
  output$label_picker <- renderUI({
    
    selectInput("label", "Or select from top 50 record labels:",choices=c("all labels",sort(topLabels),"other"))
  })
  
  #reviewer list selection, populated by top 50 reviewers
  output$reviewer_picker <- renderUI({
    
    selectInput("reviewer", "Select reviewer:",choices=c("all reviewers",sort(topReviewers),"other"))
  })
  
  #allow download of review subset
  output$downloadData <- downloadHandler(
    filename = "reviews.csv",
    content = function(file) {
      reviews_sub <- Data()
      write.csv(reviews_sub, file)
    }
  )
  
  output$notes <- renderUI({
    
    #refresh plot to match user query
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 0){
      maxDate <- max(reviews_sub$publish_date)
      minDate <- min(reviews_sub$publish_date)
      HTML(paste0("Displaying ",nrow(reviews_sub)," reviews published between ",minDate," and ",maxDate,"."))
    }
    else
      HTML(paste0("Your query does not match any reviews."))    
    
  })
  
  #specify histogram of matching result
  output$histPlot <- renderPlot({
    
    #refresh plot to match user query
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 0)
    {
      #a stable fill scale for accolade factor
      myColors <- c("#E41A1C","#377EB8","#999999")
      names(myColors) <- levels(reviews_sub$accolade)
      colScale <- scale_fill_manual(name = "Accolade",values = myColors)
      
      #determine y range
      scores <- table(reviews_sub$score)
      scores.srt <- order(scores,decreasing = T)
      scorePeak <- as.integer(scores[scores.srt][1])
      
      #ggplot histogram with median score (7.2), 0.1 binwidth
      q <- ggplot(reviews_sub, aes(x=score)) + 
        geom_histogram(binwidth=0.1, alpha=0.5, colour = "black", aes(fill = factor(accolade))) +
        colScale +
        theme(legend.position="bottom") +
        geom_vline(xintercept=c(7.2), colour = "red") +      
        #       annotate("text", y = scorePeak, x = 7.8, label = "Median Score", color = "red") +
        xlab("Review score") + ylab("Count") +
        xlim(0,10.2) + ylim(0,scorePeak)
      
      print(q)
    }
    
  })  
  
  output$medianNote <- renderUI({
    
    #refresh plot to match user query
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 0)
      HTML('<em>Note</em>: The <span style="color: red">red line</span> indicates the median review score (7.2) for all reviews in the dataset.')      
  })
  
  #specify temporal linegraph of matching result
  output$linePlot <- renderPlot({
    
    #refresh plot to match user query
    reviews_sub <- Data()
    
    #check for matches
    if (nrow(reviews_sub) > 0)
    {
      #       minDate <- min(reviews_sub$publish_date)
      
      #a stable colour scale for accolade factor
      myColors <- c("#E41A1C","#377EB8","#999999")
      names(myColors) <- levels(reviews_sub$accolade)
      colScale <- scale_colour_manual(name = "Accolade",values = myColors)
      
      if (nrow(reviews_sub) < 1000)
        gpoint <- geom_point(alpha=0.5,aes(colour = factor(accolade)),size=3)
      else
        gpoint <- geom_point(alpha=0.5,aes(colour = factor(accolade)),size=2)
      
      #ggplot line graph with median score (7.2)
      if (nrow(reviews_sub) > 1){
        p <- ggplot(reviews_sub, aes(x=publish_date,y=score)) + 
          gpoint + 
          colScale +
          theme(legend.position="none") +      
          stat_smooth(size=1, alpha = 0.2, se=FALSE, fullrange=FALSE) +
          geom_hline(yintercept=c(7.2), colour = "red") + 
          #       annotate("text", x = minDate, y = 6.8, label = "Median Score", color = "red") +
          ylab("Review score") + xlab("Date review published") + 
          ylim(0,10.2)
      }            
      print(p)
    }
    
  })
  
  output$trendNote <- renderUI({
    
    #refresh plot to match user query
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 1)
      HTML('<em>Note</em>: The <span style="color: blue">blue line</span> indicates the trend for currently displayed reviews.')
  })
  
  #display a summary of the dataset
  output$summary <- renderPrint({
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 0)
    {
      summary(reviews_sub[3:7])
    }
    
  })
  
  #display table of matching results
  output$table <- renderTable({
    
    #refresh table to match user query
    reviews_sub <- Data()
    
    #check for matches    
    if (nrow(reviews_sub) > 0)
    {
      #sort by user selection
      switch(input$tableSortBy,
             artist = sortedReviews <- reviews_sub[order(reviews_sub$artist,decreasing=input$isDecreasing),],
             album = sortedReviews <- reviews_sub[order(reviews_sub$album,decreasing=input$isDecreasing),],
             label = sortedReviews <- reviews_sub[order(reviews_sub$label,decreasing=input$isDecreasing),],
             year = sortedReviews <- reviews_sub[order(reviews_sub$release_year,decreasing=input$isDecreasing),],
             reviewer = sortedReviews <- reviews_sub[order(reviews_sub$reviewer,decreasing=input$isDecreasing),],
             score = sortedReviews <- reviews_sub[order(reviews_sub$score,decreasing=input$isDecreasing),],
             accolade = sortedReviews <- reviews_sub[order(reviews_sub$accolade,decreasing=input$isDecreasing),],
             publish_date = sortedReviews <- reviews_sub[order(reviews_sub$publish_date,decreasing=input$isDecreasing),],
             publish_date)
      
      sortedReviews$publish_date <- as.character(sortedReviews$publish_date)
      
      #show first 100 rows of table
      head(sortedReviews, n=input$obs)
    }
  })  
})

# Returns a logical vector of which values in `x` are within the min and max values of `range`.
in_range <- function(x, range) {
  if (is.null(range))
    T
  else
    x >= min(range) & x <= max(range)
}

# Returns a logical vector of BNM records
bnm_check <- function(x, bnm) {
  if (bnm)
    T
  else
    x != "Best New Music"
}

# Returns a logical vector of BNR records
bnr_check <- function(x, bnr) {
  if (bnr)
    T
  else
    x != "Best New Reissue"
}

# Returns a logical vector of non BNM/BNR records
no_acc_check <- function(x, no_acc) {
  if (no_acc)
    T
  else
    x != "None"
}

# Returns a logical vector of which values in `x` partially match the artist query
name_match <- function(x, nameQuery) {
  if (is.null(nameQuery))
    T
  else
    grepl(nameQuery,x, ignore.case=T)
}

# Returns a logical vector of which values in `x` match the selected label.
label_match <- function(x, selectedLabel) {
  if (is.null(selectedLabel) || selectedLabel == "all labels")
    T
  else if (selectedLabel == "other") #labels not in the top 50
    !x %in% topLabels
  else
    x == selectedLabel
}

# Returns a logical vector of which values in `x` match the selected review author
reviewer_match <- function(x, selectedReviewer) {
  if (is.null(selectedReviewer) || selectedReviewer == "all reviewers")
    T
  else if (selectedReviewer == "other") #reviewers not in the top 50
    !x %in% topReviewers
  else
    x == selectedReviewer
}

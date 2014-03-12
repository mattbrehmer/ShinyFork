#ShinyForks P4K review explorer
#by Matt Brehmer
#Updated Nov 10, 2013 
#global.r

library(shiny)
library(ggplot2)
library(plyr)

#load, tidy the data
reviews <- read.csv("pitchfork_review_data.csv",stringsAsFactors=FALSE)
  reviews <- reviews[c("artist","album","label","release_year","publish_date","accolade","score","reviewer","url")]

reviews$publish_date <- as.Date(reviews$publish_date)

#if release year empty, use the year from the review publish data
emptyYears <- which(is.na(reviews$release_year)==TRUE)
reviews$release_year[emptyYears] <- format(reviews$publish_date[emptyYears], "%Y")
reviews$release_year <- as.integer(reviews$release_year)

reviews$score <- as.double(reviews$score)

#if label year empty, use n/a
reviews$label <- gsub("^ ","",reviews$label)
reviews$label <- gsub("^$","(n/a)",reviews$label)
reviews$label <- as.factor(reviews$label)
reviews$reviewer <- as.factor(reviews$reviewer)

#format the accolades
reviews$accolade <- gsub("^[ ]+Best New Music[ ]+$","Best New Music",reviews$accolade)
reviews$accolade <- gsub("^[ ]+Best New Reissue[ ]+$","Best New Reissue",reviews$accolade)
reviews$accolade <- gsub("^[ ]+$","None",reviews$accolade)
reviews$accolade <- as.factor(reviews$accolade)

#sort by publish date descending
reviews <- arrange(reviews,desc(publish_date))

#get table of record labels with review counts
recordLabels <- table(reviews$label)
#sort record label table in decreasing order
labels.srt <- order(recordLabels,decreasing = T)    
#get top 50 record labels by review count
topLabels <- names(recordLabels[labels.srt][1:50])

#get table of review authors with review counts
reviewers <- table(reviews$reviewer)
#sort review author table in decreasing order
reviewers.srt <- order(reviewers,decreasing = T)
#get top 50 review authors by review count
topReviewers <- names(reviewers[reviewers.srt][1:50])

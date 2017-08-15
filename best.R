rm(list=ls())
##setwd("F:/Work/Coursera/R Programming/ProgrammingAssignment3")
##list.files()
##hosdata <- read.csv("hospital-data.csv", header=T, colClasses = "character")
##measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
##names(measure.data)
##Required Columns 2,7,11,17,23
##measure.data[,11] <- as.numeric(measure.data[,11])
##hist(measure.data[,11])

best <- function(state,outcome) {
  ##state <- "AL"
  ##outcome <- "heart attack"
  measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
    state_le30_min_mor <- measure.data[,c(2,7,11,17,23)]
  names(state_le30_min_mor) <- c("Hospital", "State","heart attack","heart failure","pneumonia")
  if (!(state %in% unique(state_le30_min_mor[,"State"]))) {
    stop("invalid state entered")
  }
  else if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("Invalid outcome entered")
  }
  else {
    ##outcome <- "heart attack"
    input.st <- state_le30_min_mor[state_le30_min_mor[,"State"]==state,]
    input.st[,outcome] <- as.numeric(input.st[,outcome])
    min.outcome.idx <- which(input.st[,outcome]==min(input.st[,outcome],na.rm=T))
    result <- input.st[min.outcome.idx,]
  }
  return(result[,"Hospital"])
}

best(state="AL",outcome="heart attack")
best(state="AL",outcome="heart failure")
best(state="AL",outcome="pneumonia")
best(state="AL",outcome="heart attack")

best("TX", "heart attack")
##[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
##"FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
##[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
##"GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
###Error in best("BB", "heart attack") : invalid state entered
best("NY", "hert attack")
###Error in best("NY", "hert attack") : Invalid outcome entered



rm(list=ls())
##setwd("F:/Work/Coursera/R Programming/ProgrammingAssignment3")
##list.files()
##hosdata <- read.csv("hospital-data.csv", header=T, colClasses = "character")
##measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
##names(measure.data)
##Required Columns 2,7,11,17,23
##measure.data[,11] <- as.numeric(measure.data[,11])
##hist(measure.data[,11])

rankhospital <- function(state,outcome,num=3) {
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
  ##num <- 10
  else if(is.numeric(num)) {
    input.st <- state_le30_min_mor[state_le30_min_mor[,"State"]==state,]
    input.st[,outcome] <- as.numeric(input.st[,outcome])
    input.st.ord <- input.st[order(input.st[,outcome],input.st[,"Hospital"]),]
    input.st.ord$rank <- rank(input.st.ord[,outcome],ties.method = "first")
    outcome.idx <- which(input.st.ord$rank==num)
    hos <- input.st.ord[outcome.idx,"Hospital"]
  }
  else if (!(is.numeric(num))) {
    if(num=="best") {
      hos <- best(state,outcome)
    }
    else if(num=="worst") {
      input.st <- state_le30_min_mor[state_le30_min_mor[,"State"]==state,]
      input.st[,outcome] <- as.numeric(input.st[,outcome])
      input.st.ord <- input.st[order(input.st[,outcome],input.st[,"Hospital"],decreasing=T),]
      hos <- input.st.ord[1,"Hospital"]
    }
  }
  else {
    stop("Invalid rank")
  }
  return(hos)
}

rankhospital(state="AL",outcome="heart attack",num=25)
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

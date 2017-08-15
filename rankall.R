rm(list=ls())
##setwd("F:/Work/Coursera/R Programming/ProgrammingAssignment3")
##list.files()
##hosdata <- read.csv("hospital-data.csv", header=T, colClasses = "character")
##measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
##names(measure.data)
##Required Columns 2,7,11,17,23
##measure.data[,11] <- as.numeric(measure.data[,11])
##hist(measure.data[,11])

rankall <- function(outcome,num=5) {
  measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
  state_le30_min_mor <- measure.data[,c(2,7,11,17,23)]
  names(state_le30_min_mor) <- c("Hospital", "State","heart attack","heart failure","pneumonia")
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("Invalid outcome entered")
  }
  else if(is.numeric(num)) {
    ##outcome <- "heart attack"
    all.ranked <- data.frame()
    state_le30_min_mor[,outcome] <- as.numeric(state_le30_min_mor[,outcome])
    for(i in unique(state_le30_min_mor[,2])) {
      input.st <- state_le30_min_mor[state_le30_min_mor[,"State"]==i,]
      input.st.ord <- input.st[order(input.st[,outcome],input.st[,"Hospital"]),]
      input.st.ord$rank <- rank(input.st.ord[,outcome],ties.method = "first")
      all.ranked <- rbind(all.ranked,input.st.ord)
    }
    result <- all.ranked[all.ranked$rank==num,c("Hospital","State")]  
  }
  
  else if(!(is.numeric(num))) {
    if(num=="best") {
      output.all <- data.frame()
      for(i in unique(state_le30_min_mor[,2])) {
        output <- cbind(best(i,outcome="heart attack"),i)
        output.all <- rbind(output.all,output)
      }
      names(output.all) <- c("Hospital","State")
      result <- output.all
    }
    else if(num=="worst") {
      output.all <- data.frame()
      for(i in unique(state_le30_min_mor[,2])) {
        input.st <- state_le30_min_mor[state_le30_min_mor[,"State"]==i,]
        input.st.ord <- input.st[order(input.st[,outcome],input.st[,"Hospital"],decreasing=T),]
        output <- input.st.ord[1,c("State","Hospital")]
        output.all <- rbind(output.all,output)
      }
      result <- output.all
    }
  }
  else {
    return(result)
  }
}
head(rankall("heart attack", "worst"), 10)
tail(rankall("pneumonia", "worst"), 3)

rankall(outcome="heart attack",num=10)

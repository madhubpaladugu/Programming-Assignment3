setwd("F:/Work/Coursera/R Programming/ProgrammingAssignment3")
list.files()
hosdata <- read.csv("hospital-data.csv", header=T, colClasses = "character")
measure.data <- read.csv("outcome-of-care-measures.csv", header=T,colClasses = "character")
str(measure.data)
measure.data[,11] <- as.numeric(measure.data[,11])
hist(measure.data[,11])

best <- function(state,outcome) {
  statecnt <- length(unique(measure.data[,7]))
  if (!(state %in% unique(measure.data[,"State"]))) {
    stop("invalid state")
  }
  if (!(outcome %in% unique(measure.data[,c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]))) {
    stop("Entered Variable not found in the dataframe")
  }
}

best(state="AL",outcome='test')
best <- sapply(split(measure.data[,11],as.factor(measure.data[,"State"])),min,na.rm=T)

names(measure.data)

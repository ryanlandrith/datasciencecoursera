library(dplyr)

best <- function(state, outcome) {
  
  #valid state check
  #valid outcome check
  #Find set of hospitals matching state argument
  #sort by outcome
  #Handle ties
  temp <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  temp <- filter(temp,temp$State==state)
  if(nrow(temp)==0)
  {
    stop("invalid state")
  }
  if(outcome =="heart attack")
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),temp$Hospital.Name,na.last = NA),]
    
  }
  else if(outcome == "pneumonia" )
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),temp$Hospital.Name,na.last = NA),]
    
  }
  else if(outcome == "heart failure")
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),temp$Hospital.Name,na.last = NA),]
    
  }
  else
  {
    stop("invalid outcome")
  }
 temp$Hospital.Name[1]

  
}



rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  temp <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  temp <- filter(temp,temp$State==state)
  if(nrow(temp)==0)
  {
    stop("invalid state")
  }
  if(outcome =="heart attack")
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),temp$Hospital.Name,na.last = NA),]
    
  }
  else if(outcome == "pneumonia" )
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),temp$Hospital.Name,na.last = NA),]
    
  }
  else if(outcome == "heart failure")
  {
    temp <- temp[order(as.numeric(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),temp$Hospital.Name,na.last = NA),]
    
  }
  else
  {
    stop("invalid outcome")
  }
 
  ranks <- temp$Hospital.Name
  if(num=="best") index <- 1
  if(num=="worst") index <- length(ranks)
  else index <- as.numeric(num)

  ranks[index]
  
}




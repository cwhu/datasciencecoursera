####2. Finding the best hospital in a state

best <- function(state, outcome) { 
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  type <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
    if (any(dt$State == state) & any(type == outcome)) {
  ## Return hospital name in that state with lowest 30-day death rate
      lowest <- vector()
      stdata <- dt[dt$State == state, c(2, 11, 17, 23)]
      #stdata[stdata == "Not Available"] <- NA
  
        if(outcome == "heart attack") {
          min1 = min(as.numeric(stdata[,2]), na.rm = TRUE)
          lowest <- sort(stdata$Hospital.Name[as.numeric(stdata[,2]) == min1], na.last = NA) 
        } else if(outcome == "heart failure") {
          min2 = min(as.numeric(stdata[,3]), na.rm = TRUE)
          lowest <- sort(stdata$Hospital.Name[as.numeric(stdata[,3]) == min2], na.last = NA) 
        } else if(outcome == "pneumonia") {
          min3 = min(as.numeric(stdata[,4]), na.rm = TRUE)
          lowest <- sort(stdata$Hospital.Name[as.numeric(stdata[,4]) == min3], na.last = NA)
        }
  
         return(lowest[1])   
      
    } else if(!any(dt$State == state)) {
      stop("invalid state")
    } else if(!any(type == outcome)) {
      stop("invalid outcome")
    }
}

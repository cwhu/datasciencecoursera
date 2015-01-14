####4. Ranking hospitals in all states
###Write a function called rankall that takes two arguments: 
###an outcome name (outcome) and a hospital ranking (num). 
###The function reads the outcome-of-care-measures.csv ???le and 
###returns a 2-column data frame containing the hospital in each 
###state that has the ranking speci???ed in num. For example the 
###function call rankall("heart attack", "best") would return a 
###data frame containing the names of the hospitals that are the 
###best in their respective states for 30-day heart attack death rates. 
###The function should return a value for every state (some may be NA). 
###The ???rst column in the data frame is named hospital, which contains 
###the hospital name, and the second column is named state, which 
###contains the 2-character abbreviation for the state name. Hospitals 
###that do not have data on a particular outcome should be excluded 
###from the set of hospitals when deciding the rankings.

###Handling ties. The rankall function should handle ties in the 
###30-day mortality rates in the same way that the rankhospital 
###function handles ties.

rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  type <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  
  if(!any(type == outcome)) {
      stop("invalid outcome")
    
  } else {
   
  ## For each state, find the hospital of the given rank
    ha <- dt[,c(2, 7, 11)]
    colnames(ha) <- c("hospital", "state", "outcome")
    ha <- ha[order(ha$state, as.numeric(ha$outcome), ha$hospital),]
    
    hf <- dt[,c(2, 7, 17)]
    colnames(hf) <- c("hospital", "state", "outcome")
    hf <- hf[order(hf$state, as.numeric(hf$outcome), hf$hospital),]
    
    pn <- dt[,c(2, 7, 23)] 
    colnames(pn) <- c("hospital", "state", "outcome")
    pn <- pn[order(pn$state, as.numeric(pn$outcome), pn$hospital),]
    
    ls <- list()
    state <- vector()
    
    if(outcome == "heart attack") {
      state <- unique(ha$state)
      for(i in 1:length(state)){
        ls[[i]] <- ha[ha$state == state[i],]
      }
    } else if(outcome == "heart failure") {
      state <- unique(hf$state)
      for(i in 1:length(state)){
        ls[[i]] <- hf[hf$state == state[i],]
      }
    } else if(outcome == "pneumonia") {
      state <- unique(pn$state)
      for(i in 1:length(state)){
        ls[[i]] <- pn[pn$state == state[i],]
      }
    }
  }
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name

  if(num == "best") {
    for(i in 1:length(ls)) {
      if(i == 1) df <- ls[[1]][1, 1]
      if(i > 1) df <- rbind(df, ls[[i]][1, 1])
      } 
  } else if(num == "worst") {
    for(i in 1:length(ls)) {
      if(i == 1) {
        num = sum(!is.na(as.numeric(ls[[1]]$outcome)))
        df <- ls[[1]][num, 1]
      }
      if(i > 1) {
        num = sum(!is.na(as.numeric(ls[[i]]$outcome)))
        df <- rbind(df, ls[[i]][num, 1])
      }
    } 
  } else {
    for(i in 1:length(ls)) {
      if(i == 1) {
        if(num > sum(!is.na(as.numeric(ls[[1]]$outcome)))) {
              ls[[1]] <- NA
              df <- ls[[1]]
          } else {
              df <- ls[[1]][num, 1]
        }
      }
      if(i > 1) {
        if(num > sum(!is.na(as.numeric(ls[[i]]$outcome)))) {
              ls[[i]] <- NA
              df <- rbind(df, ls[[i]])
          } else {
              df <- rbind(df, ls[[i]][num, 1])
        }
      }
    }
  }
  
  df <- data.frame(df[,1], sort(unique(dt$State)))
  colnames(df) <- c("hospital", "state")
  rownames(df) <- sort(make.unique(unique(dt$State)))
  return(df)

}


###The function should check the validity of its arguments. 
###If an invalid outcome value is passed to rankall, the function 
###should throw an error via the stop function with the exact 
###message"invalid outcome". The num variable can take values 
###"best", "worst", or an integer indicating the ranking 
###(smaller numbers are better). If the number given by num is 
###larger than the number of hospitals in that state, then the 
###function should return NA.

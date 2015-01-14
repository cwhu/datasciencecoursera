####3. Ranking hospitals by outcome in a state
###Write a function called rankhospital that takes three arguments: 
###the 2-character abbreviated name of a state (state), an outcome (outcome), 
###and the ranking of a hospital in that state for that outcome (num). 
###The function reads the outcome-of-care-measures.csv ???le and returns a 
###character vector with the name of the hospital that has the ranking 
###speci???ed by the num argument. For example, the call
###rankhospital("MD", "heart failure", 5)
###would return a character vector containing the name of the hospital 
###with the 5th lowest 30-day death rate for heart failure. The num argument 
###can take values "best", "worst", or an integer indicating the ranking 
###(smaller numbers are better). If the number given by num is larger than 
###the number of hospitals in that state, then the function should return NA. 
###Hospitals that do not have data on a particular outcome should be excluded 
###from the set of hospitals when deciding the rankings.

###Handling ties. It may occur that multiple hospitals have the same 
###30-day mortality rate for a given cause of death. In those cases ties 
###should be broken by using the hospital name. For example, in Texas ("TX"), 
###the hospitals with lowest 30-day mortality rate for heart failure are shown here.



###The function should use the following template.

rankhospital <- function(state, outcome, num = "best") { ## Read outcome data
  
    ## Read outcome data
    dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    type <- c("heart attack", "heart failure", "pneumonia")
    ## Check that state and outcome are valid
    
    if(!any(dt$State == state)) {
      stop("invalid state")
    } else if(!any(type == outcome)) {
      stop("invalid outcome")
    } else if(any(dt$State == state) & any(type == outcome)) {
      ## Return hospital name in that state with lowest 30-day death rate
          rank <- vector()
          ha <- dt[dt$State == state ,c(2, 11)]
          colnames(ha) <- c("name", "outcome")
          hf <- dt[dt$State == state ,c(2, 17)]
          colnames(hf) <- c("name", "outcome")
          pn <- dt[dt$State == state ,c(2, 23)] 
          colnames(pn) <- c("name", "outcome")
          
      if(outcome == "heart attack") {
        rank <- ha[order(as.numeric(ha$outcome), ha$name),]
      } else if(outcome == "heart failure") {
        rank <- hf[order(as.numeric(hf$outcome), hf$name),]
      } else if(outcome == "pneumonia") {
        rank <- pn[order(as.numeric(pn$outcome), pn$name),]
      }
    }

      if(num == "best") {
        rank = rank$name[1]
      } else if(num == "worst") {
        num = sum(!is.na(as.numeric(rank$outcome)))
        rank = rank$name[num]
      } else if(num > sum(!is.na(as.numeric(rank$outcome)))) {
        rank = NA
      } else {
        rank = rank$name[num]
      }
    
    return(rank)
}
      
  

###The function should check the validity of its arguments. If an invalid 
###state value is passed to best, the function should throw an error via 
###the stop function with the exact message "invalid state". If an invalid 
###outcome value is passed to best, the function should throw an error via 
###the stop function with the exact message "invalid outcome".


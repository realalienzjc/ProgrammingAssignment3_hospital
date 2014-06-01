rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  hospitals <- read.csv("hospital-data.csv")
  
  ## Check that state and outcome are valid
  valid.state <- levels(as.factor(outcomes$State))
  valid.outcome <- levels(as.factor(c("heart attack","heart failure","pneumonia"))) ## 11, 17, 23
  #if (! is.element(state, valid.state)) { stop("invalid state") }
  if (! is.element(outcome, valid.outcome)) { stop("invalid outcome") }
  #print("Good to go!")
  
  #clean
  outcomes[, 11] <- as.numeric(outcomes[,11],rm.na=T)
  outcomes[, 17] <- as.numeric(outcomes[,17],rm.na=T)
  outcomes[, 23] <- as.numeric(outcomes[,23],rm.na=T)
  
  colIdx <- if ( "heart attack" == outcome ) {11}
  else if ( "heart failure" == outcome) { 17}
  else if ( "pneumonia" == outcome) { 23}
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  of.one.state <- function(elem){  # list elem of one states
    #print(elem)
    df <- data.frame(elem)  # convert to data frame
    df <- df[complete.cases(df[colIdx]),]  # remove empty
    df.sorted <- df[order(df[,colIdx], df[,2], na.last = TRUE), ]  # sorted by measure and hospital name. 
    
    out <- if ( num == "best") {
      df.sorted[1,c(2,7)] 
    } else if ( num == "worst"){                        
      df.sorted[nrow(df.sorted),c(2,7)]
    } else if ( num > nrow(df.sorted) ){
      #print(df.sorted[7])
      # NOTE: here can NOT  df.sorted[7]  Q:Why?
      d <- data.frame(NA, levels(as.factor(df.sorted$State))) 
      names(d) <- c( names(df.sorted)[2],"State")
      d
    } else {
      df.sorted[num,c(2,7)]
    }    
    # check 
    #print( df.sorted[num,c(2,7)])
    #print("----------")
    out      
  }
  
  output <- lapply(split(outcomes, as.factor(outcomes$State)), of.one.state)
  df.final<-do.call('rbind',output) 
  names(df.final) <- c("hospital", "state")
  #print(df.final)
  df.final
}

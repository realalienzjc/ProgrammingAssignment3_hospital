rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        hospitals <- read.csv("hospital-data.csv")
        
        ## Check that state and outcome are valid
        valid.state <- levels(as.factor(outcomes$State))
        valid.outcome <- levels(as.factor(c("heart attack","heart failure","pneumonia"))) ## 11, 17, 23
        if (! is.element(state, valid.state)) { stop("invalid state") }
        if (! is.element(outcome, valid.outcome)) { stop("invalid outcome") }
        #print("Good to go!")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        outcomes[, 11] <- as.numeric(outcomes[,11],rm.na=T)
        outcomes[, 17] <- as.numeric(outcomes[,17],rm.na=T)
        outcomes[, 23] <- as.numeric(outcomes[,23],rm.na=T)
        
        colIdx <- if ( "heart attack" == outcome ) {11}
        else if ( "heart failure" == outcome) { 17}
        else if ( "pneumonia" == outcome) { 23}
        by.state <- split(outcomes, outcomes$State)  #Q: memory usage? better way?  A:
        df <- data.frame(by.state[state][1]) # find location specific data   NOTE: the.state <-  by.state[state]
        df.sorted <- df[order(df[,colIdx], df[,2]),]  # sort by measurement,name
        
        
        if ( length(df.sorted) > 0){
                # clean for NA udring convertions
                df.sorted <- df.sorted[complete.cases(df.sorted[colIdx]),]
                if ( num == "best") {
                        df.sorted[1,2] 
                } else if ( num == "worst"){                        
                        df.sorted[nrow(df.sorted),2]
                } else if ( num > length(df.sorted) ){
                        NA
                } else {
                        df.sorted[num,2]     
                }
        } else {
                NA 
        }  
}

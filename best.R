best <- function(state, outcome) {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomedf <- data.frame(validoutcomes=c("heart attack", "heart failure", "pneumonia"), position=c(11,17,23))
        ## Check that state and outcome are valid
        if (!(state %in% outcomedata$State)) {
                stop("invalid state")
        }
        pos <- outcomedf[match(outcome, outcomedf$validoutcomes), "position"]
        if( is.na(pos) ) {
                stop("invalid outcome")
        }
        #filter by state
        outcomebystate <- outcomedata[outcomedata$State == state, ]
        #coerce the column to numeric
        outcomebystate[, pos] <- as.numeric(x=outcomebystate[, pos])
        #just complete cases
        outcomebystate <- outcomebystate[complete.cases(outcomebystate), ]
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        hospital <- outcomebystate[(outcomebystate[, pos] == min(outcomebystate[, pos])), ]$Hospital.Name
        #sort in case of tie
        sort(hospital)[1]

}
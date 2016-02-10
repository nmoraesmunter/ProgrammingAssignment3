rankhospital <- function(state, outcome, num = "best") {
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
        #order by death rate and name
        outcomebystateRank <- outcomebystate[order(outcomebystate[,pos], outcomebystate$Hospital.Name),]
        l <- nrow(outcomebystateRank)
        if ( num == "best") {
                outcomebystateRank[1,2]
        } else if (num == "worst") {
                outcomebystateRank[l,2]
        } else if (num > l) {
                NA
        } else {
                outcomebystateRank[num,2]
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}
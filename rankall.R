rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomedf <- data.frame(validoutcomes=c("heart attack", "heart failure", "pneumonia"), position=c(11,17,23))
        ## Check that outcome is valid
        pos <- outcomedf[match(outcome, outcomedf$validoutcomes), "position"]
        if( is.na(pos) ) {
                stop("invalid outcome")
        }
        
        #coerce the column to numeric
        outcomedata[, pos] <- as.numeric(x=outcomedata[, pos])
        #just complete cases
        outcomedata <- outcomedata[complete.cases(outcomedata), ]
        #split by state
        splitedByState <- split(outcomedata, outcomedata$State)

        res <- lapply(splitedByState, function(x, num) {
                #order by death rate and name
                x <- x[order(x[,pos], x$Hospital.Name),]
                # Return
                l <- nrow(x)
                if ( num == "best") {
                        x[1,2]
                } else if (num == "worst") {
                        x[l,2]
                } else if (num > l) {
                        NA
                } else {
                        x[num,2]
                }
               
        }, num)
        
        data.frame(hospital=unlist(res), state=names(res))

}
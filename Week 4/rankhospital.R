rankhospital <- function(state, outcome, num = "best") {
    ## "state" is two letter state name, "outcome" is "heart attack", "heart failure", or "pneumonia"
    
    data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  #load data
    data_sub <- data_set[, c(2, 7, 11, 17, 23)]                                     #reduce to columns of interest

    ## check if state and outcome are valid
    if(!is.element(state, data_sub$State)) {
        stop("invalid state")
    }
    
    if(outcome == "heart attack") {
        col_num <- 3
    } else if(outcome == "heart failure") {
        col_num <- 4
    } else if(outcome == "pneumonia") {
        col_num <- 5
    } else {
        stop("invalid outcome")
    }
    
    suppressWarnings(data_sub[, col_num] <- as.numeric(data_sub[, col_num]))        #convert outcome to numeric
    data_sub2 <- data_sub[!is.na(data_sub[col_num]) & data_sub[2] == state, c(1, 2, col_num)]
    ordering <- with(data_sub2, order(data_sub2[3],data_sub2[1]))
    data_sub2 <- data_sub2[ordering,]
    
    if(num == "best") {
        data_sub2[1, "Hospital.Name"]
    } else if(num == "worst") {
        data_sub2[nrow(data_sub2), "Hospital.Name"]
    } else if(is.numeric(num)) {
        data_sub2[num, "Hospital.Name"]
    } else {
        stop("invalid ranking")
    }
}
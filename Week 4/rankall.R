rankall <- function(outcome, num = "best") {

    data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  #load data
    data_sub <- data_set[, c(2, 7, 11, 17, 23)]
    
    if(outcome == "heart attack") {
        col_num <- 3
    } else if (outcome == "heart failure") {
        col_num <- 4
    } else if (outcome == "pneumonia") {
        col_num <- 5
    } else {
        stop("invalid outcome")
    }
    
    suppressWarnings(data_sub[, col_num] <- as.numeric(data_sub[, col_num]))        #convert outcome to numeric
    data_sub2 <- data_sub[!is.na(data_sub[col_num]), c(1, 2, col_num)]
    
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- 1
        data_sub2[3] <- data_sub2[3] * -1
    }
    
    ordering <- with(data_sub2, order(data_sub2[2],data_sub2[3]))
    data_sub2 <- data_sub2[ordering,]
    
    data_split <- split(data_sub2, as.factor(data_sub2$State))
    
    result <- data_split[[1]][num, 1:2]
    if(is.na(data_split[[1]][num, 1])) {
        result[1,2] <- names(data_split[1])
    }
    for (i in 2:54) {
        result <- rbind(result, data_split[[i]][num, 1:2])
        if(is.na(data_split[[i]][num, 1])) {
            result[i,2] <- names(data_split[i])
        }
    }
    result
}
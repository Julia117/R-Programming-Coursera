rankall <- function(outcome, num = "best"){
        outcomes <- c("heart attack" = 11,
                      "heart failure" = 17,
                      "pneumonia" = 23)
        data  <- read.csv(
                "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                na.strings = "Not Available",
                stringsAsFactors = FALSE
        )
        data_states <- split(data, data$State)
        
        result <- data.frame()
        
        for (state in names(data_states)){
                one_state <- data_states[[state]]
                check_input()
                if (num == "best") {
                        num <- 1
                }
                ordered <- one_state[order (one_state[outcomes[outcome]],
                                            one_state$Hospital.Name), ]
                
                
                res <- ordered[!is.na(ordered[outcomes[outcome]]),]
                
                rank_h <- 
                         if (!is.character(num) && num > nrow(res)) {
                         NA
                 } else
                        if (is.character(num))
                {
                        tail(res$Hospital.Name, n = 1)
                } else {
                        res$Hospital.Name[num]
                }
                result <- rbind(result, data.frame("hospital" = rank_h,
                                                   "state" = state))
                
        }
        result
}

check_input <- function(){
        if (is.null(one_state)) {
                stop("invalid state")
        }
        if (is.na(outcomes[outcome])) {
                stop("invalid outcome")
        }
}

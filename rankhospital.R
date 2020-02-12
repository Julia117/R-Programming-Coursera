rankhospital <- function(state, outcome, num) {
        outcomes <-
                c(
                        "heart attack" = 11,
                        "heart failure" = 17,
                        "pneumonia" = 23
                )
        data  <-
                read.csv(
                        "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        na.strings = "Not Available",
                        stringsAsFactors = FALSE
                )
        data_states <- split(data, data$State)
        one_state <- data_states[[state]]
        if (is.null(one_state)) {
                stop("invalid state")
        }
        if (is.na(outcomes[outcome])) {
                stop("invalid outcome")
        }
        ordered <- one_state[order (one_state[outcomes[outcome]],
                                    one_state$Hospital.Name), ]
        
        res <- ordered[!is.na(ordered[outcomes[outcome]]),]
        if (!is.character(num) && num > nrow(res)) {
                return(NA)
        } else if (is.character(num))
        {
                if (num == "best") {
                        res$Hospital.Name[1]
                } else{
                        tail(res$Hospital.Name, n = 1)
                }
        }
        else {
                res$Hospital.Name[num]
        }
        
}

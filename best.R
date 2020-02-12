outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
 ## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


best <- function(state, outcome){
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
  data  <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
                    na.strings = "Not Available", 
                    stringsAsFactors = FALSE)
  data_states <- split(data, data$State)
  one_state <- data_states[[state]]
  if (is.null(one_state)){
    stop("invalid state")
  }
  
  if (is.na(outcomes[outcome])){
    stop("invalid outcome")
  }
  min_death_rate <- min(one_state[[outcomes[outcome]]], na.rm = TRUE)

  hospital <- sort(one_state$Hospital.Name[which(one_state[outcomes[outcome]] 
                                                 == min_death_rate)])[1]
  hospital
}


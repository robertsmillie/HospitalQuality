simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


rankall <- function(outcome, num = 1) {
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  outcome <- simpleCap(outcome)
  outcome <- gsub(" ", ".", outcome)
  validOutcome <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
  if (!(outcome %in% validOutcome)) {
    stop("invalid state.")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  states <- unique(data$State)
  states <- states[order(states)]
  hospitals <- rep(NA, length(states))
  results <- data.frame(states, hospitals)
  data[,eval(columnName)] <- as.numeric(data[,eval(columnName)])
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")
  for (state1 in states) {
    rn <- which(states == state1, arr.ind=TRUE)
    data2 <- data[data$State == state1,]
    df <- data.frame(data2$Hospital.Name,data2[,eval(columnName)])
    colnames(df) <- c("Hospital Name", "Rate")
  
    suppressWarnings(df$Rate <- as.numeric(as.character(df$Rate)))
    #df$Rate <- as.numeric(as.character(df$Rate))
    df <- df[order(df$`Hospital Name`),]
    df <- df[order(df$Rate),]
    #head(df)
    df <- df[complete.cases(df),]
    if (num == "best") {
      num1 <- 1
    }
    else if (num == "worst") {
      num1 <- nrow(df)
    }
    else {
      num1 <- num
    }
    #results <- data[,eval(columnName)]
    results$hospitals[rn] <- toString(df$`Hospital Name`[num1])
    #complete.cases(df) 
  }
  colnames(results) <- c("state", "hospital")
  results
  
}
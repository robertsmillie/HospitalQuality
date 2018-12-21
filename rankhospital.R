simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


rankhospital <- function(state, outcome, num = 1) {
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  outcome <- simpleCap(outcome)
  outcome <- gsub(" ", ".", outcome)
  validOutcome <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
  if (!(state %in% data$State)) {
    stop("invalid state.")
  }
  if (!(outcome %in% validOutcome)) {
    stop("invalid state.")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data[,eval(columnName)] <- as.numeric(data[,eval(columnName)])
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")
  data <- data[data$State == state,]
  df <- data.frame(data$Hospital.Name,data[,eval(columnName)])
  colnames(df) <- c("Hospital Name", "Rate")
 
  df$Rate <- as.numeric(as.character(df$Rate))

  df <- df[order(df$`Hospital Name`),]
  df <- df[order(df$Rate),]
  #head(df)
  df <- df[complete.cases(df),]
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst") {
    num <- nrow(df)
  }
  #results <- data[,eval(columnName)]
  toString(df$`Hospital Name`[num])
  #complete.cases(df) 
  
}
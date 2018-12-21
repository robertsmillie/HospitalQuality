simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


best <- function(state, outcome) {
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
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")
  data <- data[data$State == state,]
  data[,eval(columnName)] <- as.numeric(as.character(data[,eval(columnName)]))
  data <- data[order(data$Hospital.Name),]
  mincol<-which.min(data[,eval(columnName)])
  names <- data$Hospital.Name
  toString(names[mincol])
}
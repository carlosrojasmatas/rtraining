best <- function(state,out) {
  
  asNumeric <- function(x) {
    suppressWarnings(as.numeric(x))
  }
  outs <- c("heart attack" = 11,"heart failure" = 17, "pneumonia" = 23)

    ## read the data
  outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## check the args
  
  if(is.na(match(state,outcome$State))){
    stop(paste("invalid state: ", state))
  }
  
  colIdx <- match(out,names(outs))
  if(is.na(colIdx)) {
    stop(paste("invalid outcome: ", out))
  } 
  
  ##subset only that state
  subState <- subset(outcome,State == state)
  ##get only the complete cases
  
  outByHosp <- subState[,c(2,outs[[colIdx]])]
  outByHosp[,2] <- sapply(outByHosp[,2],asNumeric)
  
  comp <- outByHosp[complete.cases(outByHosp),]
  comp[which.min(comp[,2]),1]
  
}
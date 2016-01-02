rankhospital <- function(state, out, num = "best") {

  state <- toupper(state)
  outs <- c("heart attack" = 11,"heart failure" = 17, "pneumonia" = 23)
  asNumeric <- function(x) {
    suppressWarnings(as.numeric(x))
  }
  
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Check that state and outcome are valid
  if(is.na(match(state,outcome$State))){
    stop(paste("invalid state: ", state))
  }
  
  colIdx <- match(out,names(outs))
  if(is.na(colIdx)) {
    stop(paste("invalid outcome: ", out))
  } 
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ##subset only that state
  subState <- subset(outcome,State == state)
  if(is.numeric(num) & num > length(subState$Hospital.Name)){
    NA
  }
  
  outByHosp <- subState[,c(2,outs[[colIdx]])]
  outByHosp[,2] <- sapply(outByHosp[,2],asNumeric)
  comp <- outByHosp[complete.cases(outByHosp),]

  if (num == "best"){
    comp[which.min(comp[,2]),1]
  }else if (num == "worst"){
    comp[which.max(comp[,2]),1]
  }else {
    ord <- comp[order(comp[,2],comp[,1]),]
    ord[num,1]
  }
  
}
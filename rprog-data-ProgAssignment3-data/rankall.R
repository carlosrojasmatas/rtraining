rankall <- function(out, num = "best") {
  
  outs <- c("heart attack" = 11,"heart failure" = 17, "pneumonia" = 23)
  asNumeric <- function(x) {
    suppressWarnings(as.numeric(x))
  }
  
  
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Check that state and outcome are valid
  colIdx <- match(out,names(outs))
  if(is.na(colIdx)) {
    stop(paste("invalid outcome: ", out))
  } 
  
  states <- unique(outcome$State)
  rs <- data.frame(hospital=as.character(),state=as.character())
  for(st in states){
    subState <- subset(outcome,State == st)
    outByHosp <- subState[,c(2,outs[[colIdx]])]
    outByHosp[,2] <- sapply(outByHosp[,2],asNumeric)
    # comp <- outByHosp[complete.cases(outByHosp),]
    comp <- outByHosp
    
    if (num == "best"){
      rs <- rbind(rs,data.frame(hospital = comp[which.min(comp[,2]),1],state=st))
    }else if (num == "worst"){
      rs <- rbind(rs,data.frame(hospital = comp[which.max(comp[,2]),1],state=st))
    }else {
      ord <- comp[order(comp[,2],comp[,1]),]
      rs <- rbind(rs, data.frame(hospital= ord[num,1], state=st))
    }
  }
  ## For each state, find the hospital of the given rank
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
rs[order(as.character(rs$state)),]
  
}
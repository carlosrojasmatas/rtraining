corr <- function(directory,threshold=0){
  
  source("complete.R")    
  resolvename <- function(name) {
    fname <- if( name<10) {
      paste("/00",name,sep="")} 
    else if (name < 100) {
      paste("/0",name,sep = "")}
    else {
      paste("/",name,sep="")
    }  
  }
  
  comps <- complete("specdata")
  targets <- numeric(0)
  for (i in comps$id){
      r <- comps[i,]
      if(r$nobs > threshold){
        targets <- c(targets,r$id)
      }
  }
  results <- numeric(0)
  for(i in targets){
    name <- resolvename(i)
    fullName <- paste(directory,name,".csv",sep = "")
    content <- read.csv(fullName)
    completed <- content[complete.cases(content),]
    c <- cor(completed$sulfate,completed$nitrate)
    results <- c(results,c)
  }
  results
}
complete <- function(directory,id = 1:332){
  
  
  resolvename <- function(name) {
    fname <- if( name<10) {
      paste("/00",name,sep="")} 
    else if (name < 100) {
      paste("/0",name,sep = "")}
    else {
      paste("/",name,sep="")
    }  
  }
  
  counts <- numeric(length(id))
  
  for( i in seq_along(id)){
    name <- resolvename(id[i])
    fullName <- paste(directory,name,".csv",sep = "")
    content <- read.csv(fullName)
    for(line in 1:dim(content)[1]){
      sultval <- content[line,]$sulfate
      nitval <- content[line,]$nitrate
      if(!is.na(nitval) && !is.na(sultval)){
        counts[i] = counts[i]+1
      }
    }  
  }
  data.frame("id"=seq_along(counts),"nobs" = counts)
  
}
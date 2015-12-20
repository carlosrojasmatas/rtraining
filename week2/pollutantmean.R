pollutantmean <- function(directory,pollutant,id=1:332) {
  
  resolvename <- function(name) {
    fname <- if( name<10) {
      paste("/00",name,sep="")} 
    else if (name < 100) {
      paste("/0",name,sep = "")}
    else {
      paste("/",name,sep="")
    }  
  }
  
  
  totavg <- numeric(0)
  for( i in seq_along(id)){
    name <- resolvename(id[i])
    fullName <- paste(directory,name,".csv",sep = "")
    content <- read.csv(fullName)
    totavg <- c(totavg,content[,pollutant])
  }
  
  mean(totavg,na.rm = TRUE)
}
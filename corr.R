corr <- function(directory, threshold=0)
{
  numberOfCompleteCases <- complete(directory)
  numberOfCompleteCasesOverThreshold <- numberOfCompleteCases[numberOfCompleteCases$nobs>threshold,]
  
  id <- numberOfCompleteCasesOverThreshold[1]
  corVar <- 0
  i <- 1
  rowCount <- nrow(id)
  while(i<=332)
  {
    fileName = ""
    if (i > 99)
      fileName = paste(i,".csv",sep="")
    else if (i > 9)
      fileName = paste("0",paste(i,".csv",sep=""),sep="")
    else
      fileName = paste("00",paste(i,".csv",sep=""),sep="")
    fileNameWithSlash <- paste("/",fileName,sep="")
    filePath = paste(directory,fileNameWithSlash,sep="")
    pollutantdata = read.csv(filePath)
    
    subpollutantdata <- pollutantdata[c(2,3)]
    good <- sum(complete.cases(subpollutantdata))
    
    print(good)
    
    i <- i+1
  }
  
  corVar
}
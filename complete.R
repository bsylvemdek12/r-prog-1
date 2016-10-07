complete <- function(directory, id=1:332)
{
  dataMatrix <- matrix(nrow=length(id),ncol=2)
  rowIndex = 1
  
  for(i in id)
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
    good <- complete.cases(subpollutantdata)
    completes<-subpollutantdata[good,]
    completeRows <- nrow(completes)
    
    dataMatrix[rowIndex,1] <- i
    dataMatrix[rowIndex,2] <- completeRows
    pollutantData = read.csv(filePath)
    rowIndex <- rowIndex + 1
  }
  
  dataFrame <- as.data.frame(dataMatrix)
  colnames(dataFrame) <- c("id","nobs")
  
  dataFrame
}
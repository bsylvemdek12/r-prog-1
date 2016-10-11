corr <- function(directory, threshold=0)
{
  rowIndex = 1
  completeCaseCount <- 0
  id <- 1:332
  corResults <- vector(mode="numeric",length=0)
  dataTable <- 0
  
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
    threshholdCount <- sum(complete.cases(subpollutantdata))
    #completes<-subpollutantdata[good,]
    
    if (threshholdCount > threshold) 
    {
      good <- complete.cases(subpollutantdata)
      dataTable<-subpollutantdata[good,]
      
      if (completeCaseCount == 0)
        corResults <- as.numeric(cor(dataTable[1],dataTable[2]))
      else
        corResults <- c(corResults,as.numeric(cor(dataTable[1],dataTable[2])))
      
      #print(cor(dataTable[1],dataTable[2]))
      
      completeCaseCount <- completeCaseCount + 1
    }
  }
  
  corResults
}
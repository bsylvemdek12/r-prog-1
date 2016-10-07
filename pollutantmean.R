pollutantmean <- function(directory, pollutant, id=1:332)
{
  index <- 0
  sumVals <-0
  numVals <-0
  
  if (pollutant == "sulfate")
  {
    index <- 2
  }
  else
  {
    index <- 3
  }
  
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
    pollutantData = read.csv(filePath)
    observations = pollutantData[index]
    observationsNoNull <- observations[!is.na(observations)]
    sumVals <- sumVals+sum(observationsNoNull)
    numVals <- numVals+length(observationsNoNull)
  }
  
  sumVals / numVals
}
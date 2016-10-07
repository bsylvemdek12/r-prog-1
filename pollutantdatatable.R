pollutantdatatable <- function(directory, id=1)
{
  fileName = ""
  if (id > 99)
    fileName = paste(id,".csv",sep="")
  else if (id > 9)
    fileName = paste("0",paste(id,".csv",sep=""),sep="")
  else
    fileName = paste("00",paste(id,".csv",sep=""),sep="")
  fileNameWithSlash <- paste("/",fileName,sep="")
  filePath = paste(directory,fileNameWithSlash,sep="")
  pollutantData = read.csv(filePath)
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

makefilename<-function(dir="",fileid){
if (fileid>=100) {
  filename<-paste(dir,"/",fileid,".csv",sep="")}
else{
  if ((fileid<=99)&&(fileid>=10)){
    filename<-paste(dir,"/","0",fileid,".csv",sep="")
  }
  else filename<-paste(dir,"/","00",fileid,".csv",sep="")
}
filename
} 

accumulatedmeans<-numeric()

  for (i in id)
{  
    file<-makefilename(directory,i)
    data<-read.csv(file)
    accumulatedmeans<-c(accumulatedmeans,data[[pollutant]])
    
  }
mean(accumulatedmeans,na.rm=TRUE)
}


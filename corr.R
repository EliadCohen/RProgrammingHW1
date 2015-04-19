corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
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
  source("complete.R")

  correlation<-numeric()
  files <- list.files(path=directory, pattern="*.csv")
  completes<-complete(directory)
  for (i in 1:nrow(completes)){
    if (completes$nobs[i]>threshold){
      file<-makefilename(directory,i)
      data1<-read.csv(file)
      filter<-complete.cases(data1)
      data<-data1[filter,]
      v1<-data$sulfate
      v2<-data$nitrate
      correlation<-c(correlation,cor(v1,v2))
      
    }
  }
  correlation
  
}
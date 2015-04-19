complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
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
  
  nobs<-numeric()
  for (i in id)
  {  
    file<-makefilename(directory,i)
    data<-read.csv(file)
    comp<-complete.cases(data)
    nobs<-c(nobs,nrow(data[comp,]))
    
    
  }
  data.frame(id,nobs)
  
}
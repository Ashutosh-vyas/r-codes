complete <- function(directory, id = 1:332)
 { #q<-paste("C:\\Temp\\Personal\\",directory,sep="")
#   setwd(q)
  files <- list.files(pattern = ".csv") ## creates a vector with all file names in your folder
  polmean<-0
  nob <- rep(0,length(id))
  for(i in id){
    if(i == id[1]){
    data <- read.csv(files[i],header=T)
    nob[i]<-sum(complete.cases(data))
    completedata<-cbind(id=i,nobs=nob[i])
    }else{
      data <- read.csv(files[i],header=T)
      nob[i]<-sum(complete.cases(data))
      myrow<-cbind(i,nob[i])
      completedata<-rbind(completedata,myrow)}
  }
x<-data.frame(completedata)
x
}

corr <- function(directory, threshold = 0) {
    com<-complete(directory)
   x<-vector("numeric")
 files <- list.files(pattern = ".csv")
    for(i in 1:nrow(com)){
      fileId<-com$id[i]
      obj <- com$nobs[i]
        if(obj>threshold){
          data <- read.csv(files[fileId],header=T)
          b=cor(data$nitrate,data$sulfate,use="complete.obs")
          v=b
          x<-c(x,v)
        }
    }
 x
}

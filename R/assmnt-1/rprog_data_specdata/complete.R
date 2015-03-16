# Author : Ashutosh Vyas
# international institute of information technology - bangalore

# 2nd function for the part 2 of assesment 1
# our target is to create a data frame which tells the number of
# complete sets in all the given range of ids

complete <- function(directory,id=1:332)
{
	dirc <- "C:/Users/private/r/r data coursera/assment1/"
	setwd(paste(dirc,directory,sep="")) # setting the directory 
	total <- length(id)

	mtrx <- matrix(0,ncol = 2,nrow = total,dimnames=list(1:total,c("id","nobs")))
	cntr <- 1


	while(cntr <= total)
	{
		i <- id[cntr]
		if(i > 332)
		{
			print("file do not exist , check the file id again")
		}
		else if(i >= 100) # condition to set id as name
		{
			data <- data.matrix(read.csv(paste(c(as.character(i),".csv"),collapse="",sep="")))
		}
		else if(i < 10)
		{
			data <- data.matrix(read.csv(paste(c("0","0",as.character(i),".csv"),collapse="",sep="")))
		}
		else 
		{
			data <- data.matrix(read.csv(paste(c("0",as.character(i),".csv"),collapse="",sep="")))		
		}
		
		j <- 1
		cnt <- 0
		nrw <- length(data[,1])
		while(j <= nrw)
		{
			if((is.na(data[j,2])==FALSE) && (is.na(data[j,3])==FALSE))
			{
				cnt <- cnt + 1 
			}
			j <- j + 1
		}
		mtrx[cntr,1] <- i 
		mtrx[cntr,2] <- cnt
		cntr <- cntr + 1
	}		
	frame_out <- as.data.frame(mtrx)
	print(frame_out)
}


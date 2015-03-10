# Author : Ashutosh Vyas
# international institute of information technology - bangalore

# 2nd function for the part 2 of assesment 1
# our target is to create a data frame which tells the number of
# complete sets in all the given range of ids

complete <- function(directory,id)
{
	setwd(directory)
	x <- id[1]
	y <- id[length(id)]
	mtrx <- matrix(0,ncol = 2,nrow = (x*y), dimnames = list(1:(y - x + 1),c("id","nobs")))
	i <- 1
	while(i <= (y - x + 1))
	{
		if(i > 332)
		{
			print("file do not exist , check the file id again")
		}
		else if(i >= 100) # condition to set id as name
		{
			data <- data.matrix(read.csv(paste(c(as.character(i),".csv"),collapse="")))
		}
		else if(i < 10)
		{
			data <- data.matrix(read.csv(paste(c("0","0",as.character(i),".csv"),collapse="")))
		}
		else 
		{
			data <- data.matrix(read.csv(paste(c("0",as.character(i),".csv"),collapse="")))		
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
		mtrx[i,1] <- i 
		mtrx[i,2] <- cnt
		i <- i + 1
	}		
	frame_out <- as.data.frame(mtrx)
	print(frame_out)
}


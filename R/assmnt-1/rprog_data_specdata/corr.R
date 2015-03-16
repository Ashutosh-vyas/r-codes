## FUNCTION to generate correlation details of all rhe tables
## involved

corr <- function(directory,threshold=0)
{
	dirc <- "C:/Users/private/r/r data coursera/assment1/"
	setwd(paste(dirc,directory,sep="")) # setting the directory 

	vector <- rep(NA,length(list.files()))
	i <- 1
	while(i<=length(vector))
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
		x <- 0
		y <- 0
		nrw <- length(data[,1])
		temp <- 0
		complete <- 0
		while(j <= nrw)
		{
			if((is.na(data[j,2])==FALSE) && (is.na(data[j,3])==FALSE))
			{
				complete <- complete + 1
				if((data[j,2] > threshold) && (data[j,3] > threshold))
				{
					x <- c(x,data[j,2])
					y <- c(y,data[j,3])
					temp <- temp + 1
				}
			}
			j <- j + 1
		}
		#print(temp)
		x <- x[2:length(x)]
		y <- y[2:length(y)]
		correlation <- cor(x,y)
		
			if(complete > 0 && temp >0)
			{
				vector[i] <- correlation
			}else if(complete > 0 && temp == 0 )
			{
				vector[i] <- 0
			}
		
		i <- i + 1
	}	

	#print(vector)
	i <- 1
	new_vector <- 0
	while(i <= length(vector))
	{
		if(is.na(vector[i])==FALSE && vector[i] != 0)
		{
			new_vector <- c(new_vector,vector[i])
		}
		i <- i + 1
	}	
	new_vector <- new_vector[2:length(new_vector)]

	if(length(new_vector)>0)
	{
		new_vector
	}else
	{
		vector
	}
}	


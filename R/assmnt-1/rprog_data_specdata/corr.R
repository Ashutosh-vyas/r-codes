## FUNCTION to generate correlation details of all rhe tables
## involved

corr <- function(directory,threshold)
{
	setwd(directory)
	vector <- rep(0,length(list.files()))
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
		while(j <= nrw)
		{
			if((is.na(data[j,2])==FALSE) && (is.na(data[j,3])==FALSE))
			{
				x <- c(x,data[j,2])
				y <- c(y,data[j,3])
			}
			j <- j + 1
		}
		x <- x[2:length(x)]
		y <- y[2:length(y)]
		correlation <- cor(x,y)

		if(is.na(correlation)==FALSE)
		{
			if(correlation > threshold)
			{
				vector[i] <- correlation
			}
		}
		i <- i + 1
	}		
	print(vector)
	print(length(vector))
}	


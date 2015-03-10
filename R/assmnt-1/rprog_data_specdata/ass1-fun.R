# Author : Ashutosh Vyas
# internation institute of information technology - bangalore (india)
# 
## function 1 for assignmengt of week one in coursera R programming course

pollutantmean <- function(dirc,pollutant,id)
{
	setwd(dirc) # setting the directory 
	if(id > 200)
	{
		print("file do not exist , check the file id again")
	}
	else if(id >= 100) # condition to set id as name
	{
		data <- data.matrix(read.csv(paste(c(as.character(id),".csv"),collapse="")))
	}
	else if(id < 10)
	{
		data <- data.matrix(read.csv(paste(c("0","0",as.character(id),".csv"),collapse="")))
	}else 
	{
		data <- data.matrix(read.csv(paste(c("0",as.character(id),".csv"),collapse="")))		
	}
	coltab <- colnames(data)
	no_cols <- length(coltab)
	i <- 1
	j <- 0 
	while(i <= no_cols) # finding the colum number of the required pollutant
	{
		if(coltab[i] == pollutant)
		{	
			j <- i 	
		}
		i <- i + 1
	}
	no_row <- length(data[,j])
	temp_vector <- 0 
	i <- 1
	while(i <= no_row) # code to find the mean
	{	
		if(is.na(data[i,j])==FALSE)
		{
			temp_vector <- c(temp_vector,data[i,j])
		}	
		i <- i + 1
	}
	mean_out <- mean(temp_vector[2:length(temp_vector)])
	print(mean_out)	
}

# 2nd function for the part 2 of assesment 1
# our target is to create a data frame which tells the number of
# complete sets in all the given range of ids





# Author : Ashutosh Vyas
# internation institute of information technology - bangalore (india)
# 
## function 1 for assignmengt of week one in coursera R programming course

pollutantmean <- function(directory,pollutant,ids=1:332)
{
	if (missing(ids))
	{
        
	}
	#files_list <- list.files(directory, full.names=TRUE)
	dirc <- "C:/Users/private/r/r data coursera/assment1/"
	setwd(paste(dirc,directory,sep="")) # setting the directory 
	id_crnt <- ids[1]
	id_lst <- ids[length(ids)]
	big_pollutant_arr <- 0
	while(id_crnt <= id_lst)
	{
		id <- id_crnt
 
		if(id > 332)
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
		temp_vector <- temp_vector[2:length(temp_vector)]
		if(is.na(temp_vector)==FALSE)
		{
			big_pollutant_arr <- c(	big_pollutant_arr,temp_vector)
		}
		id_crnt <- id_crnt + 1	
	}
	big_pollutant_arr <- big_pollutant_arr[2:length(big_pollutant_arr)]
	mean_out <- mean(big_pollutant_arr)
	print(round(mean_out,3))	
}





# function for quiz 1 of cleaning data course in coursera

prop_cond <- function(matrix,col,nrows,condition)
{
	i <- 1 
	j <- 0
	while(i<=nrows)
	{
		if(is.na(matrix[i,col])==FALSE)
		{
			if(matrix[i,col] >= condition)
			{
				j <- j + 1
			}		
		}
		i <- i + 1	
	}
	print(j)
}

data <- loadWorkbook("getdata_data_DATA.gov_NGAP.xlsx") 
dat = readWorksheet(data, sheet = "NGAP Sample Data", startRow =18 , endRow = 23, startCol = 7, endCol = 15)

##
restaurant <- function(rootnode,nrows)
{
	i <- 1
	j <- 0
	while(i<=nrows)
	{
		if((xmlSApply(rootnode[[1]][[i]][["zipcode"]],xmlValue))==(xmlSApply(rootnode[[1]][[2]][["zipcode"]],xmlValue)))		{
			j <- j + 1
		}
		i <- i + 1
	}
	print(j)
}



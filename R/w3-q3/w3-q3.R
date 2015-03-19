# we have to use iris data

iris_mean <- function()
{
	library(datasets)
	data(iris)
	i <- 1
	vector <- 0
	while(i <= 150)
	{
		if(iris[i,5]=="virginica"  && is.na(iris[i,1])==FALSE )
		{
			vector <- c(vector,iris[i,1])
		}
		i <- i + 1
	}
	vector <- vector[2:length(vector)]
	print(mean(vector))
}

# functions to perform 
mean_diff <- function()
{
	library(datasets)
	data(mtcars)
	i <- 1
	x <- 0
	y <- 0
	z <- length(mtcars[,1])
	while(i <= z)
	{
		if(mtcars[i,2]==4 && is.na(mtcars[i,5])==FALSE)
		{
			x <- c(x,mtcars[i,5])
		}
		if(mtcars[i,2]==8 && is.na(mtcars[i,5])==FALSE)
		{
			y <- c(y,mtcars[i,5])
		}
		i <- i + 1
	}
	x <- x[2:length(x)]
	y <- y[2:length(y)]
	j <- mean(x)
	q <- mean(y)
	w <- q - j
	r <- abs(w)
	print(r)
}

 
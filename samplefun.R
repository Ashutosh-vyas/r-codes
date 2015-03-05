#1 this function subtract two vectors
subtract <- function(x,y)
{	
	u <- x - y 
	u
}
#2this function add two vectors
add2 <- function(x,y)
{
	v = x + y
	v
}
#3 this function multiply two vectors
multi2 <- function(x , y)
{
	z = x * y
	z
}
#4 this function multiply two matrices
matrx2 <- function(r1 , c1 ,r2 = 2 , c2 = 3)
{
	v <- matrix(2:7,r1,c1)
	w <- matrix(1:6,r2,c2)
	if(c1 == r2)
	{
		u <- v %*% w
		u
	}else
	{
		print("c1 is not equal to r2 so multiplication not possible")
	}
}
#5 this function displays data forms
dataf <- function()
{
	data <- data.frame(foo = 1:6 , joo = 2:7)
	data
}
#6 function to USE FOR LOOP

loop <- function(i)
{
	X <- numeric(i)
	x <- 1:i
	jump <- function(p)
	{
		p <- p + 1
		p
	}	
	for(j in 1:i)
	{
		x[j] <- jump(x[j])
	}	
	x
}
#7 function to use looping in list
list.loop <- function(i)
{
	x <- list(foo = 1:i , zoo = 1:i)
	lapply(x,mean)
}

#8 function to loop in lapply using user defined function
list.loop1 <- function(i)
{
	x <- list(foo = 1:i , zoo = 1:i , too = 3:i)
	jump <- function(p)
	{
		p <- p + 1
	}
	lapply(x,jump)
}
#9 function to use while loop
loop.while <- function(i)
{
	x <- 1:i
	j <- 1
	while(j < i)
	{
		print(x[j])
		j <- j + 1
	}
}

#10 	function to find mean of all coloumns of a matrix
matrc_mean <- function(r,c)
{
	x <- matrix(1:(r*c) , r , c)
	print(x)
	for(i in 1:c)
	{
		y <- mean(x[,i])
		print(y)
	}
}	


min_mean <- function(mu)
{
	x <- c(0.18, -1.54, 0.42, 0.95)
	w <- c(2, 1, 3, 1)
	val <- (w*(x - mu)^2)
	print(sum(val))
}

## q-2
reg_slope<-function(x,y)
{
	beta1 <- cor(x,y)*sd(x)/sd(y)
	print(beta1)
}

## q-3

mtcar_fun <- function(matrix,col1,col2)
{
	x <- matrix[,col1]
	y <- matrix[,col2]
	beta1 <- cor(y,x)*sd(x)/sd(y)
	print(beta1)
}


## q-7

intercept <- function(x,y)
{
	beta1 <- (cor(x,y)*sd(y))/sd(x)
	beta0 <- mean(y) - beta1*mean(x)
	print(beta0)
}

min_mean <- function(mu)
{
	x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
	val <- ((x - mu)^2)
	print(sum(val))
}


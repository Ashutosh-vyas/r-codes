@ when the variance of the expected output depends in the input itself

htdcty <- function()
{
	source("residual.R")
	x <- runif(100,0,6)
	y <- x + rnorm(100,mean=0,sd=(.001*x))
#	plot(x,y)
	abline(lm(y~x))
	residual_plot(x,y)
}




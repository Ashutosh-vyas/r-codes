# now here we will predict the output with  a confidence in 
# the simple linear regression model
# here we will use the standard 'father$son' data ser

confi <- function(confidence)# mention the confidence and the funtin will return the range
{
	library(UsingR)
	data(father.son)
	x <- father.son$sheight
	mean(x) + c(-1,1)*qnorm(confidence,mean=0,sd=1)*(sd(x)/sqrt(length(x)))
}

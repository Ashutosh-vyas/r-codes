# we will find the confidence interval with a small code
# to verify results we use the standard database, WHCH IS GAUSSIANLY DISTRIBUTED
# we use parent$son database , so you need to call it , before using this function
confi <- function(confidence)
{
	x <- father.son$sheight
	mean(x) + c(-1,1)*qnorm(confidence)*sd(x)/sqrt(length(x))	
}

# remeber this confidence nterval given only gives the average interval, i.e
# on average the confidence interval will be in between the given result ,given
# by function


## now remeber that t-distribution comes into picture when there is 
# very less number of samples and we do not have mean and variance of
# the distribution underneath
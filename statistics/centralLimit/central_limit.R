# thisfunction defines the appropriate theory of central limit theorem
# give the input as no of samples you want and observe the efficiency of clt!!!
cl <- function(sample)
{
	# wiki def of cl : In probability theory, the central limit theorem (CLT) 
	#states that, given certain conditions, the arithmetic mean of a
	# sufficiently large number of iterates of independent random variables, 
	#each with a well-defined expected value and well-defined variance, 
	#will be approximately normally distributed, 
	#regardless of the underlying distribution.
	
	# let us take initially the underline distribution to be normal
	x <- seq(1,100,length=1000)
	y <- dnorm(x,mean=50,sd=10)
	
	# now we try to prove the central limit theorem
	# we will take the samples to be 100 each time 
	means <- 0
	i <- 1
	while(i<=1000)
	{
		x_new <- runif(sample,1,100)
		mean_samp <- sum(x_new)/length(x_new)
		means <- c(means,mean_samp)
	i <- i + 1
	}	
	means <- means[2:length(means)]
	sdn <- sqrt(sum((means - mean(means))^2) / (length(means) - 1))

	yS <- dnorm(means,mean=mean(means),sd=sdn)
	par(mfrow=c(1,2))
	plot(x,y,xlab="random variable",ylab="probablity distribution",main="gaussian distribution"
	,type="l",col="red",lwd=2)
	plot(means,yS,col="blue",lwd=2,xlab="means",ylab="distribution",main="no of sample as input")
			
}


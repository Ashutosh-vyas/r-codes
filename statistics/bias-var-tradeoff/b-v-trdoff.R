# the function will explain step-by-step procedure of bias-variance trade-off

standard_error <- function(number_of_samples)
{
	# let us frst plot a bell curve with mean=50 and sd = 10 
	x <- seq(1,100,length=100)
	y <- dnorm(x,mean=50,sd=10)
	plot(x,y,type="l",lwd=3,xlab="random variable",ylab="distribution",main="normal distribution",col="red")

	#sampling from the above curve
	# so we plan to take 1000 such samples each with size = parameter AND plot them
	# we will further increase the size of this samples we take, to demostrate the 
	# bais-variance 
	no_samples <- number_of_samples
	i <- 0
	arr <- 0 
	while(i <= 1000)	
	{
		w <- runif(no_samples,min=1,max=100)
		SAMPLE_var <- ((w-mean(w))*(w-mean(w)))/(no_samples-1)
		
#		abline(v=mean(w),lwd=2,col="blue")
		arr <- c(arr,sqrt(SAMPLE_var))
		i <- i + 1
	}
	arr <- arr[2:length(arr)]
	standard_deviation <- sd(arr)
	# here we will abserve the blue lines covering the region nearby mean
	
	print("the standard deviation is  ")
	print(standard_deviation)	
	print("the original average sd")
	print(10/sqrt(no_samples))		
}



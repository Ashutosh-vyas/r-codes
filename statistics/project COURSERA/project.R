# PROBLEM STATEMENT
# -----------------------------------------------------------------------------
#In this project you will investigate the exponential distribution in R and 
#compare it with the Central Limit Theorem. The exponential distribution can 
# be simulated in R with rexp(n, lambda) where lambda is the rate parameter.
# The mean of exponential distribution is 1/lambda and the standard deviation
# is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate
# the distribution of averages of 40 exponentials. Note that you will need to do
# a thousand simulations.

# Illustrate via simulation and associated explanatory text the properties of the
# distribution of the mean of 40 exponentials.  You should
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical
#    variance of the distribution.
# 3. Show that the distribution is approximately normal.

# In point 3, focus on the difference between the distribution of a large collection
# of random exponentials and the distribution of a large collection of averages of 40
# exponentials.

# As a motivating example, compare the distribution of 1000 random uniforms

# hist(runif(1000))

# and the distribution of 1000 averages of 40 random uniforms

# mns = NULL
# for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
# hist(mns)

# This distribution looks far more Gaussian than the original uniform distribution! 

#-------------------------------------------------------------------------------------
# SOLUTION :
#-------------------------------------------------------------------------------------
# Lets try to analyse the given code and then proceed to problem
test_fun <- function()
{
 par(mfrow=c(1,2))
 hist(runif(1000))
 mns = NULL
 for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
 hist(mns)
}
# in the above code we can easily observe that Central limit thheorem 
# shows it's effect , the first plot shows - that 1000 numbers are randomly selected 
# from uniform distribution in range 0 to 1 and plotted
# and In the second plot
# only 40 random variables are selected from uniform distribution, but for 1000 times
# and the mean were stored in an array , and at last histogram of mean is plotted
# the plot revails gaussian properties, and shows the mean=.5 appears the most ,
# which is actually the theoritical mean of the uniform distribution when min=0 and max= 1
# respectively...

# Now again we have to use an another distribution , whose mean = 1/labmda = standard deviation

exp_clt <- function(n,lambda,simulation)
{	
	i <- 1
	x <- seq(0,100,length=1000)
	y <- dexp(x,rate=lambda)
	mns <- NULL
	while(i <= 1000)
	{
		mns <- c(mns,mean(rexp(n,lambda))) 
		i <- i + 1
	}	
# --------------------- part 1 --------------------------------------------------

	print(paste("differnce in mean :",((1/lambda)-mean(mns))))

# --------------------- part 2 --------------------------------------------------

	print(paste("differnce in standard deviation :",((1/lambda)-sd(mns))))

# --------------------- part 3 --------------------------------------------------

	par(mfrow=c(1,2))
	plot(x,y,type="l",lwd=2,col="red",main="plot of exp distribution:green line is mean")
	abline(v=(1/lambda),col="green")
	hist(mns,main="green line is mean")
	abline(v=mean(mns))
		
}



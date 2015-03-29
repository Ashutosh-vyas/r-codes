# unlike the normal distribution the T-distribution has
# only 1 parameter to get itself specified
# that is --- > degree of freedom
# HERE in this example we will try to generate confidence interval for
# different parameters...
# for sample we will solve a COURSERA week 3 quiz question 1
Tconfi <- function(sample,meen,deviation,confidence)
{
	# formula is
	# mean + ((c(-1,1)*qt(.95,degree of freedom - 1)*s)/sqrt(degree of freedom))
	confi <- meen + c(-1,1)*qt(confidence,df=sample-1)*deviation/sqrt(sample)
	print(confi)
	return(confi)
}

# now new problem is addressed in this following function
# ,again the problem is taken from COURSERA week 3 question -2
# A diet pill is given to 9 subjects over six weeks. The average 
# difference in weight (follow up - baseline) is -2 pounds. What 
# would the standard deviation of the difference in weight have to 
# be for the upper endpoint of the 95% T confidence interval to touch 0?

Tsd_diff <- function(sample,meen,confidence,expect)
{
	result <- ((expect - meen)*(sqrt(sample)))/qt(confidence,sample-1)
	return(result)
}

# now we will  make a diff paired observation using the new problem in COURSERA
# In a study of emergency room waiting times, investigators consider a new and the
# standard triage systems. To test the systems, administrators selected 20 nights and
# randomly assigned the new triage system to be used on 10 nights and the standard system
# on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to
# see a physician. The average MWT for the new system was 3 hours with a variance of 0.60
# while the average MWT for the old system was 5 hours with a variance of 0.68. 
# Consider the 95% confidence interval estimate for the differences of the mean MWT
# associated with the new system. Assume a constant variance. What is the interval?
# Subtract in this order (New System - Old System). 

T_confi_paired <- function(mean1,mean2,sd1,sd2,n1,n2,confidence)
{

quantile <- confidence

n_y <- n1 # nights new system
n_x <- n2 # nights old system

var_y <- sd1 # variance new (sqrt of s)
var_x <- sd2 # variance old (sqrt of s)

µ_y <- mean1 # average hours new system
µ_x <- mean2 # average hours old system

# calculate pooled standard deviation
s_p <- sqrt(((n_x - 1) * var_x + (n_y - 1) * var_y)/(n_x + n_y - 2))

confidenceInterval <- µ_y - µ_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * s_p * (1 / n_x + 1 / n_y)^.5
return(round(confidenceInterval,2))	

} 


quantile = 0.95 # is 90% with 5% on both sides of the range

n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
s_y <- 1.5# kg/m2 std.dev. treated 
s_x <- 1.8# kg/m2 std.dev. placebo 
µ_y <- -3#  kg/m2 average difference treated
µ_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
s_p <- sqrt(((n_x - 1) * s_x^2 + (n_y - 1) * s_y^2)/(n_x + n_y - 2))

confidenceInterval <-  µ_y - µ_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * s_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,3)















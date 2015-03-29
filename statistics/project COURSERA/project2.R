# Now in the second portion of the class, we're going to analyze the ToothGrowth 
# data in the R datasets package. 

# 1   Load the ToothGrowth data and perform some basic exploratory data analyses 
#     Provide a basic summary of the data.
# 2   Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
# 3   State your conclusions and the assumptions needed for your conclusions. 

#------------------------------------------------------------------------------------------------
# 	SOLUTION
#------------------------------------------------------------------------------------------------

toothgrowth_infr <- function()
{
	library(datasets)
	data(ToothGrowth)
	par(mfrow=c(1,2))
 	X <- ToothGrowth[1:30,1]
     y <- ToothGrowth[1:30,3]
     plot(X,y,xlab="LENGTH",ylab="DOSE",col="red",main="with VC")
	abline(v=mean(X))
     X2 <- ToothGrowth[31:60,1]
     y2 <- ToothGrowth[31:60,3]
     plot(X2,y2,xlab="LENGTH",ylab="DOSE",col="red",main="with OJ")
	abline(v=mean(X2),lwd=2,col="blue")
# ----------------------------------------------------------------------------------
	true_mean <- mean(ToothGrowth[,1])
	print("range to accept the hypothesis of vc")
	print(mean(X)+ c(-1,1)*qt(.975,df=29)*sd(X)/sqrt(30))
	print(paste("true mean  ",true_mean))	
# -----------------------------------------------------------------------------------
	print("range to accept the hypothesis of vc")
	print(mean(X2)+ c(-1,1)*qt(.975,df=29)*sd(X2)/sqrt(30))
	print(paste("true mean  ",true_mean))	

# i conclude that the hypothesis of VC will be acceepted.
}
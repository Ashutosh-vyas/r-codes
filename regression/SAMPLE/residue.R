# residual plot

residual_plot <- function(x,y)
{
#	y <- diamond$price
#	x <- diamond$carat
	n <- length(y)
	fit <- lm(y~x)
	err <- resid(fit)
	print(err)
	yhat <- predict(fit)
	print("max residue")
	print(max(abs(err-(y-yhat))))
	plot(x,err,xlab="x",ylab="residue") #A density plot
	abline(0,0)
}


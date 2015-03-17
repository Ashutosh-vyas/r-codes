# lm to fit diamond data
 
diamond_lm <- function()
{
	plot(diamond$carat,diamond$price,xlab="Mass(carets)",ylab="Price in $",bg="lightblue",col="black",pch=21,frame=FALSE)
	abline(v<-lm(price~carat,data=diamond),lwd=2,col="red")
	coef(v)

	#lets prdic the output for some new values 
	v <- lm(diamond$carat~diamond$price,data=diamond)
	newx <- c(.38,.45,.55)
	predict(v,new_data=data.frame(carat<-newx))
}


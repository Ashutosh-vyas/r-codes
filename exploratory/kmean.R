# this function is made to illustrate and use the k-mean algorithm 
# of clustering \. The working of k-mean is as followa
# 1 it selects initially at random , centroids for each clusters
# 2 it assigns all points to their respective centers based upon the distance b/w them
# 3 it then recalculate the centroids for all points and based upon the initialization
# 4 again it goes to step 2 and repeat till the operarion become stable

kmn <- function()
{
	par(mfrow=c(1,2))
	x <- rnorm(12,mean=rep(1:3,each=4),sd=.2)
	y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=.2)
	plot(x,y,col="blue",pch=19,cex=2)
	text(x+0.05,y+0.05,labels=as.character(1:12))
	dataf <- data.frame(x,y)
	km <- kmeans(dataf,centers=3)# kmean itself is not capable of deciding the number of clusters
	names(km)
	km$cluster
	plot(km$centers)
}
# author- Ashutosh-vyas

# this is sample function which describes the process of hirarchical clustering
# initially we will generate 12 numbers in x-y plane and then try to find the number of clusters
# here we use the function 'hclust' to analyze the number of clusters
# hirarchical clustering is based upon the distance between two points, and like this it grows
# from a single point to a single large point(which is the hypothetical point of combination of all points)
# so based upon the distance of each point to another, clusters are formed.
# this phenomena results in the formation of a tree like structure, which we observe as plot in
# this function , with Y axis as the distance, thus observationally we can decide the number
# of clusters...

# ## initially in hirarchical clustering each point is considered as a seperate cluster.


hclus <- function()
{
	par(mfrow=c(1,2))
	x <- rnorm(12,mean=rep(1:3,each=4),sd=.2)
	y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=.2)
	plot(x,y,col="blue",pch=19,cex=2)
	text(x+0.05,y+0.05,labels=as.character(1:12))
	dataf <- data.frame(x=x,y=y)
	distf <- dist(dataf)
	hcluster <- hclust(distf)
	plot(hcluster)
	abline(h=1)	# this line cut tree's three branches at a distance of 1, i.e 3 clusters can be considered at this location
}
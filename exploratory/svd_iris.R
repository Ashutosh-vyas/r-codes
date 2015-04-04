# Singular Value Decomposition 
# the method is used to reduce the dimension of the given dataset
# thus helping the oanalysis
# the fundamental values returned by SVD is -> vectors which will extract 
# the maximum variance direction , which will be equivalent to maximum information 
# spread

# we will experiment on the iris dataset
# output will be values in the U D V , diagonal values
# observe the values of the digonals.
# vector 1 has largest variance the it will extract max.info

svd_iris <- function()
{
	a <- iris[,c(1:4)]
	a <- as.matrix(a)
	at <- t(a)
	at <- as.matrix(at)
	corr_matt <- at %*% a
	out <- svd(corr_matt)
	print("compare the diagonal values to compare, and the value specifie the amount of variance")
	print(out$d[1])
	print(out$d[4])	
}
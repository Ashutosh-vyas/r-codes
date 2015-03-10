library(UsingR)
library(manipulate)
mylist <- function(mu)
{
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu,mu),c(0,150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63,150,paste("mu = ",mu))
  text(63,140,paste("MSE = ",mse))
}
manipulate(mylist(mu),mu = slider(62,74,step=0.5))

plot(galton$child,galton$parent,pch=19,col="blue")
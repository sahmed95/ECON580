set.seed(2)
n <- 500  # sample size 

# Exponential random variable with parameter lambda = 2 
lambda <-2
data <- rexp(n, lambda)

# Theoretical CDF 

probs <- seq(from=0.01,to=0.99,by=0.01)
xvals <- qexp(probs, lambda)

plot.ecdf(data,xlim=c(min(xvals),max(xvals)), main ="CDF for Exponential Distribution with lambda = 2", pch=20)
points(xvals,probs,col="red",type="l")



